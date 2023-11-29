#Strategy 1: Do Lasso for years between 2007 and 2022
# Libraries
library(pacman)
p_load(tidyverse, tidymodels, skimr, glmnet, kknn)
library(tidymodels)

# Data
merge_final <- read.csv("Data/Merging/merge_final_population_cleaning_Indira.csv")
# Create an empty dataframe to store results
results_df <- data.frame()
#Looping through years
for (j in seq(2007, 2022)) {
  print(j)
  # Filter data for the current year
  df_2007 <- subset(merge_final, year == j)
  #choosing only necessary variables
  df_2007 <- df_2007 %>%
    select(-X.1, -state, -year, -X, -state_abb, -missing_eth, -missing_corn_prod, -missing_corn_prices, -X.2)
  print(tail(df_2007, 1))
  #Splitting
  # Set a seed (ensures reproducible results)
  set.seed(42)
  # Create an 80/20 split by random sampling
  e85_split = df_2007 |> initial_split(prop = 0.8)
  # Grab each subset
  e85_train = e85_split |> training()
  e85_test  = e85_split |> testing()
  e85_split
  #Recipe itself
  e85_recipe = 
    # Define the recipe: Rating predicted by all other vars in housing_train
    recipe(e85 ~ ., data = e85_train) |>
    # Create log terms for numeric predictors
    step_log(total_stations, population) |>
    # Normalize vars
    step_normalize(all_numeric_predictors()) |>
    # Remove predictors with near-zero variance (improves stability)
    step_zv(all_numeric_predictors()) |>
    # Remove very collinear vars
    step_corr(all_numeric_predictors(), method = "spearman",threshold = 0.9)
  #getting cleaned data
  system.time({
    e85_clean <- e85_recipe |> prep() |> juice()
  })
  #LASSO MODEL
  #defining model
  model_lasso = 
    linear_reg(penalty = tune(), mixture = 1) |> 
    set_engine("glmnet")
  #setting lambdas
  #Values range from 10^-7 because from iterative process it seems that lower alphas perform better
  lambdas = 10 ^ seq(from = -3, to = 3, length = 1e3)
  #creating v-fold cross-validation
  e85_cv = e85_train |> vfold_cv(v = 5)
  #defining workflow
  workflow_lasso = workflow() |>
    add_model(model_lasso) |>
    add_recipe(e85_recipe)
  #tuning for a Lasso regression model
  lasso_cv = workflow_lasso |>
    tune_grid(
      e85_cv,
      grid = data.frame(penalty = lambdas),
      metrics = metric_set(yardstick::rmse)
    )
  lasso_cv |> show_best() #best penalty is 51.8
  final_lasso = 
    workflow_lasso |>
    finalize_workflow(select_best(lasso_cv, metric = "rmse"))
  final_lasso
  #graph
  autoplot(lasso_cv, metric="rmse")
  #looking at coefficients
  final_fit_lasso = final_lasso |> fit(data=e85_train) |> last_fit(split=e85_split)
  
  #getting the coefficients from best models
  coefs = final_fit_lasso |>
    extract_fit_parsnip() |>
    tidy()
  #getting non-zero coefs
  coefs_nonzero = coefs |>
    filter(estimate > 0) |>
    arrange(desc(estimate))
  # Storing results in the dataframe
  result_row <- data.frame(terms = toString(coefs_nonzero$term))
  results_df <- bind_rows(results_df, result_row)
  rm(df_2007)
  rm(result_row)
  rm(e85_split)
  rm(e85_train)
  rm(e85_test)
  rm(e85_cv)
  rm(lasso_cv)
  rm(final_fit_lasso)
  rm(coefs)
  rm(coefs_nonzero)
  rm(e85_recipe)
  rm(e85_clean)
  
}
#creating df
results_df$year=seq(2007, 2022)