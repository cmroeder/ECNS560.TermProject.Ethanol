#Strategy 1: do machine learning on 3 years: 2007, 2014 and 2021 to see if laws and ethanol production are good predictors
#libraries
library(pacman)
p_load(tidyverse, tidymodels, skimr, glmnet, kknn)
library(tidymodels)
#data
merge_final=read.csv("Data/Merging/merge_final_population_cleaning_Indira.csv")
df_2007 <- subset(merge_final, year == 2007)

#investigating Y
#cannot use log since I have a lot of 0s
hist(df_2007$e85, xlab = "", col = "pink", border = "black")
#choosing only necessary variables
df_2007 <- df_2007 %>%
  select(-X.1, -state, -year, -X, -state_abb, -missing_eth, -missing_corn_prod, -missing_corn_prices, -X.2)

#Splitting
# Set a seed (ensures reproducible results)
set.seed(42)
# Create an 80/20 split by random sampling
e85_split = df_2007 |> initial_split(prop = 0.8)
# Grab each subset
e85_train = e85_split |> training()
e85_test  = e85_split |> testing()
e85_split

#RECIPE
vars=colnames(df_2007)
par(mfrow = c(6, 2),mar = c(2, 2, 1, 1))

#histograms for each numeric variable
for (var in vars) {
  hist(e85_train[[var]], main = var, xlab = "", col = "skyblue", border = "black")
}

#Reset the layout to the default
par(mfrow = c(1, 1))

#Trying Log
numeric_data = e85_train[, vars]
log_transformed_data <- log1p(numeric_data)
par(mfrow = c(6, 2), mar = c(2, 2, 1, 1))
# Create histograms for each log-transformed numeric variable
for (var in vars) {
  #handle NAs in log
  if (any(!is.na(log_transformed_data[[var]]))) {
    hist(log_transformed_data[[var]], main = paste("Log(", var, ")", sep = ""), xlab = "", col = "lightpink", border = "black", breaks = 20)
  }
}
par(mfrow = c(1, 1))
#Recipe itself
e85_recipe = 
  # Define the recipe: Rating predicted by all other vars in housing_train
  recipe(e85 ~ ., data = e85_train) |>
  # Create log terms for numeric predictors
  step_log(total_stations, population) |>
  # Normalize vars
  step_normalize(all_numeric_predictors()) |>
  # Remove predictors with near-zero variance (improves stability)
  step_nzv(all_numeric_predictors()) |>
  # Remove very collinear vars
  step_corr(all_numeric_predictors(), method = "spearman",threshold = 0.9)
#getting cleaned data
system.time({
  e85_clean <- e85_recipe |> prep() |> juice()
})

#OLS MODEL
#Defining model
model_lm = 
  linear_reg() |>
  set_engine("lm")
#Defining workflow
workflow_lm = workflow() |>
  add_model(model_lm) |>
  add_recipe(e85_recipe)
#Predicting values
ols= workflow_lm |>
  fit(data=e85_train) |>
  last_fit(split=e85_split)
#assessing performance
ols |>
  collect_metrics(yardstick::rmse, yardstick::mae)
#graph
ols_fit <- workflow_lm %>%
  fit(data = e85_train)
predicted_values <- predict(ols_fit, new_data = e85_train)
# Plotting predicted vs actual values
library(ggplot2)
predicted_values$actuals=e85_train$e85
ggplot(data=predicted_values, aes(x = actuals, y = .pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual Values",
       x = "Actual #E85 stations",
       y = "Predicted #E85 stations") +
  theme_minimal()
#Conclusion: OLS is not good

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
result_row <- data.frame(terms = toString(coefs_nonzero$term))
print(result_row)
results_df <- bind_rows(results_df, result_row)
rm(df_2007)

