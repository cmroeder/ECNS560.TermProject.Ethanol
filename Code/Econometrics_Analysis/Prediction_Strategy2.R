#Strategy 2: Do Lasso for all
# Libraries
library(pacman)
p_load(tidyverse, tidymodels, skimr, glmnet, kknn)
library(tidymodels)
library(openxlsx)

# Data
merge_final <- read.csv("Data/Merging/merge_last.csv")
#splitting data
merge_final_model <- merge_final %>%
  select(-X.1, -X, -state_abb, -missing_eth, -missing_corn_prod, -missing_corn_prices, -X.2, -ratio_e85)
merge_final_model$year <- as.character(merge_final_model$year)
set.seed(42)
e85_split = merge_final_model |> group_initial_split(group=state, prop = 0.8)
e85_train = e85_split |> training()
e85_test  = e85_split |> testing()
e85_split
#recipe
e85_recipe = 
  # Define the recipe: Rating predicted by all other vars in housing_train
  recipe(e85 ~ ., data = e85_train) |>
  update_role(state, new_role = "ID") |>
  # Create log terms for numeric predictors
  step_log(total, population) |>
  # Normalize vars
  step_normalize(all_numeric_predictors()) |>
  #dummy for years
  step_dummy(year) |>
  #interactions
  step_interact(~all_predictors():all_predictors()) |>
  # Remove predictors with near-zero variance (improves stability)
  step_zv(all_numeric_predictors()) |>
  # Remove very collinear vars
  step_corr(all_numeric_predictors(), method = "spearman",threshold = 0.9) 
system.time({
  e85_clean <- e85_recipe |> prep() |> juice()
})
#model
model_lasso = 
  linear_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet")
#setting lambdas
#Values range from 10^-7 because from iterative process it seems that lower alphas perform better
lambdas = 10 ^ seq(from = -3, to = 3, length = 1e3)
e85_cv = e85_train |> group_vfold_cv(group=state, v = 5)
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

lasso_cv |> show_best()
final_lasso = 
  workflow_lasso |>
  finalize_workflow(select_best(lasso_cv, metric = "rmse"))
final_lasso
#graph
autoplot(lasso_cv, metric="rmse")

#on test data
final_fit_lasso = final_lasso |> fit(data=e85_train) |> last_fit(split=e85_split)
metrics= final_fit_lasso |> collect_metrics()
#looking at coefficients
coefs = final_fit_lasso |>
  extract_fit_parsnip() |>
  tidy()
#getting non-zero coefs
coefs_nonzero = coefs |>
  filter(estimate > 0) |>
  arrange(desc(estimate))
#graph
# Predicting values on the test set
lasso_fit <- final_lasso %>%
  fit(data = e85_test)
predicted_values <- predict(lasso_fit, new_data = e85_test)
# Plotting predicted vs actual values
library(ggplot2)
predicted_values$actuals=e85_test$e85
plot1=ggplot(data=predicted_values, aes(x = actuals, y = .pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual Values",
       x = "Actual #E85 stations",
       y = "Predicted #E85 stations") +
  theme_minimal()
ggsave("Outputs/Econometric_Analysis/predicted_actual.png", plot1, width = 8, height = 6, units = "in", dpi = 300)
#saving non-zero coefs
library(openxlsx)
write.xlsx(coefs_nonzero, file = "Outputs/Econometric_Analysis/coefs_nonzero.xlsx", rowNames = FALSE)