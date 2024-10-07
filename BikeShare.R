library(tidyverse)
library(tidymodels)
library(patchwork)
library(vroom)

data <- vroom('train.csv')
testData <- vroom('test.csv')
glimpse(data)

data <- data %>%
  mutate(hour = hour(datetime),
         day = day(datetime),
         month = month(datetime),
         year = year(datetime),
         weekday = wday(datetime, label = TRUE))

p1 <- ggplot(data, aes(x = hour, y = count)) +
  geom_line() +
  labs(title = "Rental Count by Hour")

p2 <- ggplot(data, aes(x = factor(weather), y = count)) +
  geom_bar(stat="identity") +
  labs(title = "Rental Count by Weather")

p3 <- ggplot(data, aes(x = factor(season), y = count)) +
  geom_bar(stat="identity") +
  labs(title = "Rental Count by Season")

p4 <- ggplot(data, aes(x = weekday, y = count)) +
  geom_bar(stat="identity") +
  labs(title = "Rental Count by Weekday")

combined_plot <- p1 + p2 + p3 + p4 + plot_layout(ncol = 2)
combined_plot

library(tidymodels)

## Setup and Fit the Linear Regression Model
my_linear_model <- linear_reg() %>% # Type of model
  set_engine("lm") %>% # Engine = What R function to use
  set_mode("regression") %>% # Regression just means quantitative response
  fit(formula = log(count) ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed, data = data)

bike_predictions <- exp(predict(my_linear_model, new_data = testData) )




library(vroom)

kaggle_submission <- bike_predictions %>%
  bind_cols(., testData) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = pmax(0, count)) %>%
  mutate(datetime = as.character(format(datetime)))

vroom_write(x = kaggle_submission, file = "./LinearPreds.csv", delim = ",")


library(poissonreg)

my_pois_model <- poisson_reg() %>%
  set_engine("glm") %>%
  set_mode("regression") %>%
  fit(formula = count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed, data = data)

bike_predictions2 <- predict(my_pois_model, new_data = testData)
bike_predictions2


pois_kaggle_submission <- bike_predictions2 %>%
  bind_cols(., testData) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(datetime = as.character(format(datetime)))

vroom_write(x = pois_kaggle_submission, file = "./PoissonPreds.csv", delim = ",")

library(tidyverse)
library(vroom)

# Load the data
train_data <- vroom('train.csv')
test_data <- vroom('test.csv')


train_data <- train_data %>%
  select(-casual, -registered) %>% 
  mutate(count = log(count))  

train_data <- train_data %>%
  mutate(datetime_numeric = as.numeric(datetime - min(datetime)))

glimpse(train_data)


library(tidymodels)


bike_recipe <- recipe(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + datetime, data = train_data) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%  
  step_mutate(weather = factor(weather)) %>%  
  step_mutate(hour = hour(datetime), month = month(datetime), year = year(datetime)) %>%  
  step_mutate(season = factor(season)) %>%  
  step_rm(datetime) %>%  
  step_corr(all_numeric_predictors(), threshold = 0.5) %>%
  step_dummy(all_nominal_predictors()) %>%  
  step_normalize(all_numeric_predictors()) 


lin_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")


bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model) %>%
  fit(data = train_data)


lin_preds <- predict(bike_workflow, new_data = test_data) %>%
  mutate(.pred = exp(.pred))  


kaggle_submission <- lin_preds %>%
  bind_cols(., test_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = pmax(0, count)) %>%  
  mutate(datetime = as.numeric(format(datetime)))  


vroom_write(x = kaggle_submission, file = "./linearpreds2.csv", delim = ",")

preg_model <- linear_reg(penalty = 0.0000000001, mixture = 1) %>% 
  set_engine("glmnet") 


preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model) %>%
  fit(data=train_data)

predict(preg_wf, new_data=test_data)


kaggle_submission <- penalized_preds %>%
  bind_cols(test_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = pmax(0, count)) %>%
  mutate(datetime = as.character(format(datetime)))


vroom_write(kaggle_submission, file = "./penalized_preds.csv", delim = ",")

# Load necessary libraries
library(tidymodels)
library(poissonreg)  # If you want to use penalized Poisson regression

# Define penalized regression model with tuning for penalty and mixture
preg_model <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

# Set up the workflow
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%  # 'myRecipe' should be the recipe you're using
  add_model(preg_model)

# Create a grid of values for tuning 'penalty' and 'mixture'
grid_of_tuning_params <- grid_regular(
  penalty(), 
  mixture(), 
  levels = 5  # This controls how many combinations to try
)

# Split the data into folds for cross-validation
folds <- vfold_cv(train_data, v = 5)  # 5-fold cross-validation

# Run the cross-validation tuning process
CV_results <- preg_wf %>%
  tune_grid(
    resamples = folds, 
    grid = grid_of_tuning_params,
    metrics = metric_set(rmse, mae, rsq)  # You can use other metrics as well
  )

# Collect and visualize the tuning results (optional)
collect_metrics(CV_results) %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(x = penalty, y = mean, color = factor(mixture))) +
  geom_line() +
  labs(title = "RMSE by Penalty and Mixture", x = "Penalty", y = "RMSE")

# Find the best tuning parameters based on RMSE
bestTune <- CV_results %>%
  select_best(metric = "rmse")

# Print the best penalty and mixture values
print(bestTune)

final_wf <-2
preg_wf %>%3
finalize_workflow(bestTune) %>%4
fit(data=trainData)5
6
## Predict7
final_wf %>%8
predict(new_data = testData)

# Regression Trees #

library(rpart)
library(tidymodels)


my_mod <- decision_tree(
  tree_depth = tune(),
  cost_complexity = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("regression")

tree_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(my_mod)

grid_of_tuning_values <- grid_regular(
  tree_depth(),
  cost_complexity(),
  min_n(),
  levels = 5
)

set.seed(123)
folds <- vfold_cv(train_data, v = 5)

CV_results_tree <- tune_grid(
  tree_wf,
  resamples = folds,
  grid = grid_of_tuning_values,
  metrics = metric_set(rmse, mae, rsq)
)

bestTune_tree <- CV_results_tree %>%
  select_best(metric = "rmse")

final_tree_wf <- tree_wf %>%
  finalize_workflow(bestTune_tree) %>%
  fit(data = train_data)

tree_preds <- final_tree_wf %>%
  predict(new_data = test_data)

kaggle_submission_tree <- tree_preds %>%
  bind_cols(test_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = pmax(0, count)) %>%
  mutate(datetime = as.character(format(datetime)))

vroom_write(kaggle_submission_tree, file = "./tree_preds.csv", delim = ",")

# Stacking Model

library(stacks)
library(tidymodels)
library(vroom)

folds <- vfold_cv(train_data, v = 5, repeats = 1)

untunedModel <- control_stack_grid()
tunedModel <- control_stack_resamples()

preg_model <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model)

preg_tuning_grid <- grid_regular(penalty(), mixture(), levels = 5)

preg_models <- preg_wf %>%
  tune_grid(resamples = folds,
            grid = preg_tuning_grid,
            metrics = metric_set(rmse, mae, rsq),
            control = untunedModel)

lin_reg <- linear_reg() %>%
  set_engine("lm")

lin_reg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_reg)

lin_reg_model <- fit_resamples(lin_reg_wf, resamples = folds, metrics = metric_set(rmse, mae, rsq), control = tunedModel)

tree_model <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")

tree_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(tree_model)

tree_model_fit <- fit_resamples(tree_wf, resamples = folds, metrics = metric_set(rmse, mae, rsq), control = tunedModel)

my_stack <- stacks() %>%
  add_candidates(preg_models) %>%
  add_candidates(lin_reg_model) %>%
  add_candidates(tree_model_fit)

stack_mod <- my_stack %>%
  blend_predictions() %>%
  fit_members()

test_predictions <- stack_mod %>%
  predict(new_data = test_data)

kaggle_submission <- test_predictions %>%
  bind_cols(test_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = pmax(0, count)) %>%
  mutate(datetime = as.character(format(datetime)))

vroom_write(kaggle_submission, file = "./StackedModelPreds.csv", delim = ",")

# CHOOSE MY OWN MODEL
library(stacks)
library(tidymodels)
library(vroom)

L <- 5

folds <- vfold_cv(train_data, v = K, repeats = 1)

untunedModel <- control_stack_grid()
tunedModel <- control_stack_resamples()

preg_model <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model)

preg_tuning_grid <- grid_regular(penalty(), mixture(), levels = L)

preg_models <- preg_wf %>%
  tune_grid(resamples = folds, grid = preg_tuning_grid, 
            metrics = metric_set(rmse, mae, rsq), control = untunedModel)

rf_model <- rand_forest(trees = tune(), mtry = tune()) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(rf_model)

rf_tuning_grid <- grid_regular(trees(), mtry(range = c(1, 10)), levels = L)

rf_models <- rf_wf %>%
  tune_grid(resamples = folds, grid = rf_tuning_grid,
            metrics = metric_set(rmse, mae, rsq), control = tunedModel)

svm_model <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

svm_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(svm_model)

svm_tuning_grid <- grid_regular(cost(), rbf_sigma(), levels = L)

svm_models <- svm_wf %>%
  tune_grid(resamples = folds, grid = svm_tuning_grid,
            metrics = metric_set(rmse, mae, rsq), control = tunedModel)

my_stack <- stacks() %>%
  add_candidates(preg_models) %>%
  add_candidates(rf_models) %>%
  add_candidates(svm_models)

stack_mod <- my_stack %>%
  blend_predictions() %>%
  fit_members()
