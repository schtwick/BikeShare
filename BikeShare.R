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

preg_model <- linear_reg(penalty = 0.1, mixture = 0) %>% 
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
