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

bike_predictions <- predict(my_linear_model, new_data = testData) 




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




