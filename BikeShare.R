library(tidyverse)
library(tidymodels)
library(patchwork)
library(vroom)

data <- vroom('train.csv')
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