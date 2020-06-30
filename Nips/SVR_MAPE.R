# Support Vector Regression with MAPE-customization
# Pablo Benavides-Herrera
# 2020-06-03


# pkgs --------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)


# data --------------------------------------------------------------------

y_train <- read_csv("Nips/y_train.csv") %>% 
  mutate(date = dmy(date),
         type = "train")

y_test <- read_csv("Nips/y_test.csv") %>% 
  mutate(date = dmy(date),
         type = "test")

y_train <- y_train %>% 
  pivot_longer(cols = `e-classic`:`V-MAPE`,
               names_to = "Models", values_to = "prediction") %>% 
  mutate(MAPE = abs((y - prediction)/y)*100) %>% 
  as_tsibble(index = date, key = Models)

y_test <- y_test %>% 
  pivot_longer(cols = `e-classic`:`V-MAPE`,
               names_to = "Models", values_to = "prediction") %>% 
  mutate(MAPE = abs((y - prediction)/y)*100) %>% 
  as_tsibble(index = date, key = Models)

# train plot
y_train %>% 
  ggplot(aes(x = date, y = y)) +
  geom_line(size = 0.9) + 
  geom_line(aes(y = prediction, color = Models), size = 0.9) +
  facet_wrap(~ Models) +
  theme(legend.position = "none") +
  labs(title = "Energy load modelling with SVR", 
       x = "Date", y = "MWh")

# test plot
y_test %>% 
  ggplot(aes(x = date, y = y)) +
  geom_line(size = 0.9) + 
  geom_line(aes(y = prediction, color = Models), size = 0.9) +
  facet_wrap(~ Models) +
  theme(legend.position = "none") +
  labs(title = "Energy load forecasting with SVR", 
       x = "Date", y = "MWh")

# MAPE per day
y_test %>% 
  select(-prediction) %>% 
  pivot_wider(names_from = Models, values_from = MAPE)
