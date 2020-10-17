# First training results
# SVM-MAPE
# Pablo Benavides-Herrera
# 2020-10-16
# pkgs --------------------------------------------------------------------

library(tidyverse)
# library(readxl)
library(lubridate)
library(timetk)
library(tidymodels)
library(modeltime)
library(plotly)


# DATA --------------------------------------------------------------------

y_train <- read_csv("./IDI IV/Poncho/First training results/y_train.csv") %>% 
  rename(
    y      = `0`,
    E_reg  = `1`,
    mape   = `2`,
    vE     = `3`,
    v_mape = `4`
  )
y_train  

y_test <- read_csv("./IDI IV/Poncho/First training results/y_test.csv") %>% 
  rename(
    y      = `0`,
    E_reg  = `1`,
    mape   = `2`,
    vE     = `3`,
    v_mape = `4`
  ) %>% 
  mutate(X1 = seq(max(y_train$X1)+1, length.out = 7)) %>% 
  pivot_longer(cols = -X1, names_to = "Model")

y_test

g <- y_train %>% 
  ggplot(aes(x = X1, y = y)) + 
  geom_line(size = 1) +
  geom_line(data = y_test %>% filter(Model == "y"), aes(y = value), size = 1) +
  geom_line(data = y_test %>% filter(Model != "y"), aes(y = value, color = Model), size = 1)

ggplotly(g)

