# SVM-MAPE
# Results
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


# .fns --------------------------------------------------------------------

accuracy_metrics <- function(df){
  df %>% 
    summarise(across(
      .cols = E_reg:v_mape,
      .fns  = list(MAPE = ~ mean(abs(.x / y - 1)*100),
                   RMSE = ~ sqrt(mean((y - .x)^2))
      )
    )) %>% 
    pivot_longer(cols = everything()) %>% 
    separate(name, into = c("Model", "Error"), sep= "_(?=[^_]+$)") %>% 
    pivot_wider(names_from = Error, values_from = value)
}

# DAILY LOAD FORECASTING --------------------------------------------------


# * data ------------------------------------------------------------------

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
  mutate(X1 = seq(max(y_train$X1)+1, length.out = 7))

y_test_long <- y_test %>% 
  pivot_longer(cols = -X1, names_to = "Model")

y_test

# * plots -----------------------------------------------------------------

g <- y_train %>% 
  ggplot(aes(x = X1, y = y)) + 
  geom_line(size = 1) +
  geom_line(data = y_test_long %>% filter(Model == "y"), aes(y = value), size = 1) +
  geom_line(data = y_test_long %>% filter(Model != "y"), aes(y = value, color = Model), size = 1)

ggplotly(g)


# * accuracy --------------------------------------------------------------

accuracy_metrics(y_train)
accuracy_metrics(y_test)


# HOURLY LOAD FORECASTING -------------------------------------------------


# * data ------------------------------------------------------------------


# * plots -----------------------------------------------------------------


# * accuracy --------------------------------------------------------------


