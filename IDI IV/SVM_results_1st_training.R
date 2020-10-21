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

file_names <- list.files(path = "./IDI IV/SVM_hourly/corregido_d730, sw15, mi10/")

y_files <- file_names[str_starts(file_names, "y")]

y <- tibble(filename = y_files) %>% 
  mutate(
    file_contents = map(
      y_files,
      ~ read_csv(
        file = file.path("./IDI IV/SVM_hourly/d730, sw25, mi10/",.),
        trim_ws = TRUE,
        col_names = c("t", "y", "E reg", "E MAPE", "v reg", "v MAPE"),
        skip = 1
      )
    )
  ) %>% 
  unnest(c(file_contents))

y <- y %>% 
  separate(filename, into = c("y1", "type", "kernel", "D"), sep = "_") %>% 
  select(-c(y1, D)) %>% 
  mutate(t = if_else(type == "test", t + 723, t)) %>% 
  pivot_longer(
    cols      = `E reg`:`v MAPE`, 
    names_to  = "Model",
    values_to = "Estimation"
  ) %>% 
  group_by(type, kernel, Model)

y  

y %>% 
  filter(Model %in% c("E MAPE", "v MAPE"))

# * plots -----------------------------------------------------------------

p <- y %>% 
  filter(type == "test") %>% 
  ggplot(aes(x = t)) +
  geom_line(aes(y = y)) +
  geom_line(aes(y = Estimation, color = Model)) +
  facet_wrap(~ kernel, ncol = 1)

ggplotly(p)

# * accuracy --------------------------------------------------------------

y %>% 
  summarise(
    MAPE = mean(abs(1 - Estimation / y)*100),
    RMSE = sqrt(mean((y - Estimation)^2))
  ) %>% view()
  
