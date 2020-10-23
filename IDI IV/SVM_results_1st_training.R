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
# data importing and wrangling
y_fun <- function(.path, starts = "y"){
  file_names <- list.files(path = .path)
  
  y_files <- file_names[str_starts(file_names, starts)]
  
  y <- tibble(filename = y_files) %>% 
    mutate(
      file_contents = map(
        y_files,
        ~ read_csv(
          file = file.path(.path,.),
          trim_ws = TRUE,
          col_names = c("t", "y", "E reg", "E MAPE", "v reg", "v MAPE"),
          skip = 1
        )
      )
    ) %>% 
    unnest(c(file_contents))
  
  max_t <- y %>% pull(t) %>% max() + 1
  
  y <- y %>% 
    separate(filename, into = c("y1", "type", "kernel", "D"), sep = "_") %>% 
    select(-c(y1, D)) %>% 
    mutate(t = if_else(type == "test", t + max_t, t)) %>% 
    pivot_longer(
      cols      = `E reg`:`v MAPE`, 
      names_to  = "Model",
      values_to = "Estimation"
    ) %>% 
    group_by(type, kernel, Model)
}

# plotting results

plots_by_kernel <- function(.data,
                            .type = "all", 
                            .interactive = TRUE,
                            .facet_by_formulation = FALSE) {
  if (.type == "all"){
    y <- .data
  } else {
    y <- .data %>% 
      filter(type == .type)
  }
  
  y <- y %>% 
    mutate(formulation = if_else(str_detect(Model, "MAPE"), "MAPE", "Classic"))
  p <- y %>% 
    ggplot(aes(x = t)) +
    geom_line(aes(y = y)) +
    geom_line(aes(y = Estimation, color = Model)) +
    labs(y = "MWh", x = "Days")
  
  if (.facet_by_formulation == TRUE){
    p <- p + facet_grid(formulation ~ kernel, scales = "free_y")
  } else {
    p <- p + facet_wrap(~ kernel, ncol = 1, scales = "free_y")
  } 
  
  if (.interactive == TRUE){
    ggplotly(p)
  } else {
    p
  }
}

plots_by_formulation <- function(.data,
                                 .type = "all", 
                                 .interactive = TRUE) {
  if (.type == "all"){
    y <- .data
  } else {
    y <- .data %>% 
      filter(type == .type)
  }
  p <- y %>% 
    ggplot(aes(x = t)) +
    geom_line(aes(y = y)) +
    geom_line(aes(y = Estimation, color = kernel)) +
    facet_wrap(~ Model, ncol = 1, scales = "free_y")
  
  if(.interactive == TRUE){
    ggplotly(p)
  } else {
    p
  }
}

# accuracy
accuracy_metrics <- function(.data){
  .data %>% 
    summarise(
      MAPE = mean(abs(1 - Estimation / y)*100),
      RMSE = sqrt(mean((y - Estimation)^2))
    )
    # summarise(across(
    #   .cols = E_reg:v_mape,
    #   .fns  = list(MAPE = ~ mean(abs(.x / y - 1)*100),
    #                RMSE = ~ sqrt(mean((y - .x)^2))
    #   )
    # )) %>% 
    # pivot_longer(cols = everything()) %>% 
    # separate(name, into = c("Model", "Error"), sep= "_(?=[^_]+$)") %>% 
    # pivot_wider(names_from = Error, values_from = value)
}

# DAILY LOAD FORECASTING --------------------------------------------------


# - OLD FORMAT -------------------------------------------------------------
# Forecasts 
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




# - NEW FORMAT -------------------------------------------------------------



# * corrected data = 1st 200, swarm = 30, max iter = 15 ----------------------

y_d200_sw30_mi15 <- y_fun(.path = "./IDI IV/SVM_daily/d1st200, sw30, mi15/")

y_d200_sw30_mi15 %>% 
  #filter(kernel == "linear") %>% 
  #filter(Model != "E reg") %>% 
  plots_by_kernel(.type = "train", .interactive = TRUE)

plots_by_formulation(y_d200_sw30_mi15, .type = "train", .interactive = TRUE)

y_d200_sw30_mi15 %>% 
  filter(Model  == "E reg",
         kernel != "rbf") %>% 
  plots_by_formulation()

y_d200_sw30_mi15 %>% 
  plots_by_kernel(.facet_by_formulation = TRUE)

accuracy_metrics(y_d200_sw30_mi15) %>% 
  filter(type == "test") %>% 
  arrange(MAPE)




# * classic old code, MAPE new code ---------------------------------------

y_before <- y_d200_sw30_mi15 %>% 
  filter(Model %in% c("v MAPE"),
         kernel == "linear")

y_classic_old_mape_new <- y_fun(.path = "./IDI IV/SVM_daily/classic old code, MAPE new/")



# * best simulations ------------------------------------------------------

y_classic_old_mape_new_best <- y_classic_old_mape_new %>% 
  filter(Model != "v MAPE")

y_before %>% 
  accuracy_metrics() %>% 
  #filter(type == "test") %>% 
  arrange(MAPE)

y_classic_old_mape_new_best %>% 
  accuracy_metrics() %>% 
  #filter(type == "test") %>% 
  arrange(MAPE)

y_full <- y_classic_old_mape_new_best %>% 
  bind_rows(y_before)
  
y_full %>% 
  accuracy_metrics() %>% 
  filter(type == "test") %>% 
  arrange(MAPE) %>% 
  write_csv("test_mapes.csv")

y_full %>% 
  plots_by_kernel(.type = "test")

p <- y_full %>% 
  filter(type == "test") %>% 
  ggplot(aes(x = t)) +
  geom_line(aes(y = y)) +
  geom_line(aes(y = Estimation, color = Model)) +
  labs(
    y     = "MWh", 
    x     = "Days", 
    color = "Formulation",
    title = "Forecasts of the Daily Electricity Load"
  )

ggplotly(p)

p <- y_full %>% 
  ggplot(aes(x = t)) +
  geom_line(aes(y = y)) +
  labs(
    y     = "MWh", 
    x     = "Days", 
    title = "Daily Electricity Load"
  )
ggplotly(p)

# HOURLY LOAD FORECASTING -------------------------------------------------

# * h730_sw25_mi10 --------------------------------------------------------

# data = last 730, swarm = 25, max iter = 10

y_h730_sw25_mi10 <- y_fun(.path = "./IDI IV/SVM_hourly/d730, sw25, mi10/")

plots_by_kernel(y_h730_sw25_mi10, .type = "train", .interactive = TRUE)

plots_by_formulation(y_h730_sw25_mi10, .type = "train", .interactive = TRUE)

accuracy_metrics(y_h730_sw25_mi10)





