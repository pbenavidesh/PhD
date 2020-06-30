# Support Vector Regression with MAPE-customization
# Pablo Benavides-Herrera
# 2020-06-29
# CCE-congress paper


# pkgs --------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)

# data --------------------------------------------------------------------

x_train <- read_csv("./SVR_MAPE paper/X_train_rbf.csv")

y_train <- read_csv("./SVR_MAPE paper/y_train_rbf.csv")




