# Forecasting & Feature Engineering
# Hourly Electricity Load in the WRCM, CENACE.
# PhD in Engineering Sciences, ITESO
# Pablo Benavides-Herrera
# 2020-10-06


# pkgs --------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(timetk)
library(tidymodels)
library(modeltime)

# DATA --------------------------------------------------------------------

# * Import ----------------------------------------------------------------

hour_load_tbl <- read_excel(
  "IDI IV/Consumo horario GCROC 1997-2020.xlsx",
  skip = 1
)


# * Wrangling -------------------------------------------------------------

hour_load_tidy_tbl <- hour_load_tbl %>% 
  transmute(
    # create a datetime variable
    date = tk_make_timeseries(
      start_date = min(hour_load_tbl$fecha, 
                       na.rm = TRUE),
      length_out = length(hour_load_tbl$fecha),
      by         = "hour"
    ),
    # day of week
    Day = wday(x      = date, 
               label  = TRUE, 
               locale = Sys.setlocale("LC_TIME","English")),
    Hour = hour(date),
    MWh = `(MWh)`
  ) %>% 
  drop_na()

hour_load_tidy_tbl

# EDA ---------------------------------------------------------------------


# * Summary ---------------------------------------------------------------


hour_load_tidy_tbl %>% 
  tk_summary_diagnostics()

hour_load_tidy_tbl %>% 
  pull(date) %>% 
  tk_get_timeseries_summary()

# * Visualization ---------------------------------------------------------

# General plot
hour_load_tidy_tbl %>% 
  ggplot(aes(x = date, y = MWh)) +
  geom_line()

# Facets by hour
hour_load_tidy_tbl %>% 
  ggplot(aes(x = date, y = MWh)) +
  geom_line() +
  facet_wrap(~ Hour)



# * Checking missing or wrong values --------------------------------------

# The plots shows hours where the demand is zero. This
# is due to DST shifts.

hour_load_tidy_tbl %>% 
  filter(MWh == 0)

# This effect is always at 1 or 2 a.m.

# Keeping only the hour 19

hour_filtered_tbl <- hour_load_tidy_tbl %>% 
  filter(Hour == 19)

# FEATURE ENGINEERING -----------------------------------------------------


# * Seasonal diagnostics --------------------------------------------------

hour_load_tidy_tbl %>% 
  plot_seasonal_diagnostics(
    .date_var    = date,
    .value       = MWh,
    .feature_set = c("hour", "wday.lbl", "week", "month.lbl")
  )

hour_filtered_tbl %>% 
  plot_seasonal_diagnostics(
    .date_var = date,
    .value    = MWh
  )


# * Train/test splits -----------------------------------------------------

splits <- hour_filtered_tbl %>% 
  time_series_split(
    date_var   = date,
    assess     = "2 months",
    cumulative = TRUE
  )

splits %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(date, MWh)

# * Recipe spec -----------------------------------------------------------

recipe_spec <- training(splits) %>% 
  recipe(MWh ~ .) %>% 
  
  step_timeseries_signature(date) %>% 
  
  step_rm(ends_with(".iso"), ends_with(".xts"),
          contains("second"), contains("minute"),
          ends_with("_hour12"), ends_with("hour"),
          ends_with("_half"), ends_with("am.pm")
          ) %>% 
  
  step_fourier(date, period = c(7, 365), K = 2) %>% 
  
  step_normalize(ends_with("index.num"),
                 ends_with("_year")) %>% 
  
  step_dummy(all_nominal()) %>% 
  step_interact()

# View the recipe_spec
recipe_spec %>% prep() %>% juice() %>% glimpse()


# ML SPECS ----------------------------------------------------------------

model_spec <- linear_reg() %>% 
  set_engine("lm")

workflow_fit_arima <- workflow() %>% 
  add_model(model_spec) %>% 
  add_recipe(recipe_spec) %>% 
  fit(training(splits))

workflow_fit_arima


# * Modeltime process -----------------------------------------------------

calibration_tbl <- modeltime_table(
  workflow_fit_arima
) %>% 
  modeltime_calibrate(testing(splits))

calibration_tbl

calibration_tbl %>% modeltime_accuracy()

calibration_tbl %>% 
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = hour_filtered_tbl
  ) %>% 
  plot_modeltime_forecast()


calibration_tbl %>% 
  modeltime_residuals() %>% 
  plot_modeltime_residuals()
