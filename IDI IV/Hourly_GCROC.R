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

holidays_tbl <- read_csv("./IDI II/festivos_utf8.csv") %>% 
  mutate(fecha = dmy(fecha))

holidays <- holidays_tbl %>% distinct(festivo) %>% pull()

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
    MWh = `(MWh)`,
    fecha = as_date(date)
  ) %>% 
  drop_na() %>% 
  # joining holidays data
  left_join(holidays_tbl, by = c("fecha" = "fecha")) %>% 
  #cleaning holidays
  mutate(
    festivo = if_else(is.na(festivo), "No",festivo) %>% 
      factor(levels = c("No", holidays))
  )

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

hour_filtered_tbl %>% 
  plot_time_series(date, MWh, .smooth = FALSE)

hour_filtered_tbl %>% 
  plot_time_series(date, box_cox_vec(MWh), .smooth = FALSE)


hour_day_filtered_tbl <- hour_load_tidy_tbl %>% 
  filter(Hour == 19,
         Day == "Mon")

hour_day_filtered_tbl %>% 
  plot_time_series(date, MWh, .smooth = FALSE)

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


# hour day ----------------------------------------------------------------

hour_day_filtered_tbl %>% 
  summarise_by_time(
    date,
    .by = "year",
    n = n()
  )


# * Train/test splits -----------------------------------------------------

# splits <- hour_day_filtered_tbl %>% 
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
  
  step_box_cox(MWh, method = "guerrero") %>% 
  
  step_timeseries_signature(date) %>%

  step_fourier(date, period = c(52), K = 2) %>%

  step_normalize(ends_with("index.num"),
                 ends_with("_year")) %>%

  step_unorder(all_nominal()) %>%

  step_dummy(all_nominal()) %>%

  step_rm(ends_with(".iso"), ends_with(".xts"),
          contains("second"), contains("minute"),
          ends_with("_hour12"), ends_with("hour"),
          ends_with("_half"), ends_with("am.pm"),
          starts_with("Day_"),
          ends_with("week"), contains("date_week"),
          date_quarter, date_month, date_mday7,
          date_day, date_wday, date_mday, date_qday,
          date_yday) %>% 
  # step_interact(terms = ~ contains("52") * contains("month")) %>%
  step_interact(terms = ~ contains("52") * contains("festivo")) %>%
  step_interact(terms = ~ contains("festivo") * contains("wday"))# %>%
  # step_interact(terms = ~ contains("52") * contains("wday")) %>% 
  # step_interact(terms = ~ contains("month") * contains("wday"))
  # step_interact(terms = ~ contains("month") * contains("wday") *
  #               contains("52") * contains("festivo"))

# View the recipe_spec
recipe_spec %>% prep() %>% juice() %>% glimpse()
# Export the data with the features
# recipe_spec %>% prep() %>% juice() %>% 
#   write_csv("consumo_horario_19_feats3.csv")

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

