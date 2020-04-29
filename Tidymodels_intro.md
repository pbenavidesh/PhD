Tidymodels intro
================
Pablo Benavides-Herrera
2020-04-29

  - [Build a model](#build-a-model)
      - [Import, clean and visualize the
        data](#import-clean-and-visualize-the-data)
      - [Build and fit a model: linear
        model](#build-and-fit-a-model-linear-model)
      - [Use a model to predict](#use-a-model-to-predict)
      - [Model with a different engine: Bayesian
        Analysis](#model-with-a-different-engine-bayesian-analysis)

*Based on <https://www.tidymodels.org/start/>.*

# Build a model

The specific packages to be used here are `broom` and `parsnip`,
contained in `tidymodels`.

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.0     v purrr   0.3.4
    ## v tibble  3.0.1     v dplyr   0.8.5
    ## v tidyr   1.0.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ----------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(tidymodels)
```

    ## -- Attaching packages ------------------------------- tidymodels 0.1.0 --

    ## v broom     0.5.6      v rsample   0.0.6 
    ## v dials     0.0.6      v tune      0.1.0 
    ## v infer     0.5.1      v workflows 0.1.1 
    ## v parsnip   0.1.0      v yardstick 0.0.6 
    ## v recipes   0.1.10

    ## -- Conflicts ---------------------------------- tidymodels_conflicts() --
    ## x scales::discard() masks purrr::discard()
    ## x dplyr::filter()   masks stats::filter()
    ## x recipes::fixed()  masks stringr::fixed()
    ## x dplyr::lag()      masks stats::lag()
    ## x dials::margin()   masks ggplot2::margin()
    ## x yardstick::spec() masks readr::spec()
    ## x recipes::step()   masks stats::step()

## Import, clean and visualize the data

We first start reviewing the sea urchins data, loading it with
`read_csv` from :

``` r
urchins <-
  read_csv("https://tidymodels.org/start/models/urchins.csv") %>% 
  # rename
  setNames(c("food_regime", "initial_volume", "width")) %>% 
  # change to factor
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))
```

    ## Parsed with column specification:
    ## cols(
    ##   TREAT = col_character(),
    ##   IV = col_double(),
    ##   SUTW = col_double()
    ## )

``` r
urchins
```

    ## # A tibble: 72 x 3
    ##    food_regime initial_volume width
    ##    <fct>                <dbl> <dbl>
    ##  1 Initial                3.5 0.01 
    ##  2 Initial                5   0.02 
    ##  3 Initial                8   0.061
    ##  4 Initial               10   0.051
    ##  5 Initial               13   0.041
    ##  6 Initial               13   0.061
    ##  7 Initial               15   0.041
    ##  8 Initial               15   0.071
    ##  9 Initial               16   0.092
    ## 10 Initial               17   0.051
    ## # ... with 62 more rows

This [tibble](https://tibble.tidyverse.org/index.html) contains three
variables or features regarding sea urchins used in an experiment by
[Constable(1993)](https://link.springer.com/article/10.1007/BF00349318):

  - experimental feeding regime group (`food_regime`: one from
    `Initial`, `Low`, or `High`),

  - size (mm) at the start of the experiment (`initial_volume`), and

  - suture width at the end of the experiment (`width`).

One of the very first steps when modelling data is plotting it:

``` r
urchins %>% 
ggplot(aes(x = initial_volume, y = width, 
           group = food_regime, col = food_regime)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  scale_color_viridis_d(option = "plasma", end = .7)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Tidymodels_intro_files/figure-gfm/sea%20urchins%20plot-1.jpeg)<!-- -->

## Build and fit a model: linear model

We will first do a standard two-way analysis of variance model (ANOVA).
We build a model that allows for two-way interactions in **R**, since
the slopes appear to be different between food regimes.

    width ~ initial volume * food_regime

We take an ordinary least squares (OLS) approach. How `tidymodels` work
is as follows:

1.  Specify the functional form of the model using the [`parsnip`
    package](https://tidymodels.github.io/parsnip/).
      - In this case, we use a linear regression, declaring it with
        `linear_reg()`.
2.  Set the engine with `set_engine()`. This includes the software that
    can be used to train the model and the estimation method.
      - We use OLS, therefore we set the engine to `lm`.

<!-- end list -->

``` r
lm_mod <- linear_reg() %>% 
  set_engine("lm")
lm_mod
```

    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm

We then train the model using the function `fit()` and the specified
data and equation.

``` r
lm_fit <- 
  lm_mod %>% 
  fit(width ~ initial_volume * food_regime, data = urchins)
lm_fit
```

    ## parsnip model object
    ## 
    ## Fit time:  11ms 
    ## 
    ## Call:
    ## stats::lm(formula = formula, data = data)
    ## 
    ## Coefficients:
    ##                    (Intercept)                  initial_volume  
    ##                      0.0331216                       0.0015546  
    ##                 food_regimeLow                 food_regimeHigh  
    ##                      0.0197824                       0.0214111  
    ##  initial_volume:food_regimeLow  initial_volume:food_regimeHigh  
    ##                     -0.0012594                       0.0005254

We use `tidy()` in order to retrieve the description of the parameter
estimates in a tidy way:

``` r
tidy(lm_fit)
```

    ## # A tibble: 6 x 5
    ##   term                            estimate std.error statistic  p.value
    ##   <chr>                              <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)                     0.0331    0.00962      3.44  0.00100 
    ## 2 initial_volume                  0.00155   0.000398     3.91  0.000222
    ## 3 food_regimeLow                  0.0198    0.0130       1.52  0.133   
    ## 4 food_regimeHigh                 0.0214    0.0145       1.47  0.145   
    ## 5 initial_volume:food_regimeLow  -0.00126   0.000510    -2.47  0.0162  
    ## 6 initial_volume:food_regimeHigh  0.000525  0.000702     0.748 0.457

## Use a model to predict

Suppose that, for a publication, it would be particularly interesting to
make a plot of the mean body size for urchins that started the
experiment with an initial volume of 20ml.

``` r
new_points <- expand.grid(initial_volume = 20, 
                          food_regime = c("Initial", "Low", "High"))
new_points
```

    ##   initial_volume food_regime
    ## 1             20     Initial
    ## 2             20         Low
    ## 3             20        High

We use `predict()` from tidymodels to get our predicted results, as it
offers the same syntax, independently of the model used.

``` r
mean_pred <- predict(lm_fit, new_data = new_points)
mean_pred
```

    ## # A tibble: 3 x 1
    ##    .pred
    ##    <dbl>
    ## 1 0.0642
    ## 2 0.0588
    ## 3 0.0961

To get the confidence intervals, whe specify `type = "conf_int` whitin
`predict()`.

``` r
conf_int_pred <- predict(lm_fit, 
                         new_data = new_points, 
                         type = "conf_int")
conf_int_pred
```

    ## # A tibble: 3 x 2
    ##   .pred_lower .pred_upper
    ##         <dbl>       <dbl>
    ## 1      0.0555      0.0729
    ## 2      0.0499      0.0678
    ## 3      0.0870      0.105

To plot our results, we first need to combine all these results and the
original data into a single tibble.

``` r
# Combine data
plot_data <- 
  new_points %>% 
  bind_cols(mean_pred) %>% 
  bind_cols(conf_int_pred)

# and plot:
ggplot(plot_data, aes(x = food_regime)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2) + 
  labs(y = "urchin size")
```

![](Tidymodels_intro_files/figure-gfm/lm%20predict%20plot-1.jpeg)<!-- -->

## Model with a different engine: Bayesian Analysis
