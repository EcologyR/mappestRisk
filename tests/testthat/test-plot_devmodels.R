library(dplyr)
library(testthat)
suppressWarnings(library(ggplot2))
## first use a data of example
set.seed(2023)
fitted_params_example <- fit_devmodels(temp = seq(4, 40, 3),
                                       dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                                       model_name = "all",
                                       variance_model = "exp")

# Test input data types
test_that("plot_devmodels should throw an error if temperature data is not numeric", {
  expect_error(plot_devmodels(temp = as.factor(seq(4, 40, 3)),
                               dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                               fitted_parameters = fitted_params_example))
})

test_that("plot_devmodels should throw an error if dev_rate data is not numeric", {
  expect_error(plot_devmodels(temp = seq(4, 40, 3),
                              dev_rate = as.character(rnorm(13, mean = 0.02, sd = 0.005)),
                              fitted_parameters = fitted_params_example))
})

test_that("plot_devmodels should throw an error if temperature data is a data.frame", {
  expect_error(plot_devmodels(temp = data.frame(temperature = seq(4, 40, 3),
                                               temp_error = runif(13, 0, 2)),
                             dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                             fitted_parameters = fitted_params_example))
})

test_that("plot_devmodels should throw an error if temperature and development rate inputs are not of same length", {
  expect_error(plot_devmodels(temp = seq(4, 40, 3),
                             dev_rate = rnorm(12, mean = 0.02, sd = 0.005),
                             fitted_parameters = fitted_params_example))
})


test_that("plot_devmodels should throw an error if fitted_parameters is not inherited unmodified from `fit_devmodels()`", {
  expect_error(plot_devmodels(temp = seq(4, 40, 3),
                              dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                              fitted_parameters = fitted_params_example |> select(- model_AIC)))
})

test_that("plot_devmodels should throw an error if fitted_parameters columns are renamed from `fit_devmodels()`", {
  expect_error(plot_devmodels(temp = seq(4, 40, 3),
                              dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                              fitted_parameters = fitted_params_example |>
                                dplyr::rename(aic = model_AIC)))
})

test_that("no error happens when filtering or subsetting only by rows and at least one model is left in the data.frame", {
  expect_no_error(plot_devmodels(temp = seq(4, 40, 3),
                              dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                              fitted_parameters = fitted_params_example |>
                                dplyr::group_by(model_name) |>
                                filter(all(fit == "okay"))))
})

test_that("plot_devmodels should throw an error if fitted_parameters is not inherited from `fit_devmodels()`", {
  expect_error(plot_devmodels(temp = seq(4, 40, 3),
                              dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                              fitted_parameters = data.frame(model_name = "briere1",
                                                             ctmin_est = 8,
                                                             ctmin_se = 0.23,
                                                             model_AIC = -67.32)))
})



# test if output is a ggplot object
test_that("plot_devmodels() outputs a ggplot object",{
  example_plotdevs <- plot_devmodels(temp = seq(4, 40, 3),
                                     dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                                     fitted_parameters = fitted_params_example)
  expect_true(class(example_plotdevs)[2] == "ggplot")
})

# test if ºC are parsed correctly
test_that("temperature units (ºC) are not parsed as `?`", {
  example_plotdevs <- plot_devmodels(temp = seq(4, 40, 3),
                                     dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                                     fitted_parameters = fitted_params_example)
  expect_true(stringr::str_sub(example_plotdevs$labels$x, 14, 15) == "ºC")
})

# test that likely mistaken development data input gives a warning
test_that("if dev_data and predictions are steeply different, a warning is printed", {
  expect_warning(plot_devmodels(temp = seq(4, 40, 3),
                                     dev_rate = rnorm(13, mean = 0.3, sd = 0.005),
                                     fitted_parameters = fitted_params_example))
})

# test that if only one model has converged, the plot works
test_that("a single model converging does also allow a ggplot with one facet", {
  fitted_params_one <- fitted_params_example |>
    dplyr::filter(model_name == "mod_polynomial")
  expect_no_error(plot_devmodels(temp = seq(4, 40, 3),
                                 dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                                 fitted_parameters = fitted_params_one))
})

# test that no model converge returns an error.
test_that("an empty tibble -that should not be returned anyways from fit_devmodels()- returns an error ", {
  fitted_params_null <- fitted_params_example |>
    dplyr::filter(model_name == "briere1") # <- empty tibble
  expect_error(plot_devmodels(temp = seq(4, 40, 3),
                              dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                              fitted_parameters = fitted_params_null))
})

