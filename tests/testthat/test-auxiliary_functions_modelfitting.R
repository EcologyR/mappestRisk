## test for auxiliary  function start_vals_devRate(model_name, temperature, dev_rate)
library(testthat)
library(dplyr)

# input test
test_that("start_vals_devRate should throw an error if temperature data is not numeric", {
  expect_error(start_vals_devRate(temperature = as.character(seq(4, 40, 3)),
                                 dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                                 model_name = sample(available_models |>
                                                        filter(package == "devRate") |>
                                                        pull(model_name),
                                                      1)),
               "temperature data is not numeric; please consider transforming it")
})

test_that("start_vals_devRate should throw an error if temperature data has NAs", {
  expect_error(start_vals_devRate(temperature = c(seq(4, 37, 3), NA),
                                  dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                                  model_name = sample(available_models |>
                                                        filter(package == "devRate") |>
                                                        pull(model_name),
                                                      1)),
               "temperature data have NAs; please consider removing them or fixing them")
})

test_that("start_vals_devRate should throw an error if dev_rate data has NAs", {
  expect_error(start_vals_devRate(temperature = c(seq(4, 40, 3)),
                                  dev_rate = c(rnorm(12, mean = 0.02, sd = 0.005), NA),
                                  model_name = sample(available_models |>
                                                        filter(package == "devRate") |>
                                                        pull(model_name),
                                                      1)),
               "development rate data have NAs; please consider removing them or fixing them")
})


test_that("start_vals_devRate should throw an error if temperature data is a data.frame", {
  expect_error(start_vals_devRate(temperature = data.frame(temperature = seq(4, 40, 3),
                                                temp_error = runif(13, 0, 2)),
                             dev_rate = rnorm(12, mean = 0.02, sd = 0.005),
                             model_name =sample(available_models |>
                                                  filter(package == "devRate") |>
                                                  pull(model_name),
                                                1)),
               "temperature data is not numeric; please consider transforming it")
})

test_that("start_vals_devRate should throw an error if temperature data have just three values", {
  expect_error(start_vals_devRate(temperature = c(15, 20, 25),
                                  dev_rate = rnorm(3, mean = 0.02, sd = 0.005),
                                  model_name = sample(available_models |>
                                                        filter(package == "devRate") |>
                                                        pull(model_name),
                                                      1)),
               "At least four different temperature treatments in the data are required.",
               fixed = TRUE)
})

test_that("start_vals_devRate should throw an error if development rate data is not numeric
          (e.g. incorrectly importing data from csv/xlsx, using commas as decimal markers, etc)", {
            expect_error(start_vals_devRate(temperature = seq(4, 40, 3),
                                            dev_rate = as.character(rnorm(13, mean = 0.02, sd = 0.005)),
                                            model_name = sample(available_models |>
                                                                  filter(package == "devRate") |>
                                                                  pull(model_name),
                                                                1)),
                         "dev_rate data is not numeric; please consider transforming it")
          })

test_that("start_vals_devRate should throw an error if temperature and development rate inputs are not of same length", {
  expect_error(start_vals_devRate(temperature = seq(4, 40, 3),
                             dev_rate = rnorm(12, mean = 0.02, sd = 0.005),
                             model_name =sample(available_models |>
                                                  filter(package == "devRate") |>
                                                  pull(model_name),
                                                1)),
               "development rate and temperature inputs are not of same length. Please check it.")
})

test_that("start_vals_devRate should throw an error if model_name is not in available_models", {
  expect_error(start_vals_devRate(temperature = seq(4, 40, 3),
                                  dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                                  model_name = "SharpeDeMichele"),
               "model not available. For available model names, see `available_models`")
})

test_that("briere1 model gives a warning advising the use of generic starting values", {
  expect_warning(start_vals_devRate(temperature = seq(4, 40, 3),
                                    dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                                    model_name = "briere1"),
                 "briere1 start values are uninformative; default to `c(tmin = 6, tmax = 32, a = 1e-04)`",
                 fixed = TRUE)
})

test_that("no achieved convergence in nls_multstart() from returns generic starting values from devRate::devRateEqStartVal", {

  # show ensuring no fitting of ssi model to this data
  example_devdata <- data.frame(temperature = runif(6, 10, 40),
                                dev_rate = runif(6, min = -234, max = 101))
  start_vals_prev <- devRate::devRateEqStartVal[[model_name_translate("ssi")]] # take literature start values from devRate
  names(start_vals_prev) <- startvals_names_translate_devrate(start_vals = start_vals_prev,
                                                              "ssi")
  start_upper_vals <- purrr::map(.x = start_vals_prev,
                                 .f = ~.x + abs(.x))
  start_lower_vals <- purrr::map(.x = start_vals_prev,
                                 .f = ~.x - abs(.x))
  fit_starting <- nls.multstart::nls_multstart(formula = dev_rate ~ ssi(temperature, p25, a, b, c, d, e),
                               data = example_devdata,
                               iter = 500,
                               start_lower = start_lower_vals,
                               start_upper = start_upper_vals,
                               supp_errors = "Y")
  expect_true(is.null(fit_starting))
  expect_message(example_correction_startvals <- start_vals_devRate(temperature = runif(6, 10, 40),
                                    dev_rate = runif(6, min = -234, max = 101), # force non-convergence
                                    model_name = "ssi"),
                 "generic starting values")
  expect_equal(unlist(start_vals_prev), example_correction_startvals)
})


