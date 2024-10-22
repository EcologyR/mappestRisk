
data("available_models")

model_name_test <- available_models |>
  dplyr::filter(package == "devRate") |>
  dplyr::slice_sample(n = 1)

rate_test <- rnorm(13, mean = 0.02, sd = 0.005)
temp_test <- seq(4, 40, 3)

# input tests (check_data())
test_that("start_vals_devRate should throw an error if temperature data is not numeric", {
  expect_error(start_vals_devRate(temperature = as.character(temp_test),
                                  dev_rate = rate_test,
                                  model_name_2fit = model_name_test),
               "temperature data is not numeric. Please check it.")
               })

test_that("start_vals_devRate should throw an error if temperature data has NAs", {
  expect_error(start_vals_devRate(temperature = c(temp_test[1:12], NA),
                                  dev_rate = rate_test,
                                  model_name_2fit = model_name_test),
               "temperature data have NAs; please consider removing them or fixing them")
})

test_that("start_vals_devRate should throw an error if development rate data has NAs", {
  expect_error(start_vals_devRate(temperature = temp_test,
                                  dev_rate = c(rate_test[1:12], NA),
                                  model_name_2fit = model_name_test),
               "development rate data have NAs; please consider removing them or fixing them")
})

test_that("start_vals_devRate should throw an error if temperature data is a data.frame", {
  expect_error(start_vals_devRate(temperature = data.frame(temperature = temp_test,
                                                           temp_error = runif(13, 0, 2)),
                                    dev_rate = rate_test,
                                    model_name_2fit = model_name_test),
                 "temperature data is not numeric. Please check it.")
  })

test_that("start_vals_devRate should throw an error if temperature only has three values", {
  expect_error(start_vals_devRate(temperature = temp_test[1:3],
                                  dev_rate = rate_test[1:3],
                                  model_name_2fit = model_name_test),
               "At least four different temperature treatments in the data are required.")
})

test_that("start_vals_devRate should throw an error if dev_rate data is not numeric", {
  expect_error(start_vals_devRate(temperature = temp_test,
                                  dev_rate = as.factor(rate_test),
                                  model_name_2fit = model_name_test),
               "development rate data is not numeric. Please check it.")
})

test_that("start_vals_devRate should throw an error if dev_rate data is not numeric", {
  expect_error(start_vals_devRate(temperature = temp_test[1:10],
                                  dev_rate = rate_test[1:12],
                                  model_name_2fit = model_name_test),
               "development rate and temperature inputs are not of same length. Please check it.")
})


test_that("start_vals_devRate should throw an error if dev_rate data is not numeric", {
  expect_message(start_vals_devRate(temperature = temp_test,
                                  dev_rate = rate_test,
                                  model_name_2fit = available_models |>
                                    dplyr::filter(model_name == "briere1")),
               "Poorly informative start values for Brière-1 model")
})

test_that("no achieved convergence in nls_multstart() from returns generic starting values from devRate::devRateEqStartVal", {

  # show ensuring no fitting of ssi model to this data
  example_devdata <- data.frame(temperature = runif(7, 10, 40),
                                dev_rate = runif(7, min = -234, max = 101))
  start_vals_prev <- devRate::devRateEqStartVal[[model_name_translate("regniere")]] # take literature start values from devRate
  names(start_vals_prev) <- startvals_names_translate_devrate(start_vals = start_vals_prev,
                                                              "regniere")
  start_upper_vals <- purrr::map(.x = start_vals_prev,
                                 .f = ~.x + abs(.x))
  start_lower_vals <- purrr::map(.x = start_vals_prev,
                                 .f = ~.x - abs(.x))
  fit_starting <- nls.multstart::nls_multstart(formula = dev_rate ~ regniere(temperature, tmin, tmax, phi, delta_b, delta_m, b),
                               data = example_devdata,
                               iter = 500,
                               start_lower = start_lower_vals,
                               start_upper = start_upper_vals,
                               supp_errors = "Y",
                               control = minpack.lm::nls.lm.control(maxiter = 5))
  expect_true(is.null(fit_starting))
  expect_message(example_correction_startvals <- start_vals_devRate(temperature = runif(6, 10, 40),
                                                                    dev_rate = runif(6, min = .5, max = .55), # force non-convergence
                                                                    model_name = available_models |>
                                                                      dplyr::filter(model_name == "regniere")),
                 "generic starting values")
  expect_equal(unlist(start_vals_prev), example_correction_startvals)
})


