
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
               "Poorly informative start values for briere1 model")
})


