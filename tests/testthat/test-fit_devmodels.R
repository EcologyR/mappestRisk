library(testthat)
library(mappestRisk)

as.character()
# Test input data types
test_that("fit_devmodels should throw an error if temperature data is not numeric", {
  expect_error(fit_devmodels(temp = as.character(seq(4, 40, 3)),
                             dev_rate = rnorm(12, mean = 0.02, sd = 0.005),
                             model_name = "all",
                             variance_model = "exp"))
})

test_that("fit_devmodels should throw an error if development rate data is not numeric
          (e.g. incorrectly importing data from csv/xlsx, using commas as decimal markers, etc)", {
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = as.character(rnorm(12, mean = 0.02, sd = 0.005)),
                             model_name = "all",
                             variance_model = "exp"))
})

test_that("fit_devmodels should throw an error if temperature and development rate inputs are not of same length", {
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                             model_name = "all",
                             variance_model = "exp"))
})

test_that("fit_devmodels should throw an error if model_name is not a character string but a R object", {
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = rnorm(12, mean = 0.02, sd = 0.005),
                             model_name = briere2,
                             variance_model = "exp"))
})

test_that("fit_devmodels should throw an error if model_name is not a character string but a number", {
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                             model_name = 2,
                             variance_model = "exp"))
})

test_that("fit_devmodels should throw an error if model_name is not in available_models", {
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                             model_name = "SharpeDeMichele",
                             variance_model = "exp"))
})

# Test input data ranges and warnings
test_that("fit_devmodels should print a warning if development rate data contains negative
          values or higher than 10", {
  set.seed(2023)
  expect
  expect_warning(fit_devmodels(temp = seq(4, 40, 3),
                               dev_rate = rnorm(13, mean = 0, sd = 0.005),
                               model_name = "all",
                               variance_model = "exp"))
})

test_that("fit_devmodels should issue a warning if temperature data contains values outside of the range of active organisms", {
  set.seed(2023)
  expect_warning(fit_devmodels(temp = c(seq(4, 39, 3), 4000),
                               dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                               model_name = "wang",
                               variance_model = "power"))
})

# Test output object
test_that("fit_devmodels should return a list object", {
  expect_is(fit_devmodels(temp = 1:3, dev_rate = 1:3, model_name = "all", variance_model = "exp"), "list")
})

test_that("fit_devmodels should return a list object with class `dev_rate_fit`", {
  expect_s3_class(fit_devmodels(temp = 1:3, dev_rate = 1:3, model_name = "all", variance_model = "exp"), "dev_rate_fit")
})
