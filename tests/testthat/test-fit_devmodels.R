
# input test
test_that("fit_devmodels should throw an error if temperature data is not numeric", {
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(fit_devmodels(temp = as.character(seq(4, 40, 3)),
                             dev_rate = sample(rate_test, 12),
                             model_name = c("lactin2", "briere2")),
               "temperature data is not numeric. Please check it.")
})

test_that("fit_devmodels should throw an error if temperature data is a data.frame", {
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(fit_devmodels(temp = data.frame(temperature = seq(4, 40, 3),
                                               temp_error = runif(13, 0, 2)),
                             dev_rate = sample(rate_test, 12),
                             model_name = c("lactin2", "briere2")),
               "temperature data is not numeric. Please check it.")
})

test_that("fit_devmodels should throw an error if temperature data have just three values", {
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(fit_devmodels(temp = c(15, 20, 25),
                             dev_rate = sample(rate_test, 3),
                             model_name = c("lactin2", "briere2")),
               "At least four different temperature treatments in the data are required.",
               fixed = TRUE)
})

test_that("fit_devmodels should throw an error if development rate data is not numeric
          (e.g. incorrectly importing data from csv/xlsx, using commas as decimal markers, etc)", {
            rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
            expect_error(fit_devmodels(temp = seq(4, 40, 3),
                                       dev_rate = as.character(rate_test),
                                       model_name = c("lactin2", "briere2")),
                         "development rate data is not numeric. Please check it.")
          })

test_that("fit_devmodels should throw an error if temperature and development rate inputs are not of same length", {
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = sample(rate_test, 12),
                             model_name = c("lactin2", "briere2")),
               "development rate and temperature inputs are not of same length. Please check it.")
})

test_that("fit_devmodels should throw an error if model_name is not a character string but a number", {
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = rate_test,
                             model_name = 2),
               "model_name must be a string in ?available_models",
               fixed = TRUE)
})

test_that("fit_devmodels should throw an error if model_name is not in available_models", {
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = rate_test,
                             model_name = "SharpeDeMichele"),
               "model not available. For available model names, see `available_models`")
})

test_that("fit_devmodels should throw an error if development rate is negative, which is biologically unrealistic", {
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = c(sample(rate_test, 12), -abs(rnorm(1))),
                             model_name = c("lactin2", "briere2")),
               "Negative dev_rate development rate data found. Please check it.")
})

# Test input data ranges and warnings

test_that("fit_devmodels should throw an error if temperature data contains values outside of the range of active organisms", {
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(fit_devmodels(temp = c(seq(4, 39, 3), 4000),
                             dev_rate = rate_test,
                             model_name = c("lactin2", "briere2")),
               "experienced temperatures by active organisms are usually between 0 and 50 degrees centigrades",
               fixed = TRUE)
})

# Test output object
test_that("fit_devmodels should return a list object", {
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_type(fit_devmodels(temp = c(seq(4, 40, 3)),
                            dev_rate = rate_test,
                            model_name = c("lactin2", "briere2")),
              type = "list")
})


## test that rate of development data have NAs
test_that("fit_devmodels should stop the function and advise the user about NAs in the data set", {
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(fit_devmodels(temp = c(seq(4, 40, 3)),
                             dev_rate = c(sample(rate_test, 12), NA),
                             model_name = c("lactin2", "briere2")),
               "development rate data have NAs; please consider removing them or fixing them")
})

## test that temperature data have NAs
test_that("fit_devmodels should stop the function and advise the user about NAs in the data set", {
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(fit_devmodels(temp = c(seq(4, 39, 3), NA),
                             dev_rate = rate_test,
                             model_name = c("lactin2", "briere2")),
               "temperature data have NAs; please consider removing them or fixing them")
})

## test that no converged model yields a warning
test_that("fit_devmodels warns no convergence", {
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_warning(fit_devmodels(temp = c(21, 28, 35, 42),
                               dev_rate = c(.5, 0, 0.8, 0.8),
                               model_name = c("mod_polynomial")),
                 "no model converged adequately for fitting your data")
})

## test that a fitted model is accessible through the fitted_params tbl
test_that("nls object is retrieved and of correct class", {
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  fitted_parameters <-fit_devmodels(temp = seq(4, 40, 3),
                                    dev_rate = rate_test,
                                    model_name = c("lactin2", "briere2"))
  expect_true(all(class(fitted_parameters$model_fit[[1]])[1] == "nls"))

})

## test that a fitted model summary is accessible
test_that("nls object and its summary and table of parameters and statistics are correct", {
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  suppressWarnings(fitted_parameters <-fit_devmodels(temp = seq(4, 40, 3),
                                                     dev_rate = rate_test,
                                                     model_name = c("lactin2", "briere2")))
  sum_fitted <- summary(fitted_parameters$model_fit[[1]])
  expect_true(all(colnames(sum_fitted$tTable) == c("Value", "Std.Error", "t-value", "p-value")))

})


