# input test
test_that("fit_devmodels should throw an error if temperature data is not numeric", {
  expect_error(fit_devmodels(temp = as.character(seq(4, 40, 3)),
                             dev_rate = rnorm(12, mean = 0.02, sd = 0.005),
                             model_name = "all"),
               "temperature data is not numeric. Please check it.")
})

test_that("fit_devmodels should throw an error if temperature data is a data.frame", {
  expect_error(fit_devmodels(temp = data.frame(temperature = seq(4, 40, 3),
                                               temp_error = runif(13, 0, 2)),
                             dev_rate = rnorm(12, mean = 0.02, sd = 0.005),
                             model_name = "all"),
               "temperature data is not numeric. Please check it.")
})

test_that("fit_devmodels should throw an error if temperature data have just three values", {
  expect_error(fit_devmodels(temp = c(15, 20, 25),
                             dev_rate = rnorm(3, mean = 0.02, sd = 0.005),
                             model_name = "all"),
               "At least four different temperature treatments in the data are required.",
               fixed = TRUE)
})

test_that("fit_devmodels should throw an error if development rate data is not numeric
          (e.g. incorrectly importing data from csv/xlsx, using commas as decimal markers, etc)", {
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = as.character(rnorm(13, mean = 0.02, sd = 0.005)),
                             model_name = "all"),
               "development rate data is not numeric. Please check it.")
})

test_that("fit_devmodels should throw an error if temperature and development rate inputs are not of same length", {
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = rnorm(12, mean = 0.02, sd = 0.005),
                             model_name = "all"),
               "development rate and temperature inputs are not of same length. Please check it.")
})

test_that("fit_devmodels should throw an error if model_name is not a character string but a number", {
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                             model_name = 2),
               "model_name must be a string in ?available_models",
               fixed = TRUE)
})

test_that("fit_devmodels should throw an error if model_name is not in available_models", {
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                             model_name = "SharpeDeMichele"),
               "model not available. For available model names, see `available_models`")
})

test_that("fit_devmodels should throw an error if development rate is negative, which is biologically unrealistic", {
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = c(rnorm(12, mean = 0.02, sd = 0.005), -abs(rnorm(1))),
                             model_name = "all"),
               "Negative dev_rate development rate data found. Please check it.")
})

# Test input data ranges and warnings

test_that("fit_devmodels should issue a warning if temperature data contains values outside of the range of active organisms", {
  expect_warning(fit_devmodels(temp = c(seq(4, 39, 3), 4000),
                                             dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                                             model_name = "all"),
                 "experienced temperatures by active organisms are usually between 0 and 50 degrees centigrades",
                 fixed = TRUE)
})

# Test output object
test_that("fit_devmodels should return a list object", {
  expect_type(fit_devmodels(temp = c(seq(4, 40, 3)),
                          dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                          model_name = "all"),
              type = "list")
})


## test that rate of development data have NAs
test_that("fit_devmodels should stop the function and advise the user about NAs in the data set", {
            expect_error(fit_devmodels(temp = c(seq(4, 40, 3)),
                                       dev_rate = c(rnorm(12, mean = 0.02, sd = 0.005), NA),
                                       model_name = "all"),
                         "development rate data have NAs; please consider removing them or fixing them")
          })

## test that temperature data have NAs
test_that("fit_devmodels should stop the function and advise the user about NAs in the data set", {
  expect_error(fit_devmodels(temp = c(seq(4, 40, 3), NA),
                             dev_rate = c(rnorm(14, mean = 0.02, sd = 0.005)),
                             model_name = "all"),
               "temperature data have NAs; please consider removing them or fixing them")
})

## test that no converged model yields a warning
test_that("fit_devmodels warns no convergence", {
  expect_warning(fit_devmodels(temp = c(21, 28, 35, 42),
                               dev_rate = c(.5, 0, 0.8, 0.8),
                               model_name = c("mod_polynomial")),
               "no model converged adequately for fitting your data")
})

## test that a fitted model is accessible through the fitted_params tbl
test_that("nls object is retrieved and of correct class", {
  fitted_parameters <-fit_devmodels(temp = seq(4, 40, 3),
                            dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                            model_name = "all")
  expect_true(all(class(fitted_parameters$model_fit[[1]])[1] == "nls"))

})

## test that a fitted model summary is accessible
test_that("nls object and its summary and table of parameters and statistics are correct", {
  suppressWarnings(fitted_parameters <-fit_devmodels(temp = seq(4, 40, 3),
                                    dev_rate = rnorm(13, mean = 0.12, sd = 0.1),
                                    model_name = "all"))
  sum_fitted <- summary(fitted_parameters$model_fit[[1]])
  expect_true(all(colnames(sum_fitted$tTable) == c("Value", "Std.Error", "t-value", "p-value")))

})


