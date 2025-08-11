data("aphid")

tpcs <- fit_devmodels(temp = aphid$temperature,
                      dev_rate = aphid$rate_value,
                      model_name = c("lactin2")
)
#' fitted_tpcs
#'


test_that("fit_devmodels returns well structured output with more than one model", {

  out <- fit_devmodels(temp = aphid$temperature,
                       dev_rate = aphid$rate_value,
                       model_name = c("lactin2", "beta"))

  expect_true(inherits(out, "data.frame"))
  expect_named(out, c("model_name", "param_name", "start_vals", "param_est", "param_se",
                      "model_AIC", "model_BIC", "model_fit"))
  expect_equal(out$model_name, c(rep("lactin2", times = 4), rep("beta", times = 5)))
  expect_equal(out$param_name, c("a", "b", "tmax", "delta_t", "a", "b", "c", "d", "e"))
  expect_type(out$param_est, "double")
  expect_type(out$param_se, "double")
  expect_type(out$model_AIC, "double")
  expect_type(out$model_BIC, "double")
  expect_type(out$model_fit, "list")
  expect_true(all(sapply(out$model_fit[c(2:4, 6:9)], is.null)))
  expect_true(class(out$model_fit[[1]]) == "nls")
  expect_true(class(out$model_fit[[5]]) == "nls")

})

# input test
test_that("fit_devmodels should throw an error if temperature data is not numeric", {

  expect_error(fit_devmodels(temp = as.character(seq(4, 40, 3)),
                             dev_rate = aphid$rate_value,
                             model_name = "all"),
               "temperature data is not numeric. Please check it.")
})

test_that("fit_devmodels should throw an error if temperature data is a data.frame", {

  expect_error(fit_devmodels(temp = data.frame(temperature = seq(4, 40, 3),
                                               temp_error = runif(13, 0, 2)),
                             dev_rate = aphid$rate_value,
                             model_name = "all"),
               "temperature data is not numeric. Please check it.")
})

test_that("fit_devmodels should throw an error if temperature data have just three values", {

  expect_error(fit_devmodels(temp = c(15, 20, 25),
                             dev_rate = aphid$rate_value,
                             model_name = "all"),
               "At least four different temperature treatments in the data are required.",
               fixed = TRUE)
})

test_that("fit_devmodels should throw an error if development rate data is not numeric
          (e.g. incorrectly importing data from csv/xlsx, using commas as decimal markers, etc)", {

            expect_error(fit_devmodels(temp = seq(4, 40, 3),
                                       dev_rate = as.character(aphid$rate_value),
                                       model_name = "all"),
                         "development rate data is not numeric. Please check it.")
          })

test_that("fit_devmodels should throw an error if temperature and development rate inputs are not of same length", {

  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = seq(0, 1, by = 0.1),
                             model_name = "all"),
               "development rate and temperature inputs are not of same length. Please check it.")
})

test_that("fit_devmodels should throw an error if model_name is not a character string but a number", {

  expect_error(fit_devmodels(temp = aphid$temperature,
                             dev_rate = aphid$rate_value,
                             model_name = 2),
               "model_name must be a string in ?available_models",
               fixed = TRUE)
})

test_that("fit_devmodels should throw an error if model_name is not in available_models", {

  expect_error(fit_devmodels(temp = aphid$temperature,
                             dev_rate = aphid$rate_value,
                             model_name = "SharpeDeMichele"),
               "model not available. For available model names, see `available_models`")
})

test_that("fit_devmodels should throw an error if development rate is negative, which is biologically unrealistic", {
  expect_error(fit_devmodels(temp = aphid$temperature,
                             dev_rate = -1*aphid$rate_value,
                             model_name = "all"),
               "Negative dev_rate development rate data found. Please check it.")
})

# Test input data ranges and warnings

test_that("fit_devmodels should throw an error if temperature data contains values outside of the range of active organisms", {

  expect_error(fit_devmodels(temp = c(seq(4, 39, 3), 4000),
                             dev_rate = seq(0, 1, length.out = 13),
                             model_name = "all"),
               "experienced temperatures by active organisms are usually between 0 and 50 degrees centigrades",
               fixed = TRUE)
})


## test that rate of development data have NAs
test_that("fit_devmodels should stop the function and advise the user about NAs in the data set", {

  expect_error(fit_devmodels(temp = c(aphid$temperature, 10),
                             dev_rate = c(aphid$rate_value, NA),
                             model_name = "all"),
               "development rate data have NAs; please consider removing them or fixing them")
})

## test that get error if temperature data have NAs
test_that("fit_devmodels should stop the function and advise the user about NAs in the data set", {

  expect_error(fit_devmodels(temp = c(aphid$temperature, NA),
                             dev_rate = aphid$rate_value,
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


## test message for exclusion of ratkwosky, wang and mod_polynomial
test_that("nls object is retrieved and of correct class", {

  expect_message(fit_devmodels(temp = aphid$temperature,
                               dev_rate = aphid$rate_value,
                               model_name = "all"),
                 "By default, all models are fitted except `ratkowsky`, `mod_polynomial` and `wang` due to
unrealistic behavior at some TPC regions. If you still want to fit them, please write all model names manually")


})

# ## test that a fitted model summary is accessible
# test_that("nls object and its summary and table of parameters and statistics are correct", {
#
#   fitted_parameters <- fit_devmodels(temp = aphid$temperature,
#                                                      dev_rate = aphid$rate_value,
#                                                      model_name = "lactin2")
#   sum_fitted <- summary(fitted_parameters$model_fit[[1]])
#   expect_true(all(colnames(sum_fitted$tTable) == c("Value", "Std.Error", "t-value", "p-value")))
#
# })


