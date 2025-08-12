data("aphid")

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


