
fitted_params_example <- fit_devmodels(temp = seq(4, 40, 3),
                                       dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                                       model_name = "all")

test_that("predict_curves should throw an error if temperature data is not numeric", {
  expect_error(predict_curves(temp =  as.character(seq(4, 40, 3)),
                              dev_rate = rnorm(12, mean = 0.02, sd = 0.005),
                              fitted_parameters = fitted_params_example,
                              model_name_2boot = "lactin2",
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 100),
               "temperature data is not numeric. Please check it.")
})

test_that("predict_curves should throw an error if temperature data have just three values", {
  expect_error(predict_curves(temp =  c(15, 20, 25),
                              dev_rate = rnorm(3, mean = 0.02, sd = 0.005),
                              fitted_parameters = fitted_params_example,
                              model_name_2boot = "lactin2",
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 100),
               "At least four different temperature treatments in the data are required.",
               fixed = TRUE)
})

test_that("predict_curves should throw an error if development rate data is not numeric
          (e.g. incorrectly importing data from csv/xlsx, using commas as decimal markers, etc)", {
            expect_error(predict_curves(temp = seq(4, 40, 3),
                                        dev_rate = as.factor(rnorm(12, mean = 0.02, sd = 0.005)),
                                        fitted_parameters = fitted_params_example,
                                        model_name_2boot = "lactin2",
                                        propagate_uncertainty = TRUE,
                                        n_boots_samples = 100),
                         "development rate data is not numeric. Please check it.")
          })

test_that("predict_curves should throw an error if temperature and development rate inputs are not of same length", {
  expect_error(predict_curves(temp = seq(4, 40, 3),
                              dev_rate = rnorm(10, mean = 0.02, sd = 0.005),
                              fitted_parameters = fitted_params_example,
                              model_name_2boot = "lactin2",
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 100),
               "development rate and temperature inputs are not of same length. Please check it.")
})


test_that("predict_curves should throw an error if model_name is not a character", {
  expect_error(predict_curves(temp = seq(4, 40, 3),
                              dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                              fitted_parameters = fitted_params_example,
                              model_name_2boot = 3,
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 100),
               "model not available. Check the models that converged in `fitted_parameters`",
               fixed = TRUE)
               }
  )

test_that("predict_curves should throw an error if model_name is not from `fitted_parameters`", {
  expect_error(predict_curves(temp = seq(4, 40, 3),
                              dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                              fitted_parameters = fitted_params_example,
                              model_name_2boot = "SharpeDeMichele",
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 10),
               "model not available. Check the models that converged in `fitted_parameters`",
               fixed = TRUE)
}
)

test_that("predict_curves should throw a message with the  error of available models for bootstrapping", {
  expect_message(capture_error(predict_curves(temp = seq(4, 40, 3),
                                              dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                                              fitted_parameters = fitted_params_example,
                                              model_name_2boot = "SharpeDeMichele",
                                              propagate_uncertainty = TRUE,
                                              n_boots_samples = 100)),
                 paste0("Models available: ", paste0(unique(fitted_params_example$model_name), collapse = ", ")))
               }
  )



test_that("predict_curves should throw an error if development rate is negative, which is biologically unrealistic", {
  expect_error(predict_curves(temp = seq(4, 40, 3),
                              dev_rate = c(rnorm(12, mean = 0.02, sd = 0.005), -abs(rnorm(1))),
                              fitted_parameters = fitted_params_example,
                              model_name_2boot = "lactin2",
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 100),
               "Negative dev_rate development rate data found. Please check it.")
})

# Test input data ranges and warnings

test_that("predict_curves should throw an error if temperature data contains values outside of the range of active organisms", {
  expect_error(predict_curves(temp = c(seq(4, 39, 3), 4000),
                               dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                               fitted_parameters = fitted_params_example,
                               model_name_2boot = "lactin2",
                               propagate_uncertainty = TRUE,
                               n_boots_samples = 10),
                 "experienced temperatures by active organisms are usually between 0 and 50 degrees centigrades",
                 fixed = TRUE)
})

## few samples for bootstrap yields a warning
test_that("predict_curves should issue a warning if `n_boots_samples` < 100", {
  requireNamespace("car", quietly = T)
  capt_warnings <- capture_warnings(predict_curves(temp = seq(4, 39, 3),
                                    dev_rate = rnorm(12, mean = 0.02, sd = 0.005),
                                    fitted_parameters = fitted_params_example,
                                    model_name_2boot = "lactin1",
                                    propagate_uncertainty = TRUE,
                                    n_boots_samples = 2))
expect_true(any(capt_warnings == "100 iterations might be desirable. Consider increasing `n_boots_samples` if possible"))
})


## no bootstrap accomplished yields a warning (II)
aphid_parameters <- fit_devmodels(temp = aphid$temperature,
                                  dev_rate = aphid$rate_value,
                                  model_name = "all")

test_that("predict_curves should issue a warning if no boostrap is accomplished", {
  requireNamespace("car", quietly = T)
  capt_warnings <- capture_warnings(predicted_curves <- predict_curves(temp = aphid$temperature,
                                                                       dev_rate = aphid$rate_value,
                                                                       fitted_parameters = aphid_parameters,
                                                                       model_name_2boot = "boatman",
                                                                       propagate_uncertainty = FALSE,
                                                                       n_boots_samples = 5))

  expect_true(any(capt_warnings == "No bootstrap was performed. We strongly recommend to propagate uncertainty."))
})

# error for 0 samples to boot if desired uncertainty propagation.
test_that("predict_curves should throw an error if `n_boots_samples` is set to 0 and `propagate_uncertainty` is TRUE", {
  requireNamespace("car", quietly = T)
  expect_error(suppressWarnings(predict_curves(temp = seq(4, 39, 3),
                                               dev_rate = rnorm(12, mean = 0.02, sd = 0.005),
                                               fitted_parameters = fitted_params_example,
                                               model_name_2boot = "lactin2",
                                               propagate_uncertainty = TRUE,
                                               n_boots_samples = 0)),
               "`n_boots_samples` must be a positive integer whenever `propagate_uncertainty` is set to `TRUE`.")
  })
