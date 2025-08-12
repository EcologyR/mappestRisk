data("aphid")

set.seed(2025)

tpcs <- fit_devmodels(temp = aphid$temperature,
                      dev_rate = aphid$rate_value,
                      model_name = c("lactin2", "briere2", "mod_weibull")
)

curves <- suppressWarnings(
  predict_curves(temp = aphid$temperature,
                 dev_rate = aphid$rate_value,
                 fitted_parameters = tpcs,
                 model_name_2boot = c("lactin2", "briere2"),
                 propagate_uncertainty = TRUE,
                 n_boots_samples = 2))


test_that("predict_curves output has correct structure", {

  expect_named(curves, c("model_name", "boot_iter", "temp", "dev_rate", "curvetype"))
  expect_equal(unique(curves$model_name), c("briere2", "lactin2"))
  expect_equal(unique(curves$boot_iter), c(NA, "1", "2"))
  expect_equal(min(curves$temp), -5.0)
  expect_equal(max(curves$temp), 45.9)
  expect_type(curves$temp, "double")
  expect_type(curves$dev_rate, "double")
  expect_true(all(curves$dev_rate >= 0))
  expect_equal(unique(curves$curvetype), c("estimate", "uncertainty"))

})


test_that("predict_curves should throw an error if fitted_parameters is not provided", {

  expect_error(predict_curves(temp = aphid$temperature,
                              dev_rate = aphid$rate_value,
                              model_name_2boot = "lactin2",
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 2),
               "`fitted_parameters` must be provided.")
})

test_that("predict_curves should throw an error if model_name is not a character", {

  expect_error(predict_curves(temp = aphid$temperature,
                              dev_rate = aphid$rate_value,
                              fitted_parameters = tpcs,
                              model_name_2boot = 3,
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 2),
               "model not available. Check the models that converged in `fitted_parameters`",
               fixed = TRUE)
}
)

test_that("predict_curves should throw an error if model_name is not from `fitted_parameters`", {

  expect_error(predict_curves(temp = aphid$temperature,
                              dev_rate = aphid$rate_value,
                              fitted_parameters = tpcs,
                              model_name_2boot = "SharpeDeMichele",
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 2),
               "model not available. Check the models that converged in `fitted_parameters`",
               fixed = TRUE)
}
)

test_that("predict_curves should throw a message with the  error of available models for bootstrapping", {

  expect_message(capture_error(predict_curves(temp = aphid$temperature,
                                              dev_rate = aphid$rate_value,
                                              fitted_parameters = tpcs,
                                              model_name_2boot = "SharpeDeMichele",
                                              propagate_uncertainty = TRUE,
                                              n_boots_samples = 100)),
                 paste0("Models available: ", paste0(unique(tpcs$model_name), collapse = ", ")))
}
)


test_that("predict_curves should throw an error if `n_boots_samples` is not an integer", {

  expect_error(predict_curves(temp = aphid$temperature,
                              dev_rate = aphid$rate_value,
                              fitted_parameters = tpcs,
                              model_name_2boot = "lactin2",
                              propagate_uncertainty = TRUE,
                              n_boots_samples = unlist(sample(x = list(9.5, "100", as.factor(90)), size = 1))),
               "`n_boots_samples` must be a positive integer. Please change it within 1 and 5000 (Default 100)",
               fixed = TRUE)
})

test_that("predict_curves should throw an error if `n_boots_samples` is > 5000", {

  expect_error(predict_curves(temp = aphid$temperature,
                              dev_rate = aphid$rate_value,
                              fitted_parameters = tpcs,
                              model_name_2boot = "lactin2",
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 10000),
               "computation time will be extremely high. Please adjust `n_boots_samples` to be < 5000. Usually 100 is fine.",
               fixed = TRUE)
})

test_that("predict_curves should throw an error if `propagate_uncertainty` is not logical", {

  expect_error(suppressWarnings(predict_curves(temp = aphid$temperature,
                                               dev_rate = aphid$rate_value,
                                               fitted_parameters = tpcs,
                                               model_name_2boot = "lactin2",
                                               propagate_uncertainty = sample(c("bootstrap", "yes"), 1),
                                               n_boots_samples = 2)),
               "`propagate_uncertainty` must be `TRUE` or `FALSE` (def. `TRUE`)",
               fixed = TRUE)
})


## few samples for bootstrap yields a warning
test_that("predict_curves should issue a warning if `n_boots_samples` < 100", {

  expect_warning(predict_curves(temp = aphid$temperature,
                                dev_rate = aphid$rate_value,
                                fitted_parameters = tpcs,
                                model_name_2boot = "lactin2",
                                propagate_uncertainty = TRUE,
                                n_boots_samples = 2),
                 regexp = "100 iterations might be desirable. Consider increasing `n_boots_samples` if possible")
})


## no bootstrap accomplished yields a warning (II)

# test_that("predict_curves should issue a warning if no boostrap is accomplished", {
#
#   aphid_parameters <- fit_devmodels(temp = aphid$temperature,
#                                     dev_rate = aphid$rate_value,
#                                     model_name = "lactin1")
#   expect_error(capture_warnings(predicted_curves <- predict_curves(temp = aphid$temperature,
#                                                                    dev_rate = aphid$rate_value,
#                                                                    fitted_parameters = aphid_parameters |>
#                                                                      dplyr::mutate(param_est = purrr::map_dbl(.x = param_est,
#                                                                                                               .f = ~.x*10)), # <- alterate data to ensure not bootstrapping is performed
#                                                                    model_name_2boot = "lactin1", # <- previously known as not adequately converging
#                                                                    propagate_uncertainty = TRUE,
#                                                                    n_boots_samples = 2)),
#   "No bootstrap was attempted. Check your model(s)",
#   fixed = TRUE)
# })




