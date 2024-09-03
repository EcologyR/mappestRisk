## first use a data of example
#set.seed(2024)
#temp_test <- seq(4, 40, 3)
#dev_rate_test <- rnorm(13, mean = 0.02, sd = 0.005)
#fitted_params_example <- fit_devmodels(temp = temp_test,
#                                       dev_rate = dev_rate_test,
#                                       model_name = "all")
library(car)

fitted_tpcs_aphid <- fit_devmodels(temp = aphid$temperature,
                                   dev_rate = aphid$rate_value,
                                   model_name = "lactin2")
plot_devmodels(temp = aphid$temperature,
               dev_rate = aphid$rate_value,
               fitted_parameters = fitted_tpcs_aphid,
               species = "Brachycaudus schwartzi",
               life_stage = "Nymphs")


test_that("`therm_suit_bounds()` should throw an error if the structure of `preds_tbl` has been altered from
          that output at `predict_curves()`", {
            requireNamespace("car", quietly = TRUE)
            boots_params_example <- predict_curves(temp = aphid$temperature,
                                                   dev_rate = aphid$rate_value,
                                                   fitted_parameters = fitted_tpcs_aphid,
                                                   model_name_2boot = "lactin2",
                                                   propagate_uncertainty = TRUE,
                                                   n_boots_samples = 3)
  expect_error(therm_suit_bounds(preds_tbl = boots_params_example |> dplyr::select(1:3),
                                 model_name = unique(fitted_params_example$model_name)[3],
                                 suitability_threshold = 80),
               "preds_tbl` must be a  `data.frame` inherited   from the output of `mappestRisk::predict_curves()` function",
               fixed = TRUE)
})
