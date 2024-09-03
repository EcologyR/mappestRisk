## first use a data of example
#set.seed(2024)
#temp_test <- seq(4, 40, 3)
#dev_rate_test <- rnorm(13, mean = 0.02, sd = 0.005)
#fitted_params_example <- fit_devmodels(temp = temp_test,
#                                       dev_rate = dev_rate_test,
#                                       model_name = "all")
fitted_tpcs_aphid <- fit_devmodels(temp = aphid$temperature,
                                   dev_rate = aphid$rate_value,
                                   model_name = "lactin2")
plot_devmodels(temp = aphid$temperature,
               dev_rate = aphid$rate_value,
               fitted_parameters = fitted_tpcs_aphid,
               species = "Brachycaudus schwartzi",
               life_stage = "Nymphs")
# Obtain prediction TPCs with bootstraps for propagating uncertainty:
boots_params_example <- predict_curves(temp = aphid$temperature,
                                        dev_rate = aphid$rate_value,
                                        fitted_parameters = fitted_tpcs_aphid,
                                        model_name_2boot = "lactin2",
                                        propagate_uncertainty = TRUE,
                                        n_boots_samples = 100)
plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example,
                   temp = aphid$temperature,
                   dev_rate = aphid$rate_value,
                   species = "Aphid",
                   life_stage = "Nymph")

# boots_params_example <- predict_curves(temp = temp_test,
#                                        dev_rate = dev_rate_test,
#                                        fitted_parameters = fitted_params_example,
#                                        model_name_2boot = unique(fitted_params_example$model_name)[2],
#                                        propagate_uncertainty = TRUE,
#                                        n_boots_samples = 5)
#
test_that("`therm_suit_bounds()` should throw an error if the structure of `preds_tbl` has been altered from
          that output at `predict_curves()`", {
  expect_error(therm_suit_bounds(preds_tbl = boots_params_example |> dplyr::select(1:3),
                                 model_name = unique(fitted_params_example$model_name)[3],
                                 suitability_threshold = 80),
               "preds_tbl` must be a  `data.frame` inherited   from the output of `mappestRisk::predict_curves()` function",
               fixed = TRUE)
})

test_that("`therm_suit_bounds()` should throw an error if the names of `preds_tbl` has been altered from
          that output at `predict_curves()`", {
            expect_error(therm_suit_bounds(preds_tbl = boots_params_example |> dplyr::rename(temperature = temp),
                                           model_name = unique(fitted_params_example$model_name)[3],
                                           suitability_threshold = 80),
                         "preds_tbl` must be a  `data.frame` inherited   from the output of `mappestRisk::predict_curves()` function",
                         fixed = TRUE)
          })

test_that("`therm_suit_bounds()` should throw an error when no `preds_tbl` is provided", {
            expect_error(therm_suit_bounds(model_name = unique(fitted_params_example$model_name)[3],
                                           suitability_threshold = 80),
                         "The `preds_tbl` argument is absent. Please provide a `tibble` or `data.frame` object
         from `predict_curves()",
                         fixed = TRUE)
          })

test_that("`therm_suit_bounds()` should throw an error if `preds_tbl` is given but it's empty", {
  expect_error(therm_suit_bounds(preds_tbl = boots_params_example |> dplyr::slice(0),
                                 model_name = unique(fitted_params_example$model_name)[3],
                                 suitability_threshold = 80),
               "The `preds_tbl` table is empty; check out the output of `fit_devmodels()` and `predict_curves()`.",
               fixed = TRUE)
})

test_that("`therm_suit_bounds()` should throw a message with default value of `suitability_threshold` when it's not
          provided by the user", {
  expect_message(capture_warnings(therm_suit_bounds(preds_tbl = boots_params_example,
                                   model_name = unique(fitted_params_example$model_name)[3])),
               "No suitability_threshold value input. Default to `suitability_threshold = 75`")
})

test_that("`therm_suit_bounds()` advises with a warning that `suitability_threshold` values below 50
are not an indicator of high suitability but of thermal tolerance", {
            pool_warns <- capture_warnings(therm_suit_bounds(preds_tbl = boots_params_example,
                                                             model_name = unique(fitted_params_example$model_name)[3],
                                                             suitability_threshold = 30))
            expect_true(any(pool_warns == "Suitability thresholds under 50% indicate thermal boundaries for positive development but not
necessarily optimal for pest risk assessment. Subsequent map risk analysis will imply
risk of thermal tolerance at each location rather than risk of optimal performance or high pest pressure."))
})

## model_names
model_name_test <- "lactin1"

test_that("`therm_suit_bounds()` should throw an error if `model_name` is not in `preds_tbl`", {
  expect_error(therm_suit_bounds(preds_tbl = boots_params_example,
                                 model_name = model_name_test,
                                 suitability_threshold = 80),
               paste("Model", model_name_test, "did not fitted well to your data or is not available. Try using another fitted model in your table instead"),
               fixed = TRUE)
})

test_that("`therm_suit_bounds()` should throw an error if more than one model is provided in `model_name`", {
  expect_error(therm_suit_bounds(preds_tbl = boots_params_example,
                                 model_name = c("lactin1", "joehnk"),
                                 suitability_threshold = 80),
               "Only one model is allowed in `therm_suit_bounds()` at a time.
Please use this function repetedly for each of your models one by one",
               fixed = TRUE)
})

test_that("`therm_suit_bounds()` should throw an error when no model is provided in `model_name`
          by the user", {
  expect_error(therm_suit_bounds(preds_tbl = boots_params_example,
                                 suitability_threshold = 80),
               "No model name was provided by the user. Please provide any model present in `pred_tbl`",
               fixed = TRUE)
})

test_that("`therm_suit_bounds()` should give a warning recommending uncertainty propagation if no uncertainty curves
            are present in `preds_tbl`", {
              pool_warns <- capture_warnings(therm_suit_bounds(preds_tbl = boots_params_example |> dplyr::filter(curvetype == "estimate"),
                                             model_name = "joehnk",
                                             suitability_threshold = 80))
              expect_true(any(pool_warns ==  "No bootstrapped predictions were performed.
We strongly recommend to propagate uncertainty by setting the `predict_curves()`
arguments to `propagate_uncertainty = TRUE` and `n_boots_samples = 100`"))
})

test_that("`therm_suit_bounds()` outputs a tibble with appropriate structure", {
              pool_warns <- capture_warnings(therm_suit_bounds(preds_tbl = boots_params_example |> dplyr::filter(curvetype == "estimate"),
                                                               model_name = "joehnk",
                                                               suitability_threshold = 80))
              expect_true(any(pool_warns ==  "No bootstrapped predictions were performed.
We strongly recommend to propagate uncertainty by setting the `predict_curves()`
arguments to `propagate_uncertainty = TRUE` and `n_boots_samples = 100`"))
            })

# Detach the 'car' package to clean up
detach("package:car", unload = TRUE)
