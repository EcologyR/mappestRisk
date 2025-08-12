
data("aphid")

set.seed(2025)

tpcs <- fit_devmodels(temp = aphid$temperature,
                      dev_rate = aphid$rate_value,
                      model_name = c("lactin2", "briere2")
)

curves <- suppressWarnings(
  predict_curves(temp = aphid$temperature,
                 dev_rate = aphid$rate_value,
                 fitted_parameters = tpcs,
                 model_name_2boot = c("lactin2", "briere2"),
                 propagate_uncertainty = TRUE,
                 n_boots_samples = 2))

bounds <- therm_suit_bounds(curves, model_name = "lactin2", suitability_threshold = 80)



test_that("`therm_suit_bounds()` produces correct output", {

  #constructive::construct(bounds)
  expect_equal(bounds,
               dplyr::tibble(
                 model_name = "lactin2",
                 suitability = "80%",
                 tval_left = c(22.200000000000003, 21.1, 21.5),
                 tval_right = c(31.700000000000003, 32, 31.800000000000004),
                 pred_suit = c(0.11538113759547292, 0.11496571239406227, 0.11329888277224544),
                 iter = c("1", "2", "estimate"),
               ))
})


test_that("`therm_suit_bounds()` should throw an error if the structure of `preds_tbl` has been altered from
          that output at `predict_curves()`", {

            expect_error(therm_suit_bounds(preds_tbl = curves |> dplyr::select(1:3),
                                           model_name = unique(curves$model_name)[1],
                                           suitability_threshold = 80),
                         "`preds_tbl` must be a `data.frame` with columns as produced by the `predict_curves()` function",
                         fixed = TRUE)
          })


test_that("`therm_suit_bounds()` should throw an error if the names of `preds_tbl` has been altered from
          that output at `predict_curves()`", {

            expect_error(therm_suit_bounds(preds_tbl = curves |> dplyr::rename(temperature = temp),
                                           model_name = unique(curves$model_name)[1],
                                           suitability_threshold = 80),
                         "`preds_tbl` must be a `data.frame` with columns as produced by the `predict_curves()` function",
                         fixed = TRUE)
          })

test_that("`therm_suit_bounds()` should throw an error when no `preds_tbl` is provided", {

  expect_error(therm_suit_bounds(model_name = "lactin2",
                                 suitability_threshold = 80),
               "The `preds_tbl` argument is missing. Please provide a `tibble` or `data.frame` object
         from `predict_curves()",
               fixed = TRUE)
})

test_that("`therm_suit_bounds()` should throw an error if `preds_tbl` is given but it's empty", {

  expect_error(therm_suit_bounds(preds_tbl = curves |> dplyr::slice(0),
                                 model_name = unique(curves$model_name)[1],
                                 suitability_threshold = 80),
               "The `preds_tbl` table is empty; check out the output of `fit_devmodels()` and `predict_curves()`.",
               fixed = TRUE)
})

test_that("`therm_suit_bounds()` should throw a message with default value of `suitability_threshold` when it's not
          provided by the user", {

            expect_message(capture_warnings(therm_suit_bounds(preds_tbl = curves,
                                                              model_name = "lactin2")),
                           "No suitability_threshold value provided. Default to `suitability_threshold = 75`")
          })

test_that("`therm_suit_bounds()` advises with a warning that `suitability_threshold` values below 50
are not an indicator of high suitability but of thermal tolerance", {

  pool_warns <- capture_warnings(therm_suit_bounds(preds_tbl = curves,
                                                   model_name = "lactin2",
                                                   suitability_threshold = 30))
  expect_true(pool_warns == "Suitability thresholds under 50% indicate thermal boundaries for positive development but not\n    necessarily optimal for pest risk assessment. Subsequent map risk analysis will imply\n    risk of thermal tolerance at each location rather than risk of optimal performance or high pest pressure.")
})

## model_names
test_that("`therm_suit_bounds()` should throw an error if `model_name` is not in `preds_tbl`", {

  models <- c("lactin1", "lactin3")
  expect_error(therm_suit_bounds(preds_tbl = curves,
                                 model_name = models,
                                 suitability_threshold = 80),
               paste("Model(s)", paste(models, collapse = ", "),
                     "is/are not available in `preds_tbl`.
    Try using another fitted model in your table instead"),
               fixed = TRUE)
})



test_that("`therm_suit_bounds()` should throw an error when no model is provided in `model_name`
          by the user", {

            expect_error(therm_suit_bounds(preds_tbl = curves,
                                           suitability_threshold = 80),
                         "No model name was provided by the user. Please provide any model present in `pred_tbl`",
                         fixed = TRUE)
          })

test_that("`therm_suit_bounds()` should give a warning recommending uncertainty propagation if no uncertainty curves
            are present in `preds_tbl`", {

              pool_warns <- capture_warnings(therm_suit_bounds(preds_tbl = curves |> dplyr::filter(curvetype == "estimate"),
                                                               model_name = "lactin2",
                                                               suitability_threshold = 80))
              expect_true(pool_warns ==
                            "No bootstrapped predictions were performed.\n    We strongly recommend to propagate uncertainty by setting the `predict_curves()`\n            arguments to `propagate_uncertainty = TRUE` and `n_boots_samples = 100`")
            })



