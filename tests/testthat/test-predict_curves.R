## for test coverage with devtools, un-silence lines 3 to 6 and silence all lines containing `readRDS`

# rate_sample <- rnorm(13, mean = 0.02, sd = 0.005)
# fitted_params_example <- fit_devmodels(temp = seq(4, 40, 3),
#                                        dev_rate = rate_test,
#                                        model_name = "all")

# 2. Bootstrap predictions ----------------------------------------------


test_that("predict_curves should throw an error if temperature data is not numeric", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(predict_curves(temp =  as.character(seq(4, 40, 3)),
                              dev_rate = rate_sample,
                              fitted_parameters = fitted_params_example,
                              model_name_2boot = "lactin2",
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 100),
               "temperature data is not numeric. Please check it.")
})

test_that("predict_curves should throw an error if temperature data have just three values", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
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
            fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
            rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
            expect_error(predict_curves(temp = seq(4, 40, 3),
                                        dev_rate = as.factor(rate_sample),
                                        fitted_parameters = fitted_params_example,
                                        model_name_2boot = "lactin2",
                                        propagate_uncertainty = TRUE,
                                        n_boots_samples = 100),
                         "development rate data is not numeric. Please check it.")
          })

test_that("predict_curves should throw an error if temperature and development rate inputs are not of same length", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(predict_curves(temp = seq(4, 40, 3),
                              dev_rate = rnorm(10, mean = 0.02, sd = 0.005),
                              fitted_parameters = fitted_params_example,
                              model_name_2boot = "lactin2",
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 100),
               "development rate and temperature inputs are not of same length. Please check it.")
})

test_that("predict_curves should throw an error if fitted_parameters is not provided", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(predict_curves(temp =  seq(4, 40, 3),
                              dev_rate = rate_sample,
                              model_name_2boot = "lactin2",
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 100),
               "`fitted_parameters` must be provided.")
})

test_that("predict_curves should throw an error if model_name is not a character", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(predict_curves(temp = seq(4, 40, 3),
                              dev_rate = rate_sample,
                              fitted_parameters = fitted_params_example,
                              model_name_2boot = 3,
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 100),
               "model not available. Check the models that converged in `fitted_parameters`",
               fixed = TRUE)
               }
  )

test_that("predict_curves should throw an error if model_name is not from `fitted_parameters`", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(predict_curves(temp = seq(4, 40, 3),
                              dev_rate = rate_sample,
                              fitted_parameters = fitted_params_example,
                              model_name_2boot = "SharpeDeMichele",
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 10),
               "model not available. Check the models that converged in `fitted_parameters`",
               fixed = TRUE)
}
)

test_that("predict_curves should throw a message with the  error of available models for bootstrapping", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_message(capture_error(predict_curves(temp = seq(4, 40, 3),
                                              dev_rate = rate_sample,
                                              fitted_parameters = fitted_params_example,
                                              model_name_2boot = "SharpeDeMichele",
                                              propagate_uncertainty = TRUE,
                                              n_boots_samples = 100)),
                 paste0("Models available: ", paste0(unique(fitted_params_example$model_name), collapse = ", ")))
               }
  )



test_that("predict_curves should throw an error if development rate is negative, which is biologically unrealistic", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(predict_curves(temp = seq(4, 40, 3),
                              dev_rate = c(sample(rate_sample, 12), -abs(rnorm(1))),
                              fitted_parameters = fitted_params_example,
                              model_name_2boot = "lactin2",
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 100),
               "Negative dev_rate development rate data found. Please check it.")
})

# Test input data ranges and warnings

test_that("predict_curves should throw an error if temperature data contains values outside of the range of active organisms", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(predict_curves(temp = c(seq(4, 39, 3), 4000),
                               dev_rate = rate_sample,
                               fitted_parameters = fitted_params_example,
                               model_name_2boot = "lactin2",
                               propagate_uncertainty = TRUE,
                               n_boots_samples = 10),
                 "experienced temperatures by active organisms are usually between 0 and 50 degrees centigrades",
                 fixed = TRUE)
})

test_that("predict_curves should throw an error if `n_boots_samples` is not an integer", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(predict_curves(temp = seq(4, 40, 3),
                              dev_rate = rate_sample,
                              fitted_parameters = fitted_params_example,
                              model_name_2boot = "lactin2",
                              propagate_uncertainty = TRUE,
                              n_boots_samples = unlist(sample(x = list(9.5, "100", as.factor(90)), size = 1))),
               "`n_boots_samples` must be a positive integer. Please change it within 1 and 5000 (Default 100)",
               fixed = TRUE)
})

test_that("predict_curves should throw an error if `n_boots_samples` is > 5000", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(predict_curves(temp = seq(4, 40, 3),
                              dev_rate = rate_sample,
                              fitted_parameters = fitted_params_example,
                              model_name_2boot = "lactin2",
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 10000),
               "computation time will be extremely high. Please adjust `n_boots_samples` to be < 5000. Usually 100 is fine.",
               fixed = TRUE)
})

test_that("predict_curves should throw an error if `propagate_uncertainty` is not logical", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(predict_curves(temp = seq(4, 40, 3),
                              dev_rate = rate_sample,
                              fitted_parameters = fitted_params_example,
                              model_name_2boot = "lactin2",
                              propagate_uncertainty = sample(c("bootstrap", "yes"), 1),
                              n_boots_samples = 100),
               "`propagate_uncertainty` must be `TRUE` or `FALSE` (def. `TRUE`)",
               fixed = TRUE)
})


## few samples for bootstrap yields a warning
test_that("predict_curves should issue a warning if `n_boots_samples` < 100", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
  capt_warnings <- capture_warnings(predict_curves(temp = seq(4, 40, 3),
                                                   dev_rate = rate_sample,
                                                   fitted_parameters = fitted_params_example,
                                                   model_name_2boot = "lactin1",
                                                   propagate_uncertainty = TRUE,
                                                   n_boots_samples = 2))
expect_true(any(capt_warnings == "100 iterations might be desirable. Consider increasing `n_boots_samples` if possible"))
})


## no bootstrap accomplished yields a warning (II)


test_that("predict_curves should issue a warning if no boostrap is accomplished", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
  aphid_parameters <- fit_devmodels(temp = aphid$temperature,
                                    dev_rate = aphid$rate_value,
                                    model_name = "all")
  expect_error(capture_warnings(predicted_curves <- predict_curves(temp = aphid$temperature,
                                                                   dev_rate = aphid$rate_value,
                                                                   fitted_parameters = aphid_parameters |>
                                                                     dplyr::mutate(param_est = purrr::map_dbl(.x = param_est,
                                                                                                              .f = ~.x*10)), # <- alterate data to ensure not bootstrapping is performed
                                                                   model_name_2boot = "lactin1", # <- previously known as not adequately converging
                                                                   propagate_uncertainty = TRUE,
                                                                   n_boots_samples = 10)),
  "Bootstrapping failed for all the models provided in `model_name_2boot` due to convergence problems.
         You may try other models fitted with `fit_devmodels()`. If this error persists after attempting all the
         models obtained from `fit_devmodels()`,your data may not be appropriate for
         projecting risk of pest occurrence with the models you have fitted.",
  fixed = TRUE)
})


test_that("predict_curves output should be a tibble with some uncertainty curves", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
  preds_tbl <- suppressWarnings(predict_curves(temp = seq(4, 40, 3),
                                               dev_rate = rate_sample,
                                               fitted_parameters = fitted_params_example,
                                               model_name_2boot = unique(fitted_params_example$model_name)[1], # <- boatman is able to bootstrap
                                               propagate_uncertainty = TRUE,
                                               n_boots_samples = 5))
  expect_true(any(preds_tbl$curvetype == "uncertainty"))
})

test_that("predict_curves output should be a data.frame", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_sample <- readRDS(file = test_path("testdata", "rate_test.rds"))
  preds_tbl <- suppressWarnings(predict_curves(temp = seq(4, 40, 3),
                                               dev_rate = rate_sample,
                                               fitted_parameters = fitted_params_example,
                                               model_name_2boot = unique(fitted_params_example$model_name)[2],
                                               propagate_uncertainty = TRUE,
                                               n_boots_samples = 5))
  expect_true(is.data.frame(preds_tbl))
})

