library(mappestRisk)
library(dplyr)
suppressWarnings(library(ggplot2))
library(testthat)
## first use a data of example
temp_example_typical <- c(12, 15, 18, 21, 24, 27, 30, 33)
dev_rate_example_typical <- c(0.02, 0.045, 0.092, 0.113, 0.154, 0.187, 0.123, 0.001) # to approach a curve
# plot(temp_example_typical, dev_rate_example_typical)

fitted_params_example <- fit_devmodels(temp = temp_example_typical,
                                       dev_rate = dev_rate_example_typical,
                                       model_name = "all",
                                       variance_model = "exp")
plot_devmodels(temp = temp_example_typical,
               dev_rate = dev_rate_example_typical,
               fitted_parameters = fitted_params_example) # <- to see model appearance
# 1. test inputs ----

## test error if model name is not in the list `dev_model_table`
test_that("Error is thrown for invalid model name from list of available models", {
  expect_error(thermal_suitability_bounds(fitted_params_example,
                             model_name =  "Sharpe&DeMichele", # <- not in dev_model_table
                             suitability_threshold = 75),
               "Model name not available. For available model names, see `dev_model_table`.")
})

## test error if model name is in the list `dev_model_table` but not in the fitted, converged models.
test_that("Error is thrown for invalid model name not in fitted parameters table", {
  expect_error(thermal_suitability_bounds(fitted_params_example,
                                          model_name =  "lactin2", # <- not in fitted_params_example, but in dev_model_table
                                          suitability_threshold = 75),
               "Model lactin2 did not fitted well to your data. Try using another fitted model in your table instead")
})

## test error if suitability threshold is less than 50 (too broad curve)
test_that("Error is thrown for suitability threshold less than 50", {
  expect_error(thermal_suitability_bounds(fitted_params_example,
                                          model_name =  "ratkowsky", # <- "okay" convergence in the example
                                          suitability_threshold = 25),
               "Suitability must be higher than 50% in order to have applied sense. If set to NULL, suitability_threshold = 50% by default")
})

## test error if fitted linear model is provided by the user
test_that("Only nonlinear fitted models are allowed", {
  expect_error(thermal_suitability_bounds(fitted_params_example,
                                          model_name =  "linear_campbell", # <- "okay" convergence in the example
                                          suitability_threshold = 75),
               "Thermal Suitability predictions require nonlinear models. Try another fitted model in your `fitted_parameters` table instead")
})

## test error if fitted linear model is provided by the user
test_that("Only nonlinear fitted models are allowed", {
  expect_error(thermal_suitability_bounds(fitted_params_example,
                                          model_name =  "linear_campbell", # <- "okay" convergence in the example
                                          suitability_threshold = 75),
               "Thermal Suitability predictions require nonlinear models. Try another fitted model in your `fitted_parameters` table instead")
})

## test error if fitted linear model is provided by the user
test_that("Only nonlinear fitted models are allowed", {
  expect_error(thermal_suitability_bounds(fitted_params_example,
                                          model_name =  "linear_campbell", # <- "okay" convergence in the example
                                          suitability_threshold = 75),
               "Thermal Suitability predictions require nonlinear models. Try another fitted model in your `fitted_parameters` table instead")
})

## test error if data frame is empty (as would happen if the user filters the `fitted_parameters` with no returning rows)
test_that("no empty data.frame is allowed, since fit_devmodels() prevents it with an error", {
  fitted_params_empty <- fitted_params_example |>
    dplyr::filter(model_name %in% c("lactin2", "rezende"))
  expect_error(thermal_suitability_bounds(fitted_parameters = fitted_params_empty,
                                            model_name = "lactin2", # <- to ensure no error overlapping and mishierarchical behavior
                                            suitability_threshold = 75),
                 "The fitted_parameters table is NULL; use `fit_devmodels()` to check that at least one model converged.",
               fixed = TRUE) # <- since the message includes a regular expression (NULL)
    })

## test error if data frame is not directly inherited -without column modifications- from `fit_devmodels()`)
test_that("only a data.frame with same columns as those returned by `fit_devmodels()`  is allowed", {
  fitted_params_column_modified <- fitted_params_example |>
    dplyr::select(-model_fit, -start_vals)
  expect_error(thermal_suitability_bounds(fitted_parameters = fitted_params_column_modified,
                                          model_name = "ratkowsky",
                                          suitability_threshold = 75),
               "`fitted_parameters` must be a  data.frame inherited   from the output of `mappestRisk::fit_devmodels()` function.
  No modifications of columns of the fitted_parameters data.frame are allowed, but you can subset observations by filter(ing
  or subsetting by rows if desired.",
               fixed = TRUE) # <- since the message includes a regular expression (::)
})

## test no error emerges if user adds new columns to `fitted_params` without modifying the existing ones
test_that("only a data.frame with at least the column identities returned by `fit_devmodels()` is allowed", {
  fitted_params_column_new <- fitted_params_example |>
    dplyr::mutate(species = "Aglais io")
  expect_no_error(thermal_suitability_bounds(fitted_parameters = fitted_params_column_new,
                                          model_name = "ratkowsky",
                                          suitability_threshold = 75)
                  ) #
})


## test that a message is printed when suitability_threshold = NULL
test_that("a message is printed of default `suitability_threshold = 50` if the user does not provide any value ", {
  expect_message(thermal_suitability_bounds(fitted_parameters = fitted_params_example,
                                             model_name = "ratkowsky"),
                 "No suitability_threshold value input. Using by default suitability_threshold = 50%")
})

## test that suitability_threshold is actually 50 when suitability_threshold = NULL by the user
test_that("function is using a def. suitability_threshold = 50 if the argument is empty", {
  output_tvals <- thermal_suitability_bounds(fitted_parameters = fitted_params_example,
                                             model_name = "ratkowsky")
  expect_equal(readr::parse_number(output_tvals$suitability), 50)
})

# 2. test outputs ----
# error if tvals are non-realistic

## test that it deals with bad convergence at any or both sides of the TPC
temp_example_typical <- c(12, 15, 18, 21, 24, 27, 30, 33)
dev_rate_example_broad_v2 <- c(0.112, 0.110, 0.115, 0.113, 0.124, 0.129, 0.052, 0.001)
fitted_params_broad_left <- fit_devmodels(temp = temp_example_typical,
                                     dev_rate = dev_rate_example_broad_v2,
                                     model_name = "all",
                                     variance_model = "exp")
plot_devmodels(temp = temp_example_typical,
               dev_rate = dev_rate_example_broad_v2,
               fitted_parameters = fitted_params_broad_left) ## try it with lactin2 (1) and with mod_gaussian (2)

## test that unrealistic thermal values for boundaries are warned by bad fitting at the right side)
test_that("a warning is printed if thermal boundaries cannot be calculated, returning NA", {
  expect_warning(test_nobounds <- thermal_suitability_bounds(fitted_parameters = fitted_params_broad_left,
                                                             model_name = "lactin2",
                                                             suitability_threshold = 50),
                 "Model lactin2 is not appropriate to model thermal suitability. Try another instead (use `plot_devmodel()` to see curve shapes).",
                 fixed = TRUE)
  expect_equal(test_nobounds$tval_left[1], NA)
  expect_equal(test_nobounds$tval_right[1], NA)
})

## test that unrealistic thermal values for boundaries are warned by bad fitting at both sides)
test_that("a warning is printed if thermal boundaries cannot be calculated, returning NA", {
  expect_warning(test_nobounds <- thermal_suitability_bounds(fitted_parameters = fitted_params_broad_left,
                                                             model_name = "mod_gaussian",
                                                             suitability_threshold = 50),
                 "Model mod_gaussian is not appropriate to model thermal suitability. Try another instead (use `plot_devmodel()` to see curve shapes).",
                 fixed = TRUE)
  expect_equal(test_nobounds$tval_left[1], NA)
  expect_equal(test_nobounds$tval_right[1], NA)
})

## test that unrealistic thermal values for boundaries are warnd by bad fitting at the right tail
dev_rate_example_broad_right <- c(0.02, 0.045, 0.092, 0.113, 0.154, 0.187, 0.183, 0.180)
fitted_params_broad_right <- fit_devmodels(temp = temp_example_typical,
                                          dev_rate = dev_rate_example_broad_right,
                                          model_name = "all",
                                          variance_model = "exp")
plot_devmodels(temp = temp_example_typical,
               dev_rate = dev_rate_example_broad_right,
               fitted_parameters = fitted_params_broad_right) # use lactin1

test_that("a warning is printed if thermal boundaries cannot be calculated, returning NA", {
  expect_warning(test_nobounds <- thermal_suitability_bounds(fitted_parameters = fitted_params_broad_right,
                                                             model_name = "lactin1",
                                                             suitability_threshold = 50),
                 "Model lactin1 is not appropriate to model thermal suitability. Try another instead (use `plot_devmodel()` to see curve shapes).",
                 fixed = TRUE)
  expect_equal(test_nobounds$tval_left[1], NA)
  expect_equal(test_nobounds$tval_right[1], NA)
})



## test appropriate that output class is a data.frame or a tibble
test_that("output is a data.frame with model_name, tval_left, tval_right and suitability", {
  boundaries_example <- thermal_suitability_bounds(fitted_parameters = fitted_params_example,
                                                   model_name = "briere1",
                                                   suitability_threshold = 75)
  expect_true(any(class(boundaries_example) %in% c("data.frame", "tbl")))
})

## test column names, since tval_left and tval_right will be inherited later in `map_risk()` function
test_that("Returned object has the correct column names", {
  thermal_bounds_test1 <- thermal_suitability_bounds(fitted_params_example,
                                                     model_name =  "ratkowsky", # <- "okay" convergence in the example
                                                     suitability_threshold = 75)
  expect_equal("model_name", colnames(thermal_bounds_test1)[1])
  expect_equal("tval_left", colnames(thermal_bounds_test1)[2])
  expect_equal("tval_right", colnames(thermal_bounds_test1)[3])
  expect_equal("suitability", colnames(thermal_bounds_test1)[4])
})

## test that multiple values might be obtained iteratively and NAs are not causing problems despite warnings
## let's modify a bit the output to force worse convergence
temp_example_typical <- c(12, 15, 18, 21, 24, 27, 30, 33)
dev_rate_example_broad <- c(0.02, 0.045, 0.092, 0.113, 0.154, 0.187, 0.123, 0.140) # to approach a curve
fitted_params_broad <- fit_devmodels(temp = temp_example_typical,
                                     dev_rate = dev_rate_example_broad,
                                     model_name = "all",
                                     variance_model = "exp")
plot_devmodels(temp = temp_example_typical,
               dev_rate = dev_rate_example_broad,
               fitted_parameters = fitted_params_broad) # look at strange behaviors (mod_polynomial and lactin1)


test_that("multiple values are computed without problems, dealing with non-realistic values with NAs", {
  model_names_itertest <- c("mod_gaussian", "briere1","mod_polynomial", # <- not usable to compute boundaries, should return NA
                            "ratkowsky", "briere2", "lactin2",
                            "lactin1" # <- not usable to compute boundaries, should return NA
                            )
  expected_warnings <- c(
    "Model mod_polynomial is not appropriate to model thermal suitability. Try another instead (use `plot_devmodel()` to see curve shapes).",
    "Model lactin1 is not appropriate to model thermal suitability. Try another instead (use `plot_devmodel()` to see curve shapes)."
    )
  obtained_warnings <- capture_warnings(code = {itertest <- thermal_suitability_bounds(fitted_params_broad,
                                                       model_name =  model_names_itertest, # <- "okay" convergence in the example"
                                                       suitability_threshold = 75)})
  expect_match(expected_warnings[1], obtained_warnings[1], fixed = TRUE) # first warning is correctly printed
  expect_match(expected_warnings[2], obtained_warnings[2], fixed = TRUE) # second warning is correctly printed
  expect_equal(nrow(itertest), length(model_names_itertest)) # NAs are not causing problems and data.frame has all rows
})


