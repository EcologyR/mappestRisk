library(testthat)
library(mappestRisk)

as.character()
# Test input data types
test_that("fit_devmodels should throw an error if temperature data is not numeric", {
  expect_error(fit_devmodels(temp = as.character(seq(4, 40, 3)),
                             dev_rate = rnorm(12, mean = 0.02, sd = 0.005),
                             model_name = "all",
                             variance_model = "exp"))
})

test_that("fit_devmodels should throw an error if temperature data is a data.frame", {
  expect_error(fit_devmodels(temp = data.frame(temperature = seq(4, 40, 3),
                                               temp_error = runif(13, 0, 2)),
                             dev_rate = rnorm(12, mean = 0.02, sd = 0.005),
                             model_name = "all",
                             variance_model = "exp"))
})

test_that("fit_devmodels should throw an error if temperature data have just three values", {
  expect_error(fit_devmodels(temp = as.character(seq(4, 40, 3)),
                             dev_rate = rnorm(12, mean = 0.02, sd = 0.005),
                             model_name = "all",
                             variance_model = "exp"))
})

test_that("fit_devmodels should throw an error if development rate data is not numeric
          (e.g. incorrectly importing data from csv/xlsx, using commas as decimal markers, etc)", {
  expect_error(fit_devmodels(temp = rep(seq(10,30,10), each = 5),
                             dev_rate = as.character(rnorm(15, mean = 0.02, sd = 0.005)),
                             model_name = "all",
                             variance_model = "exp"))
})

test_that("fit_devmodels should throw an error if temperature and development rate inputs are not of same length", {
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = rnorm(12, mean = 0.02, sd = 0.005),
                             model_name = "all",
                             variance_model = "exp"))
})

test_that("fit_devmodels should throw an error if model_name is not a character string but a R object", {
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                             model_name = briere2,
                             variance_model = "exp"))
})

test_that("fit_devmodels should throw an error if model_name is not a character string but a number", {
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                             model_name = 2,
                             variance_model = "exp"))
})

test_that("fit_devmodels should throw an error if model_name is not in available_models", {
  expect_error(fit_devmodels(temp = seq(4, 40, 3),
                             dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                             model_name = "SharpeDeMichele",
                             variance_model = "exp"))
})

# Test input data ranges and warnings
test_that("fit_devmodels should print a warning if development rate data contains negative
          values or higher than 10", {
  set.seed(2023) # <- ensure a seed with any negative number and any model converging to test exactly this warning
  expect_warning(fit_devmodels(temp = seq(4, 40, 3),
                               dev_rate = rnorm(13, mean = 0.01, sd = 0.01),
                               model_name = "all",
                               variance_model = "exp"))
})

test_that("fit_devmodels should issue a warning if temperature data contains values outside of the range of active organisms", {
  expect_warning(capture_error(fit_devmodels(temp = c(seq(4, 39, 3), 4000),
                                             dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                                             model_name = "all",
                                             variance_model = "exp")))
})

# Test output object
test_that("fit_devmodels should return a list object", {
  expect_type(fit_devmodels(temp = c(seq(4, 40, 3)),
                          dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                          model_name = "all",
                          variance_model = "exp"),
              type = "list")
})

# test that function works without any value of variance_model printing a warning
test_that("fit_devmodels should incorporate by default
          variance_model exp showing a warning", {
  expect_warning(fit_devmodels(temp = c(seq(4, 40, 3)),
                            dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                            model_name = "all"))})

# test that function stops if varFunction is not an available option
test_that("fit_devmodels should incorporate by default
          variance_model exp showing a warning", {
            expect_error(fit_devmodels(temp = c(seq(4, 40, 3)),
                                       dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                                       model_name = "all",
                                       variance_model = "none"))
            })

test_that("fit_devmodels should incorporate by default
          variance_model exp showing a warning", {
            expect_error(fit_devmodels(temp = c(seq(4, 40, 3)),
                                       dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                                       model_name = "all",
                                       variance_model = 3))
          })

## test that rate of development data have NAs
test_that("fit_devmodels should stop the function and advise the user about NAs in the data set", {
            expect_error(fit_devmodels(temp = c(seq(4, 40, 3)),
                                       dev_rate = c(rnorm(12, mean = 0.02, sd = 0.005), NA),
                                       model_name = "all",
                                       variance_model = "exp"))
          })

## test that temperature data have NAs
test_that("fit_devmodels should stop the function and advise the user about NAs in the data set", {
  expect_error(fit_devmodels(temp = c(seq(4, 40, 3), NA),
                             dev_rate = c(rnorm(14, mean = 0.02, sd = 0.005)),
                             model_name = "all",
                             variance_model = "exp"))
})

## test extreme data sets with no convergence gives an error saying no model fitted to the data
test_that("fit_devmodels can deal with no convergence at all", {
  expect_error(suppressWarnings(fit_devmodels(temp = seq(4, 40, 7),
                                dev_rate = runif(6, min = -234, max = 101),
                                model_name = "all",
                                variance_model = "exp")))
})

## test that a fitted model is accessible through the fitted_params tbl
test_that("gnls object is retrieved and of correct class", {
  fitted_parameters <-fit_devmodels(temp = seq(4, 40, 3),
                            dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                            model_name = "all",
                            variance_model = "exp")
  expect_true(all(class(fitted_parameters$model_fit[[1]])[1] == "gnls"))

})

## test that a fitted model summary is accessible
test_that("gnls object and its summary and table of parameters and statistics are correct", {
  suppressWarnings(fitted_parameters <-fit_devmodels(temp = seq(4, 40, 3),
                                    dev_rate = rnorm(13, mean = 0.12, sd = 0.1),
                                    model_name = "all",
                                    variance_model = "exp"))
  sum_fitted <- summary(fitted_parameters$model_fit[[1]])
  expect_true(all(colnames(sum_fitted$tTable) == c("Value", "Std.Error", "t-value", "p-value")))

})

## test that no false convergence occurs
test_that("false convergence (i.e. start_vals == param_est) is excluded from fitted_params", {
  number_of_falseconverg <- vector("list", length = 20)
  sample_seeds_random <- sample(1000, 20, replace = FALSE)
  for(seed in sample_seeds_random) {
    set.seed(seed)
    suppressWarnings(fitted_parameters <-fit_devmodels(temp = seq(4, 40, 3),
                                     dev_rate = rnorm(13, mean = 0.12, sd = 0.01), # this combination ensures that model converge and we can test this concrete thing
                                     model_name = "all",
                                     variance_model = "exp"))
    detect_false_convergence <- fitted_parameters |>
      dplyr::mutate(bad_convergence = purrr::map2_lgl(.x = start_vals,
                                               .y = param_est,
                                               .f = ~dplyr::if_else(.x == .y,
                                                             TRUE,
                                                             FALSE
                                               )))
    n_false_conv_seed <- length(which(detect_false_convergence$bad_convergence == TRUE))
    number_of_falseconverg[which(sample_seeds_random == seed)] <- n_false_conv_seed
  }
  total_false_conv <- purrr::pmap(number_of_falseconverg, sum)
  expect_true(total_false_conv == 0)
})
