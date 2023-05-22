library(mappestRisk)
library(dplyr)
library(testthat)
## first use a data of example
set.seed(2023)
fitted_params_example <- fit_devmodels(temp = seq(4, 40, 3),
                                       dev_rate = rnorm(13, mean = 0.02, sd = 0.005),
                                       model_name = "all",
                                       variance_model = "exp")



## test function, since tval_left and tval_right will be inherited later in `map_risk()` function
test_that("Returned object has the correct column names", {
  fitted_parameters <- data.frame(...)
  model_name <- "..."
  suitability_threshold <- 75
  result <- thermal_suitability_bounds(fitted_parameters, model_name, suitability_threshold)
  expect_true("model_name" %in% colnames(result))
  expect_true("tval_left" %in% colnames(result))
  expect_true("tval_right" %in% colnames(result))
})
