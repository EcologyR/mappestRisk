# Test input data types
test_that("plot_devmodels should throw an error if temperature data is not numeric", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(plot_devmodels(temp = as.factor(seq(4, 40, 3)),
                               dev_rate = rate_test,
                               fitted_parameters = fitted_params_example),
               "temperature data is not numeric. Please check it.")
})

test_that("plot_devmodels should throw an error if a given `species` argument is not a string", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(plot_devmodels(temp = seq(4, 40, 3),
                              dev_rate = rate_test,
                              fitted_parameters = fitted_params_example,
                              species = 43),
               "`species` must be a character or NULL")
})

test_that("plot_devmodels should throw an error if a given `life_stage` argument is not a string", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(plot_devmodels(temp = seq(4, 40, 3),
                              dev_rate = rate_test,
                              fitted_parameters = fitted_params_example,
                              species = "Brachycaudus schwartzi",
                              life_stage = TRUE),
               "`life_stage` must be a character or NULL")
})

test_that("plot_devmodels should throw an error if dev_rate data is not numeric", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(plot_devmodels(temp = seq(4, 40, 3),
                              dev_rate = as.character(rate_test),
                              fitted_parameters = fitted_params_example),
               "development rate data is not numeric. Please check it.")
})

test_that("plot_devmodels should throw an error if temperature data is a data.frame", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(plot_devmodels(temp = data.frame(temperature = seq(4, 40, 3),
                                               temp_error = runif(13, 0, 2)),
                             dev_rate = rate_test,
                             fitted_parameters = fitted_params_example),
               "temperature data is not numeric. Please check it.")
})

test_that("plot_devmodels should throw an error if temperature and development rate inputs are not of same length", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(plot_devmodels(temp = seq(4, 39, 3),
                             dev_rate = rate_test,
                             fitted_parameters = fitted_params_example),
               "development rate and temperature inputs are not of same length. Please check it.")
})

test_that("plot_devmodels should throw an error if fitted_parameters is not inherited unmodified from `fit_devmodels()`", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(plot_devmodels(temp = seq(4, 40, 3),
                              dev_rate = rate_test,
                              fitted_parameters = fitted_params_example |> dplyr::select(-model_AIC)),
               "The argument `fitted_parameters` must be a tibble or data.frame as produced by `mappestRisk::fit_devmodels()` function. No modifications of columns of the fitted_parameters are allowed, but you can subset observations by filtering or subsetting by rows if desired.",
               fixed=TRUE)
  })

test_that("plot_devmodels should throw an error if fitted_parameters is not given by the user", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(plot_devmodels(temp = seq(4, 40, 3),
                              dev_rate = rate_test),
               "`fitted_parameters` is NULL; use `mappestRisk::fit_devmodels()` to check that at least one model converged",
               fixed=TRUE)
               })

test_that("plot_devmodels should throw an error if fitted_parameters is not a data.frame or tibble`", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(plot_devmodels(temp = seq(4, 40, 3),
                              dev_rate = rate_test,
                              fitted_parameters = list(fitted_params_example)),
               "The argument `fitted_parameters` must be a tibble or data.frame as produced by `mappestRisk::fit_devmodels()` function. No modifications of columns of the fitted_parameters are allowed, but you can subset observations by filtering or subsetting by rows if desired.",
               fixed=TRUE)
})

test_that("plot_devmodels should throw an error if fitted_parameters columns are renamed from `fit_devmodels()`", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(plot_devmodels(temp = seq(4, 40, 3),
                              dev_rate = rate_test,
                              fitted_parameters = fitted_params_example |>
                                dplyr::rename(aic = model_AIC)))
})

test_that("no error happens when filtering or subsetting only by rows and at least one model is left in the data.frame", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_no_error(plot_devmodels(temp = seq(4, 40, 3),
                                 dev_rate = rate_test,
                                 fitted_parameters = fitted_params_example |>
                                 dplyr::filter(model_name %in% unique(fitted_params_example$model_name)[1:3]))
                  )
                  })

test_that("plot_devmodels should throw an error if fitted_parameters is not inherited from `fit_devmodels()`", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  expect_error(plot_devmodels(temp = seq(4, 40, 3),
                              dev_rate = rate_test,
                              fitted_parameters = data.frame(model_name = "briere1",
                                                             ctmin_est = 8,
                                                             ctmin_se = 0.23,
                                                             model_AIC = -67.32)))
})



# test if output is a ggplot object
test_that("plot_devmodels() outputs a ggplot object",{
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  example_plotdevs <- plot_devmodels(temp = seq(4, 40, 3),
                                     dev_rate = rate_test,
                                     fitted_parameters = fitted_params_example)
  expect_true(class(example_plotdevs)[2] == "ggplot")
})

# test that if only one model has converged, the plot works
test_that("a single model converging does also allow a ggplot with one facet", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  fitted_params_one <- fitted_params_example |>
    dplyr::filter(model_name == "mod_polynomial")
  expect_no_error(plot_devmodels(temp = seq(4, 40, 3),
                                 dev_rate = rate_test,
                                 fitted_parameters = fitted_params_one))
})

# test that no model converge returns an error.
test_that("an empty tibble -that should not be returned anyways from fit_devmodels()- returns an error ", {
  fitted_params_example <- readRDS(file = test_path("testdata", "fitted_params_tbl.rds"))
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  fitted_params_null <- fitted_params_example |>
    dplyr::filter(model_name == "briere1") # <- empty tibble
  expect_error(plot_devmodels(temp = seq(4, 40, 3),
                              dev_rate = rate_test,
                              fitted_parameters = fitted_params_null))
})

