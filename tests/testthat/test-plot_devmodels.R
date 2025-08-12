data("aphid")

tpcs <- fit_devmodels(temp = aphid$temperature,
                      dev_rate = aphid$rate_value,
                      model_name = c("lactin2", "briere2", "mod_weibull")
)


test_that("plot_devmodels should throw an error if a given `species` argument is not a string", {

  expect_error(plot_devmodels(temp = aphid$temperature,
                              dev_rate = aphid$rate_value,
                              fitted_parameters = tpcs,
                              species = 43),
               "`species` must be a character or NULL")
})

test_that("plot_devmodels should throw an error if a given `life_stage` argument is not a string", {

  expect_error(plot_devmodels(temp = aphid$temperature,
                              dev_rate = aphid$rate_value,
                              fitted_parameters = tpcs,
                              species = "Brachycaudus schwartzi",
                              life_stage = TRUE),
               "`life_stage` must be a character or NULL")
})




test_that("plot_devmodels should throw an error if fitted_parameters is not unmodified from `fit_devmodels()`", {

  expect_error(plot_devmodels(temp = aphid$temperature,
                              dev_rate = aphid$rate_value,
                              fitted_parameters = tpcs |> dplyr::select(-model_AIC)),
               "The argument `fitted_parameters` must be a tibble or data.frame as produced by `mappestRisk::fit_devmodels()` function. No modifications of columns of the fitted_parameters are allowed, but you can subset observations by filtering or subsetting by rows if desired.",
               fixed = TRUE)
})

test_that("plot_devmodels should throw an error if fitted_parameters is not given by the user", {

  expect_error(plot_devmodels(temp = aphid$temperature,
                              dev_rate = aphid$rate_value),
               "`fitted_parameters` is NULL; use `mappestRisk::fit_devmodels()` to check that at least one model converged",
               fixed = TRUE)
})

test_that("plot_devmodels should throw an error if fitted_parameters is not a data.frame or tibble`", {

  expect_error(plot_devmodels(temp = aphid$temperature,
                              dev_rate = aphid$rate_value,
                              fitted_parameters = list(tpcs)),
               "The argument `fitted_parameters` must be a tibble or data.frame as produced by `mappestRisk::fit_devmodels()` function. No modifications of columns of the fitted_parameters are allowed, but you can subset observations by filtering or subsetting by rows if desired.",
               fixed = TRUE)
})

test_that("plot_devmodels should throw an error if fitted_parameters columns are renamed from `fit_devmodels()`", {

  expect_error(plot_devmodels(temp = aphid$temperature,
                              dev_rate = aphid$rate_value,
                              fitted_parameters = tpcs |>
                                dplyr::rename(aic = model_AIC)))
})

test_that("no error happens when filtering or subsetting only by rows and at least one model is left in the data.frame", {
  expect_no_error(plot_devmodels(temp = aphid$temperature,
                                 dev_rate = aphid$rate_value,
                                 fitted_parameters = tpcs |>
                                   dplyr::filter(model_name == "lactin2"))
  )
})

test_that("plot_devmodels should throw an error if fitted_parameters is taken from `fit_devmodels()`", {

  expect_error(plot_devmodels(temp = aphid$temperature,
                              dev_rate = aphid$rate_value,
                              fitted_parameters = data.frame(model_name = "briere1",
                                                             ctmin_est = 8,
                                                             ctmin_se = 0.23,
                                                             model_AIC = -67.32)))
})



# test if output is a ggplot object
test_that("plot_devmodels() outputs a ggplot object",{

  example_plotdevs <- plot_devmodels(temp = aphid$temperature,
                                     dev_rate = aphid$rate_value,
                                     fitted_parameters = tpcs)
  expect_true(inherits(example_plotdevs, "ggplot"))
})

# test that if only one model has converged, the plot works
test_that("a single model converging does also allow a ggplot with one facet", {

  fitted_params_one <- tpcs |>
    dplyr::filter(model_name == "lactin2")
  expect_no_error(plot_devmodels(temp = aphid$temperature,
                                 dev_rate = aphid$rate_value,
                                 fitted_parameters = fitted_params_one))
})

# test that no model converge returns an error.
test_that("an empty tibble -that should not be returned anyways from fit_devmodels()- returns an error ", {

  fitted_params_null <- tpcs |>
    dplyr::filter(model_name == "briere1") # <- empty tibble
  expect_error(plot_devmodels(temp = aphid$temperature,
                              dev_rate = aphid$rate_value,
                              fitted_parameters = fitted_params_null))
})

