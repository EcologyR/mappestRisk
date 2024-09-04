## first use a data of example
set.seed(2024)
temp_test <- seq(4, 40, 3)
dev_rate_test <- rnorm(13, mean = 0.02, sd = 0.005)

fitted_params_example <- fit_devmodels(temp = temp_test,
                                       dev_rate = dev_rate_test,
                                       model_name = "all")

boots_params_example <- predict_curves(temp = temp_test,
                                       dev_rate = dev_rate_test,
                                       fitted_parameters = fitted_params_example,
                                       model_name_2boot = unique(fitted_params_example$model_name)[3],
                                       propagate_uncertainty = TRUE,
                                       n_boots_samples = 100)

# Test input data types
test_that("plot_devmodels should throw an error if temperature data is not numeric", {
  expect_error(plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example,
                                  temp = as.data.frame(temp_test),
                                  dev_rate = dev_rate_test,
                                  species = "Brachycaudus schwartzi",
                                  life_stage = "Nymphs"),
               "temperature data is not numeric. Please check it.")
})

test_that("plot_uncertainties should throw an error if a given `species` argument is not a string", {
  expect_error(plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example,
                                  temp = temp_test,
                                  dev_rate = dev_rate_test,
                                  species = as.factor("Brachycaudus schwartzi"),
                                  life_stage = "Nymphs"),
               "`species` must be a character or `NULL`")
})

test_that("plot_uncertainties should throw an error if a given `life stage` argument is not a string", {
  expect_error(plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example,
                                  temp = temp_test,
                                  dev_rate = dev_rate_test,
                                  species = "Brachycaudus schwartzi",
                                  life_stage = 23),
               "`life_stage` must be a character or `NULL`")
})

test_that("plot_uncertainties should throw an error if dev_rate data is not numeric", {
  expect_error(plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example,
                                  temp = temp_test,
                                  dev_rate = as.logical(dev_rate_test),
                                  species = "Brachycaudus schwartzi",
                                  life_stage = "Nymphs"),
               "development rate data is not numeric. Please check it.")
})

test_that("plot_uncertainties should throw an error if temperature and development rate inputs are not of same length", {
  expect_error(plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example,
                                  temp = sample(temp_test, 5),
                                  dev_rate = sample(dev_rate_test, 6),
                                  species = "Brachycaudus schwartzi",
                                  life_stage = "Nymphs"),
               "development rate and temperature inputs are not of same length. Please check it.")
})

test_that("plot_uncertainties should throw an error if `bootstrap_uncertainties_tpcs` is not a data.frame", {
  expect_error(plot_uncertainties(bootstrap_uncertainties_tpcs = as.matrix(boots_params_example),
                                  temp = temp_test,
                                  dev_rate = dev_rate_test,
                                  species = "Brachycaudus schwartzi",
                                  life_stage = "Nymphs"),
               "`bootstrap_uncertainties_tpcs` must be a  `data.frame` or `tibble`
    inherited from the output of `mappestRisk::predict_curves()` function with
    `propagate_uncertainty = TRUE` and `n_boots_samples > 0`.",
               fixed = TRUE)
})

test_that("plot_uncertainties should throw an error if `bootstrap_uncertainties_tpcs` data.frame
          structure has been modified from the output of `predict_curves()`", {
  expect_error(plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example |>
                                    dplyr::select(-iter),
                                  temp = temp_test,
                                  dev_rate = dev_rate_test,
                                  species = "Brachycaudus schwartzi",
                                  life_stage = "Nymphs"),
               "`bootstrap_uncertainties_tpcs` must be a  `data.frame` or `tibble` inherited from the
    output of `mappestRisk::predict_curves()` function with
    `propagate_uncertainty = TRUE` and `n_boots_samples > 0`.",
               fixed = TRUE)
})

test_that("plot_uncertainties should throw an error if `bootstrap_uncertainties_tpcs` data.frame
          contains no uncertainty predictions", {
            expect_warning(plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example |>
                                              dplyr::filter(curvetype != "uncertainty"),
                                              temp = temp_test,
                                              dev_rate = dev_rate_test,
                                              species = "Brachycaudus schwartzi",
                                              life_stage = "Nymphs"),
                         "No bootstrapped predictions available. Please check `bootstrap_uncertainties_tpcs`.
             Plotting only the central curve.",
                         fixed = TRUE)
})

test_that("plot_uncertainties should throw an error if `bootstrap_uncertainties_tpcs` data.frame
          contains zero rows", {
            expect_error(plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example |> dplyr::slice(0),
                                              temp = temp_test,
                                              dev_rate = dev_rate_test,
                                              species = "Brachycaudus schwartzi",
                                              life_stage = "Nymphs"),
                           "No bootstrapped or estimate predictions are available.
         Please check `bootstrap_uncertainties_tpcs` and consider using a different model or
         setting `propagate_uncertainty` to `FALSE` in `predict_curves()",
                           fixed = TRUE)
})

# test if output is a ggplot object
test_that("plot_uncertainties() outputs a ggplot object",{
  example_plotdevs_uncertainty <- plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example,
                                         temp = temp_test,
                                         dev_rate = dev_rate_test,
                                         species = "Brachycaudus schwartzi",
                                         life_stage = "Nymphs")
  expect_true(class(example_plotdevs_uncertainty)[2] == "ggplot")
})
