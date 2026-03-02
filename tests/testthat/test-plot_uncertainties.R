
data("aphid")

set.seed(2025)

tpcs <- fit_devmodels(temp = aphid$temperature,
                      dev_rate = aphid$rate_value,
                      model_name = c("lactin2", "briere2", "mod_weibull")
)

curves <- suppressWarnings(
  predict_curves(temp = aphid$temperature,
                 dev_rate = aphid$rate_value,
                 fitted_parameters = tpcs,
                 model_name_2boot = c("lactin2", "briere2"),
                 propagate_uncertainty = TRUE,
                 n_boots_samples = 2))






test_that("plot_uncertainties should throw an error if a given `species` argument is not a string", {

  expect_error(plot_uncertainties(bootstrap_tpcs = curves,
                                  temp = aphid$temperature,
                                  dev_rate = aphid$rate_value,
                                  species = as.factor("Brachycaudus schwartzi"),
                                  life_stage = "Nymphs"),
               "`species` must be a character or `NULL`")
})

test_that("plot_uncertainties should throw an error if a given `life stage` argument is not a string", {

  expect_error(plot_uncertainties(bootstrap_tpcs = curves,
                                  temp = aphid$temperature,
                                  dev_rate = aphid$rate_value,
                                  species = "Brachycaudus schwartzi",
                                  life_stage = 23),
               "`life_stage` must be a character or `NULL`")
})



test_that("plot_uncertainties should throw an error if `bootstrap_tpcs` is not a data.frame", {

  expect_error(plot_uncertainties(bootstrap_tpcs = as.matrix(curves),
                                  temp = aphid$temperature,
                                  dev_rate = aphid$rate_value,
                                  species = "Brachycaudus schwartzi",
                                  life_stage = "Nymphs"),
               "`bootstrap_tpcs` must be a  `data.frame` or `tibble`
    as produced by `mappestRisk::predict_curves()` function with
    `propagate_uncertainty = TRUE` and `n_boots_samples > 0`.",
               fixed = TRUE)
})

test_that("plot_uncertainties should throw an error if `bootstrap_tpcs` data.frame
          structure has been modified from the output of `predict_curves()`", {

            expect_error(plot_uncertainties(bootstrap_tpcs = curves |>
                                              dplyr::select(-boot_iter),
                                            temp = aphid$temperature,
                                            dev_rate = aphid$rate_value,
                                            species = "Brachycaudus schwartzi",
                                            life_stage = "Nymphs"),
                         "`bootstrap_tpcs` must be a  `data.frame` or `tibble`
    as produced by `mappestRisk::predict_curves()` function with
    `propagate_uncertainty = TRUE` and `n_boots_samples > 0`.",
                         fixed = TRUE)
          })

test_that("plot_uncertainties should throw a warning if `bootstrap_tpcs` data.frame
          contains no uncertainty predictions", {

            expect_warning(plot_uncertainties(bootstrap_tpcs = curves |>
                                                dplyr::filter(curvetype != "uncertainty"),
                                              temp = aphid$temperature,
                                              dev_rate = aphid$rate_value,
                                              species = "Brachycaudus schwartzi",
                                              life_stage = "Nymphs"),
                           "No bootstrapped predictions available. Please check `bootstrap_tpcs`.
             Plotting only the central curve.",
                           fixed = TRUE)
          })

test_that("plot_uncertainties should throw an error if `bootstrap_tpcs` data.frame
          contains zero rows", {

            expect_error(plot_uncertainties(bootstrap_tpcs = curves |> dplyr::slice(0),
                                            temp = aphid$temperature,
                                            dev_rate = aphid$rate_value,
                                            species = "Brachycaudus schwartzi",
                                            life_stage = "Nymphs"),
                         "No bootstrapped or estimate predictions are available.
         Please check `bootstrap_tpcs` and consider using a different model or
         setting `propagate_uncertainty` to `FALSE` in `predict_curves()",
                         fixed = TRUE)
          })

# test if output is a ggplot object
test_that("plot_uncertainties() outputs a ggplot object",{

  example_plotdevs_uncertainty <- plot_uncertainties(bootstrap_tpcs = curves,
                                                     temp = aphid$temperature,
                                                     dev_rate = aphid$rate_value,
                                                     species = "Brachycaudus schwartzi",
                                                     life_stage = "Nymphs")
  expect_true(inherits(example_plotdevs_uncertainty, "ggplot"))
})

