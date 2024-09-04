
# Test input data types
test_that("plot_uncertainties should throw an error if temperature data is not numeric", {

  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  boots_params_example <- readRDS(file = test_path("testdata", "boots_params_tbl.rds"))
  temp_test <- seq(4, 40, 3)
  expect_error(plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example,
                                  temp = as.data.frame(temp_test),
                                  dev_rate = rate_test,
                                  species = "Brachycaudus schwartzi",
                                  life_stage = "Nymphs"),
               "temperature data is not numeric. Please check it.")
})

test_that("plot_uncertainties should throw an error if a given `species` argument is not a string", {

  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  boots_params_example <- readRDS(file = test_path("testdata", "boots_params_tbl.rds"))
  temp_test <- seq(4, 40, 3)
  expect_error(plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example,
                                  temp = temp_test,
                                  dev_rate = rate_test,
                                  species = as.factor("Brachycaudus schwartzi"),
                                  life_stage = "Nymphs"),
               "`species` must be a character or `NULL`")
})

test_that("plot_uncertainties should throw an error if a given `life stage` argument is not a string", {

  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  boots_params_example <- readRDS(file = test_path("testdata", "boots_params_tbl.rds"))
  temp_test <- seq(4, 40, 3)
  expect_error(plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example,
                                  temp = temp_test,
                                  dev_rate = rate_test,
                                  species = "Brachycaudus schwartzi",
                                  life_stage = 23),
               "`life_stage` must be a character or `NULL`")
})

test_that("plot_uncertainties should throw an error if dev_rate data is not numeric", {

  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  boots_params_example <- readRDS(file = test_path("testdata", "boots_params_tbl.rds"))
  temp_test <- seq(4, 40, 3)
  expect_error(plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example,
                                  temp = temp_test,
                                  dev_rate = as.logical(rate_test),
                                  species = "Brachycaudus schwartzi",
                                  life_stage = "Nymphs"),
               "development rate data is not numeric. Please check it.")
})

test_that("plot_uncertainties should throw an error if temperature and development rate inputs are not of same length", {

  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  boots_params_example <- readRDS(file = test_path("testdata", "boots_params_tbl.rds"))
  temp_test <- seq(4, 40, 3)
  expect_error(plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example,
                                  temp = sample(temp_test, 5),
                                  dev_rate = sample(rate_test, 6),
                                  species = "Brachycaudus schwartzi",
                                  life_stage = "Nymphs"),
               "development rate and temperature inputs are not of same length. Please check it.")
})

test_that("plot_uncertainties should throw an error if `bootstrap_uncertainties_tpcs` is not a data.frame", {
  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  boots_params_example <- readRDS(file = test_path("testdata", "boots_params_tbl.rds"))
  temp_test <- seq(4, 40, 3)
  expect_error(plot_uncertainties(bootstrap_uncertainties_tpcs = as.matrix(boots_params_example),
                                  temp = temp_test,
                                  dev_rate = rate_test,
                                  species = "Brachycaudus schwartzi",
                                  life_stage = "Nymphs"),
               "`bootstrap_uncertainties_tpcs` must be a  `data.frame` or `tibble`
    inherited from the output of `mappestRisk::predict_curves()` function with
    `propagate_uncertainty = TRUE` and `n_boots_samples > 0`.",
               fixed = TRUE)
})

test_that("plot_uncertainties should throw an error if `bootstrap_uncertainties_tpcs` data.frame
          structure has been modified from the output of `predict_curves()`", {
            rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
            boots_params_example <- readRDS(file = test_path("testdata", "boots_params_tbl.rds"))
            temp_test <- seq(4, 40, 3)
            expect_error(plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example |>
                                    dplyr::select(-iter),
                                  temp = temp_test,
                                  dev_rate = rate_test,
                                  species = "Brachycaudus schwartzi",
                                  life_stage = "Nymphs"),
               "`bootstrap_uncertainties_tpcs` must be a  `data.frame` or `tibble` inherited from the
    output of `mappestRisk::predict_curves()` function with
    `propagate_uncertainty = TRUE` and `n_boots_samples > 0`.",
               fixed = TRUE)
})

test_that("plot_uncertainties should throw an error if `bootstrap_uncertainties_tpcs` data.frame
          contains no uncertainty predictions", {
            rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
            boots_params_example <- readRDS(file = test_path("testdata", "boots_params_tbl.rds"))
            temp_test <- seq(4, 40, 3)
            expect_warning(plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example |>
                                              dplyr::filter(curvetype != "uncertainty"),
                                              temp = temp_test,
                                              dev_rate = rate_test,
                                              species = "Brachycaudus schwartzi",
                                              life_stage = "Nymphs"),
                         "No bootstrapped predictions available. Please check `bootstrap_uncertainties_tpcs`.
             Plotting only the central curve.",
                         fixed = TRUE)
})

test_that("plot_uncertainties should throw an error if `bootstrap_uncertainties_tpcs` data.frame
          contains zero rows", {
            rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
            boots_params_example <- readRDS(file = test_path("testdata", "boots_params_tbl.rds"))
            temp_test <- seq(4, 40, 3)
            expect_error(plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example |> dplyr::slice(0),
                                              temp = temp_test,
                                              dev_rate = rate_test,
                                              species = "Brachycaudus schwartzi",
                                              life_stage = "Nymphs"),
                           "No bootstrapped or estimate predictions are available.
         Please check `bootstrap_uncertainties_tpcs` and consider using a different model or
         setting `propagate_uncertainty` to `FALSE` in `predict_curves()",
                           fixed = TRUE)
})

# test if output is a ggplot object
test_that("plot_uncertainties() outputs a ggplot object",{

  rate_test <- readRDS(file = test_path("testdata", "rate_test.rds"))
  boots_params_example <- readRDS(file = test_path("testdata", "boots_params_tbl.rds"))
  temp_test <- seq(4, 40, 3)
  example_plotdevs_uncertainty <- plot_uncertainties(bootstrap_uncertainties_tpcs = boots_params_example,
                                         temp = temp_test,
                                         dev_rate = rate_test,
                                         species = "Brachycaudus schwartzi",
                                         life_stage = "Nymphs")
  expect_true(class(example_plotdevs_uncertainty)[2] == "ggplot")
})

