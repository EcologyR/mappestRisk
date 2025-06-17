# 1. Fitted parameters table ----------------------------------------------
set.seed(2025)
rate_test <- rnorm(13, mean = 0.02, sd = 0.005)
fitted_params_example <- fit_devmodels(temp = seq(4, 40, 3),
                                       dev_rate = rate_test,
                                       model_name = "all")
set.seed(NULL)
readr::write_rds(fitted_params_example, file = "tests/testthat/testdata/fitted_params_tbl.rds")
readr::write_rds(rate_test, file = "tests/testthat/testdata/rate_test.rds")

# 2. Bootstrap predictions ----------------------------------------------
fitted_tpcs_aphid <- fit_devmodels(temp = aphid$temperature,
                                   dev_rate = aphid$rate_value,
                                   model_name = "all")
plot_devmodels(temp = aphid$temperature,
               dev_rate = aphid$rate_value,
               fitted_parameters = fitted_tpcs_aphid,
               species = "Brachycaudus schwartzi",
               life_stage = "Nymphs")
set.seed(2025)
boots_params_example <- predict_curves(temp = aphid$temperature,
                                       dev_rate = aphid$rate_value,
                                       fitted_parameters = fitted_tpcs_aphid,
                                       model_name_2boot = "joehnk",
                                       propagate_uncertainty = TRUE,
                                       n_boots_samples = 100)
 set.seed(NULL)
readr::write_rds(boots_params_example, file = "tests/testthat/testdata/boots_params_tbl.rds")
