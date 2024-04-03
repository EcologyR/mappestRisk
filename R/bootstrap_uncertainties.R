## ex1
rate_temp <- readxl::read_xlsx("~/GitHub/tracing_thermal_niches/data/data_source/therm_ecology_dataset.xlsx") |>
  filter(species == "Chloridea virescens" &
           life_stage == "pupa" &
           rate_type == "development days") |>
  mutate(devrate = 1/rate_value) |>
  select(temperature, devrate) |>
  rename(temp = temperature,
         dev_rate = devrate)

##ex 2 fewer temperature points
rate_temp <- readxl::read_xlsx("~/GitHub/tracing_thermal_niches/data/data_source/therm_ecology_dataset.xlsx") |>
  filter(species == "Neoleucinodes elegantalis" &
           life_stage == "larva V" &
           rate_type == "development days") |>
  mutate(devrate = 1/rate_value) |>
  select(temperature, devrate) |>
  rename(temp = temperature,
         dev_rate = devrate) |>
  drop_na()

##
bootstrap_uncertainties <- function(temp,
                                    dev_rate,
                                    fitted_parameters,
                                    model_name_2boot,
                                    n_boots_samples) {

 devdata <- tibble(temp,
                    dev_rate)
  fitted_tbl <- fitted_parameters
  predict2fill <- tibble(temp = NULL,
                         dev_rate = NULL,
                         model_name = NULL,
                         model_fit = NULL,
                         model_AIC = NULL)


  for(model_name_i in model_name_2boot){
    fitted_tbl_i <- fitted_tbl |>
      filter(model_name == model_name_i)
    model_AIC_i <-fitted_tbl_i |>
      pull(model_AIC)
    model_fit_i <- fitted_tbl_i |>
      pull(model_fit)
    params_i <- fitted_tbl_i |>
      pull(param_est)
    formula_i <- dev_model_table |>
      filter(model_name == model_name_i) |>
      pull(working_formula)
    ##predict based on parameters
    explore_preds <- tibble(temp = seq(min(devdata$temp)-20,
                                       max(devdata$temp) +15,
                                       .1),
                            model_name = model_name_i,
                            model_fit = model_fit_i[1],
                            model_AIC = model_AIC_i[1],
                            preds = NULL,
                            n_params = length(params_i))
    fit_vals_tbl <- explore_preds |>
      dplyr::select(temp, model_name, model_AIC, n_params, model_fit) |>
      mutate(formula = formula_i) |>
      mutate(preds = purrr::map_dbl(.x = temp,
                                    .f = reformulate(unique(formula_i)))) |>
      filter(preds >= -0.2) |>
      dplyr::select(-formula)
    # to exclude biological non-sense predictions due to model mathematical properties
    predict2fill <- predict2fill |>
      bind_rows(fit_vals_tbl)
  }
  predict2fill_complete <- predict2fill |>
    mutate(nest(devdata)) |>
    mutate(coefs = map(.x = model_fit,
                       .f = coef))

  boot_2fill <- tibble(temp = NULL,
                       model_name = NULL,
                       model_AIC = NULL,
                       n_params = NULL,
                       model_fit = NULL,
                       preds = NULL,
                       data = NULL,
                       coefs = NULL,
                       bootstrap = NULL)

  for(model_i in model_name_2boot){
    predict_model_i <- predict2fill_complete |>
      filter(model_name == model_i)
    coefs_i <- as_vector(unique(predict_model_i$coefs))
    temp_data_i <- devdata
    formula_i <- dev_model_table |>
      filter(model_name == model_i) |>
      pull(formula)

    ## avoid errors when wrapping Boot
    assign("model_i", model_i, envir=parent.frame())
    assign("predict_model_i", predict_model_i, envir = parent.frame())
    assign("coefs_i", coefs_i, envir=parent.frame())
    assign("temp_data_i", temp_data_i, envir=parent.frame())
    assign("formula_i", formula_i, envir=parent.frame())
    possible_error <- tryCatch(expr = {
      temp_fit <- minpack.lm::nlsLM(formula = reformulate(response = "dev_rate",
                                                          termlabels = formula_i),
                                    data = temp_data_i,
                                    na.action = na.exclude,
                                    start = coefs_i)
      set.seed(2024)
      assign("temp_fit", temp_fit, envir=parent.frame())
      boot <- Boot(temp_fit, method = 'residual', R = n_boots_samples)
      boot_2fill_i <- predict_model_i |>
        mutate(bootstrap = list(boot))
    }, # <- inside tryCatch
    error = function(e) e)
    if(inherits(possible_error, "error")) {
      temp_fit <- NULL
      boot <- NA
      boot_2fill_i <- predict_model_i |>
        mutate(bootstrap = list(boot))
    }
    boot_2fill <- bind_rows(boot_2fill, boot_2fill_i)
  }
  rm("model_i", "predict_model_i", "coefs_i", "temp_data_i", "formula_i", "temp_fit", "boot")
  boot_2fill_clean <- boot_2fill |>
    filter(!is.na(bootstrap))

  #get the raw values of each bootstrap (the code pipes from here to next object was copied from `rTPC` vignette on bootstrapping curves)
  tpc_fits_boot <- boot_2fill_clean |>
    group_by(model_name) |>
    mutate(output_boot = map(bootstrap,
                             function(x) x$t))
  bootstrap_tpcs_all <- tibble(model_name = NULL,
                               iter = NULL,
                               temp = NULL,
                               pred = NULL,
                               curvetype = NULL)
  #preds boot with a for loop

  for(temp_model_i in 1:length(tpc_fits_boot$output_boot)){
    boot_preds_i <- tpc_fits_boot[temp_model_i,]
    print(paste("Predicting bootstrapped TPCs", round(100*temp_model_i/length(tpc_fits_boot$output_boot), 1), "%"))
    model_name_boot_i <- boot_preds_i$model_name
    boot_coefs_i <- boot_preds_i |>
      unnest_wider(col = coefs)
    formula_i <- dev_model_table |>
      filter(model_name == model_name_boot_i) |>
      pull(working_formula)
    output_boot_df <- as.data.frame(boot_preds_i$output_boot) |>
      mutate(iter = 1:n()) |>
      tibble()

    tpc_bootpreds <- tibble(model_name = NULL,
                            iter = NULL,
                            temp = NULL,
                            pred = NULL,
                            curvetype = NULL)
    for(boot_iter in 1:nrow(output_boot_df)){
      params_i <- output_boot_df[boot_iter, 1:boot_preds_i$n_params] |>
        as_vector()
      tpc_boot_iter_temp <- tibble(model_name = model_name_boot_i,
                                   iter = boot_iter,
                                   temp = boot_preds_i$temp,
                                   pred = map_dbl(.x = temp,
                                                  .f = reformulate(formula_i)),
                                   curvetype = "uncertainty")
      tpc_bootpreds <- bind_rows(tpc_bootpreds, tpc_boot_iter_temp)
    }
    bootstrap_tpcs_all <- bind_rows(bootstrap_tpcs_all, tpc_bootpreds) |>
      filter(pred >= 0)
    central_curve <- tpc_fits_boot |>
      select(temp, preds, model_name) |>
      rename(pred = preds) |>
      mutate(curvetype = "estimate")
    central_and_bootstrap_tpcs <- bootstrap_tpcs_all |>
      bind_rows(central_curve)
  }
    return(central_and_bootstrap_tpcs)
}

