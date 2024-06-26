#' Propagate parameter uncertainty of TPC fits using bootstrap with residual resampling
#'
#' @param temp a vector of temperatures used in the experiment.
#' It should have at least four different temperatures and must contain only numbers
#' without any missing values.
#'
#' @param dev_rate a vector of estimated development rates corresponding to each temperature.
#' These rates are calculated as the inverse of the number of days to complete the transition
#' from the beginning of a certain life stage to the beginning of the following at each temperature.
#' It must be numeric and of the same length as `temp`.
#'
#'
#' @param fitted_parameters a `tibble` obtained with [fit_devmodels()] function, including parameter names,
#'  estimates, standard errors, AICs, and <nls> objects (fitted_models) using the [nls.multstart::nls_multstart()] approach.

#' @param model_name_2boot A vector of strings including one or several TPC models fitted by [fit_devmodels()]. Contrarily to that function,
#' `model_name_2boot = "all"` is not allowed in this function due to the slow bootstrapping procedure.
#' We recommend applying this function only to a small pre-selection of models (e.g., one to four) based on statistical
#' and ecological criteria with the help of [plot_devmodels()] function.
#'
#'
#' @param propagate_uncertainty A logical argument that specifies whether to propagate parameter uncertainty by bootstrap with residual resampling.
#' If `FALSE`, the function returns predictions from the fitted TPCs for the selected model(s). If `TRUE`, bootstrap is applied
#' using residual resampling to obtain multiple TPCs using [car::Boot()] as detailed in vignettes of the `rTPC` package.
#' Defaults to `TRUE`.
#'
#' @param n_boots_samples Number of bootstrap resampling iterations (default is 100). A larger number
#' of iterations makes the resampling procedure more robust, but typically 100 is sufficient for propagating parameter
#' uncertainty, as increasing `n_boots_samples` will increase computation time for predicting resampled TPCs.
#'
#'
#' @returns A tibble object with as many curves (TPCs) as the number of iterations provided
#' in the `n_boots_samples` argument if `propagate_uncertainty = TRUE`. Otherwise, it returns just one prediction TPC
#' from model fit estimates. Each resampled TPC consists of a collection of predictions for
#' a set of temperatures from `temp - 20` to `temp + 15` with a resolution of 0.1°C and a unique identifier
#' called `iter`. In addition to the uncertainty TPCs, the estimated TPC is also explicit in the output tibble.
#'
#' @seealso `browseVignettes("rTPC")` for model names, start values searching workflows, and
#'  bootstrapping procedures using both [rTPC::get_start_vals()] and [nls.multstart::nls_multstart()]
#'
#'  [fit_devmodels()] for fitting Thermal Performance Curves to development rate data, which is in turn based on [nls.multstart::nls_multstart()].
#'
#' @references
#'  Angilletta, M.J., (2006). Estimating and comparing thermal performance curves. <i>J. Therm. Biol.</i> 31: 541-545.
#'  (for reading on model selection in TPC framework)
#'
#'  Padfield, D., O'Sullivan, H. and Pawar, S. (2021). <i>rTPC</i> and <i>nls.multstart</i>: A new pipeline to fit thermal performance curves in `R`. <i>Methods Ecol Evol</i>. 00: 1-6
#'
#'  Rebaudo, F., Struelens, Q. and Dangles, O. (2018). Modelling temperature-dependent development rate and phenology in arthropods: The `devRate` package for `R`. <i>Methods Ecol Evol</i>. 9: 1144-1150.
#'
#'  Satar, S. and Yokomi, R. (2002). Effect of temperature and host on development of <i>Brachycaudus schwartzi</i> (Homoptera: Aphididae). <i>Ann. Entomol. Soc. Am.</i> 95: 597-602.
#'
#' @source
#' The dataset used in the example was originally published in Satar & Yokomi (2022) under the CC-BY-NC license
#'
#' @export
#'
#' @examples
#' data("b.schwartzi_satar2002")
#'
#' fitted_tpcs_bschwartzi <- fit_devmodels(temp = b.schwartzi_satar2002$temperature,
#'                                         dev_rate = b.schwartzi_satar2002$rate_value,
#'                                         model_name = "all")
#'
#' plot_devmodels(temp = b.schwartzi_satar2002$temperature,
#'                dev_rate = b.schwartzi_satar2002$rate_value,
#'                fitted_parameters = fitted_tpcs_bschwartzi,
#'                species = "Brachycaudus swartzi",
#'                life_stage = "Nymphs") #choose "briere2", "thomas" and "lactin2"
#'
#' # Obtain prediction TPCs with bootstraps for propagating uncertainty:
#' tpc_preds_boots_bschwartzi <- predict_curves(temp = b.schwartzi_satar2002$temperature,
#'                                              dev_rate = b.schwartzi_satar2002$rate_value,
#'                                              fitted_parameters = fitted_tpcs_bschwartzi,
#'                                              model_name_2boot = c("briere2", "thomas", "lactin2"),
#'                                              propagate_uncertainty = TRUE,
#'                                              n_boots_samples = 100)
#'
#' head(tpc_preds_boots_bschwartzi)

predict_curves <- function(temp,
                              dev_rate,
                              fitted_parameters,
                              model_name_2boot,
                              propagate_uncertainty = TRUE,
                              n_boots_samples = 100) {

  if(any(is.na(dev_rate))) {
    stop("development rate data have NAs; please consider removing them or fixing them")
  }
  if(any(is.na(temp))) {
    stop("temperature data have NAs; please consider removing them or fixing them")
  }
  if(!is.numeric(temp)) {
    stop("temperature data is not numeric. Please check it.")
  }
  if(!is.numeric(dev_rate)) {
    stop("development rate data is not numeric. Please check it.")
  }
  if(length(temp) != length(dev_rate)) {
    stop("development rate and temperature inputs are not of same length. Please check it.")
  }
  if(!is.character(model_name_2boot)){
    stop("model_name_2boot must be a string in ?available_models")}

  if (!all(model_name_2boot %in% c("all", dev_model_table$model_name))) {
    stop("model not available. For available model names, see `dev_model_table`")
  }
  if(n_boots_samples > 5000) {
    stop("computation time will be extremely high. Please adjust `n_boots_samples` to be < 5000")
  }
  if(n_boots_samples < 100){
    warning("100 iterations might be desirable. Consider increasing `n_boots_samples` if possible")
  }
  if(!is.numeric(n_boots_samples)){
    stop("`n_boots_samples` must be numeric. Please change it within 1 and 5000 (Default 100)")
  }
  if(!is.logical(propagate_uncertainty)) {
    stop("`propagate_uncertainty` must be `TRUE` or `FALSE` (def. `TRUE`)")
  }
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

  if(propagate_uncertainty == FALSE) {
    tpc_estimate <- tibble(model_name = predict2fill_complete$model_name,
                           iter = rep(NA, nrow(predict2fill_complete)),
                           temp = predict2fill_complete$temp,
                           pred = predict2fill_complete$preds,
                           curvetype = rep("estimate", nrow(predict2fill_complete))
                           )
    return(tpc_estimate)

  } else {
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
      mutate(iter = NA) |>
      rename(pred = preds) |>
      mutate(curvetype = "estimate")
    central_and_bootstrap_tpcs <- bootstrap_tpcs_all |>
      bind_rows(central_curve)
  }
}
      return(central_and_bootstrap_tpcs)
}

