#' Propagate parameter uncertainty of TPC fits using bootstrap with residual resampling
#'
#' @param temp a vector containing temperature treatments (predictor variable).
#' It must have at least four different temperature treatments. It must be numeric
#' and not containing NAs.
#'
#' @param dev_rate a vector containing development rate estimates, calculated as
#' the reciprocal of days of development at each temperature (i.e., 1/days of development).
#' It must be numeric and of same length as `temp`.
#'
#' @param fitted_parameters a `tibble` obtained with `fit_devmodels()` function including parameter names,
#'  estimates, se, AICs and <nls> objects (i.e. fitted_models) from `fit_devmodels()`
#'  under [nls.multstart::nls_multstart()] approach.
#'
#' @param model_name_2boot one or several TPC models of those fitted in `fit_devmodels()`.
#'  `model_name = "all"` is not allowed in this function, as bootstrapping procedure is quite slow.
#'  We recommend to apply this function only to a small pre-selection of models (e.g., one to four) based on statistical and ecological
#'  criteria with help from `plot_devmodels()` function.
#'
#' @param propagate_uncertainty Whether to propagate parameter uncertainty by bootstrap with residual resampling or not.
#' If `FALSE`, the function will return the predictions from the fitted TPCs for the chosen models. If `TRUE`, bootstrap is applied
#' using residual resampling to obtain multiple TPCs using [car::Boot()] as detailed in vignettes of `rTPC` package (`browseVignettes("rTPC")`).
#' Default to `TRUE`.
#'
#' @param n_boots_samples number of bootstrap resampling iterations (def. 100). A larger number
#' of iterations makes the resampling procedure more robust, but 100 is usually
#' enough for propagating parameter uncertainty, as increasing `n_boots_samples` will increase
#' computation time for predicting resampled TPCs.
#'
#' @returns a `tibble` object with as many curves -TPCs- as the number of iterations provided
#' in `n_boots_samples` argument if `propagate_uncertainty = TRUE`. Otherwise, it will return just one prediction TPC
#' from model fit estimates. Each resampled TPC will consist of a collection of predictions for
#' a set of temperatures from `temp - 20` to `temp + 15` with a resolution of 0.1ºC and a unique identifier
#' called `iter`. In addition to the uncertainty TPCs, the estimate TPC is also explicit in the output tibble.
#'
#' @export
#'
#' @examples
#' data("b.schwartzi_satar2002")
#'
#' fitted_tpcs_bschwartzi <- fit_devmodels(temp = b.schwartzi_satar2002$temperature,
#'                                       dev_rate = b.schwartzi_satar2002$rate_value,
#'                                       model_name = "all")
#'
#' plot_devmodels(temp = b.schwartzi_satar2002$temperature,
#'                dev_rate = b.schwartzi_satar2002$rate_value,
#'                fitted_parameters = fitted_tpcs_bschwartzi,
#'                species = "Brachycaudus swartzi",
#'                life_stage = "Nymphs") #choose "briere2", "thomas" and "lactin2"
#'
#' #3. Obtain prediction TPCs with bootstraps for propagating uncertainty:
#' tpc_preds_boots_bschwartzi <- prediction_curves(temp = b.swartzi_satar2002$temperature,
#'                                                       dev_rate = b.swartzi_satar2002$rate_value,
#'                                                       fitted_parameters = fitted_tpcs_bswartzi,
#'                                                       model_name_2boot = c("briere2", "thomas", "lactin2"),
#'                                                       propagate_uncertainty = TRUE,
#'                                                       n_boots_samples = 100)
#'
#'
#' head(tpc_preds_boots_bschwartzi)
#'

prediction_curves <- function(temp,
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
  if(!is.character(model_name)){
    stop("model_name must be a string in ?available_models")}

  if (!all(model_name %in% c("all", dev_model_table$model_name))) {
    stop("model not available. For available model names, see `dev_model_table`")
  }
  if (any(dev_rate < 0) | any(dev_rate > 10)){
    warning("negative or extremely high values of dev_rate development rate data might contain a typo error. Please check it.")}
  if(any(temp < -10) | any(temp > 56)){
    warning("experienced temperatures by active organisms are usually between 0 and 50ºC")}

  if(n_boots_samples > 5000) {
    stop("computation time will be extremely high. Please adjust `n_boots_samples` to be < 5000")
  }
  if(n_boots_samples < 100){
    warning("100 iterations might be desirable. Consider increasing `n_boots_samples` if possible")
  }
  if(!is.numeric(n_bots_samples)){
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
    tpc_bootpreds <- predict2fill_complete |>
      mutate(iter = "none",
             curvetype = "estimate") |>
      rename(pred = dev_rate) |>
      relocate(model_name, iter, temp, pred, curvetype)
    return(tpc_bootpreds)

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
      mutate(iter = "none") |>
      rename(pred = preds) |>
      mutate(curvetype = "estimate")
    central_and_bootstrap_tpcs <- bootstrap_tpcs_all |>
      bind_rows(central_curve)
  }
}
      return(central_and_bootstrap_tpcs)
}

