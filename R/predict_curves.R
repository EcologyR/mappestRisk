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
#' data("aphid")
#'
#' fitted_tpcs_aphid <- fit_devmodels(temp = aphid$temperature,
#'                                       dev_rate = aphid$rate_value,
#'                                       model_name = "all")
#'
#' plot_devmodels(temp = aphid$temperature,
#'                dev_rate = aphid$rate_value,
#'                fitted_parameters = fitted_tpcs_aphid,
#'                species = "Brachycaudus schwartzi",
#'                life_stage = "Nymphs")
#'
#' # Obtain prediction TPCs with bootstraps for propagating uncertainty:
#' tpc_preds_boots_aphid <- predict_curves(temp = aphid$temperature,
#'                                         dev_rate = aphid$rate_value,
#'                                         fitted_parameters = fitted_tpcs_aphid,
#'                                         model_name_2boot = "lactin2",
#'                                         propagate_uncertainty = TRUE,
#'                                         n_boots_samples = 10)
#'
#' head(tpc_preds_boots_aphid)

predict_curves <- function(temp = NULL,
                           dev_rate = NULL,
                           fitted_parameters = NULL,
                           model_name_2boot = NULL,
                           propagate_uncertainty = TRUE,
                           n_boots_samples = 100) {

  check_data(temp, dev_rate)

  if(is.null(fitted_parameters)) {
    stop("`fitted_parameters` must be provided.")
  }

  if (!all(model_name_2boot %in% fitted_parameters$model_name)) {
    message(paste0("Models available: ", paste0(unique(fitted_parameters$model_name), collapse = ", ")))
    stop("model not available. Check the models that converged in `fitted_parameters`")
  }

  if (!is_positive_integer(n_boots_samples)){
    stop("`n_boots_samples` must be a positive integer. Please change it within 1 and 5000 (Default 100)")
  }

  if (is_positive_integer(n_boots_samples) && n_boots_samples > 5000) {
    stop("computation time will be extremely high. Please adjust `n_boots_samples` to be < 5000. Usually 100 is fine.")
  }

  if (n_boots_samples < 100){
    warning("100 iterations might be desirable. Consider increasing `n_boots_samples` if possible")
  }

  if (!is.logical(propagate_uncertainty)) {
    stop("`propagate_uncertainty` must be `TRUE` or `FALSE` (def. `TRUE`)")
  }

  devdata <- dplyr::tibble(temp,
                           dev_rate)
  predict2fill <- dplyr::tibble(temp = NULL,
                         dev_rate = NULL,
                         model_name = NULL,
                         model_fit = NULL,
                         model_AIC = NULL)

  for (model_name_i in model_name_2boot){
    fitted_parameters_i <- fitted_parameters |>
      dplyr::filter(model_name == model_name_i)
    model_AIC_i <- fitted_parameters_i |>
      dplyr::pull(model_AIC)
    model_fit_i <- fitted_parameters_i |>
      dplyr::pull(model_fit)
    params_i <- fitted_parameters_i |>
      dplyr::pull(param_est)
    formula_i <- available_models |>
      dplyr::filter(model_name == model_name_i) |>
      dplyr::pull(working_formula)
    ##predict based on parameters
    explore_preds <- dplyr::tibble(temp = seq(min(devdata$temp) - 20,
                                       max(devdata$temp) + 15,
                                       .5),
                            model_name = model_name_i,
                            model_fit = model_fit_i[1],
                            model_AIC = model_AIC_i[1],
                            preds = NULL,
                            n_params = length(params_i))
    fit_vals_tbl <- explore_preds |>
      dplyr::select(temp, model_name, model_AIC, n_params, model_fit) |>
      dplyr::mutate(formula = formula_i) |>
      dplyr::mutate(preds = purrr::map_dbl(.x = temp,
                                    .f = stats::reformulate(unique(formula_i)))) |>
      dplyr::filter(preds >= -0.2) |>
      dplyr::select(-formula)
    # to exclude biological non-sense predictions due to model mathematical properties
    predict2fill <- predict2fill |>
      dplyr::bind_rows(fit_vals_tbl)
  }
  predict2fill_complete <- predict2fill |>
    dplyr::mutate(tidyr::nest(devdata)) |>
    dplyr::mutate(coefs = purrr::map(.x = model_fit,
                                     .f = coef))

  if (propagate_uncertainty == FALSE) {
    tpc_estimate <- dplyr::tibble(model_name = predict2fill_complete$model_name,
                           iter = rep(NA, nrow(predict2fill_complete)),
                           temp = predict2fill_complete$temp,
                           pred = predict2fill_complete$preds,
                           curvetype = rep("estimate", nrow(predict2fill_complete))
                           )
    warning("No bootstrap was performed. We strongly recommend to propagate uncertainty.")
    return(tpc_estimate)

  } else {
    boot_2fill <- dplyr::tibble(temp = NULL,
                       model_name = NULL,
                       model_AIC = NULL,
                       n_params = NULL,
                       model_fit = NULL,
                       preds = NULL,
                       data = NULL,
                       coefs = NULL,
                       bootstrap = NULL)

  for (model_i in model_name_2boot){
    predict_model_i <- predict2fill_complete |>
      dplyr::filter(model_name == model_i)
    coefs_i <- purrr::as_vector(unique(predict_model_i$coefs))
    temp_data_i <- devdata
    formula_i <- available_models |>
      dplyr::filter(model_name == model_i) |>
      dplyr::pull(formula)

    ## avoid errors when wrapping Boot
    assign("model_i", model_i, envir=.GlobalEnv)
    assign("predict_model_i", predict_model_i, envir = .GlobalEnv)
    assign("coefs_i", coefs_i, envir=.GlobalEnv)
    assign("temp_data_i", temp_data_i, envir=.GlobalEnv)
    assign("formula_i", formula_i, envir=.GlobalEnv)
    possible_error <- tryCatch(expr = {
        temp_fit <- suppressWarnings(minpack.lm::nlsLM(formula = reformulate(response = "dev_rate",
                                                            termlabels = formula_i),
                                                       data = temp_data_i,
                                                       na.action = na.exclude,
                                                       start = coefs_i))
      assign("temp_fit", temp_fit, envir=.GlobalEnv)
      suppressPackageStartupMessages(library(car)) # <- necessary for "residual" method of car::Boot() to work

      # now bootstrap is performed to each model fit and listed
      boot <- suppressWarnings(car::Boot(temp_fit,
                                         method = 'residual',
                                         R = n_boots_samples)
                               )
      boot_2fill_i <- predict_model_i |>
        dplyr::mutate(bootstrap = list(boot))
    }, # <- inside tryCatch
    error = function(e) e)
    if (inherits(possible_error, "error")) {
      temp_fit <- NULL
      boot <- NA
      boot_2fill_i <- predict_model_i |>
        dplyr::mutate(bootstrap = list(boot))
    }
    boot_2fill <- dplyr::bind_rows(boot_2fill, boot_2fill_i)
  }
  rm("model_i", "predict_model_i", "coefs_i", "temp_data_i", "formula_i", "temp_fit",
     envir = .GlobalEnv)

  boot_2fill_clean <- boot_2fill |>
    dplyr::filter(!is.na(bootstrap)) # avoid errors from NAs

  if(nrow(boot_2fill_clean) == 0) {
    stop("Bootstrapping failed for all the models provided in `model_name_2boot` due to convergence problems.
         You may try other models fitted with `fit_devmodels()`. If this error persists after attempting all the
         models obtained from `fit_devmodels()`,your data may not be appropriate for
         projecting risk of pest occurrence with the models you have fitted.")
  }
  #get the raw values of each bootstrap. These raw values represent the estimates
  #  generated from each bootstrap sample for further analysis, such as computing confidence intervals
  tpc_fits_boot <- boot_2fill_clean |>
    dplyr::group_by(model_name) |>
    dplyr::mutate(output_boot = purrr::map(.x = bootstrap,
                                           .f = ~purrr::pluck(.x, "t")))

  bootstrap_tpcs_all <- dplyr::tibble(model_name = NULL,
                               iter = NULL,
                               temp = NULL,
                               pred = NULL,
                               curvetype = NULL)
  #preds boot with a for loop
  print("ADVISE: the simulation of new bootstrapped curves takes some time. Await patiently or reduce your `n_boots_samples`")
  pb <- progress::progress_bar$new(
    format = "Predicting bootstrapped TPCs [:bar] :percent",
    total = length(tpc_fits_boot$output_boot),
    clear = F)
  for (temp_model_i in 1:length(tpc_fits_boot$output_boot)){
    boot_preds_i <- tpc_fits_boot[temp_model_i,]
    model_name_boot_i <- boot_preds_i$model_name
    boot_coefs_i <- boot_preds_i |>
      tidyr::unnest_wider(col = coefs)
    formula_i <- available_models |>
      dplyr::filter(model_name == model_name_boot_i) |>
      dplyr::pull(working_formula)
    output_boot_df <- as.data.frame(boot_preds_i$output_boot) |>
      dplyr::mutate(iter = 1:dplyr::n()) |>
      dplyr::tibble()

    tpc_bootpreds <- dplyr::tibble(model_name = NULL,
                            iter = NULL,
                            temp = NULL,
                            pred = NULL,
                            curvetype = NULL)
    for (boot_iter in 1:nrow(output_boot_df)){
      params_i <- output_boot_df[boot_iter, 1:boot_preds_i$n_params] |>
        purrr::as_vector()
      tpc_boot_iter_temp <- dplyr::tibble(model_name = model_name_boot_i,
                                   iter = boot_iter,
                                   temp = boot_preds_i$temp,
                                   pred = purrr::map_dbl(.x = temp,
                                                  .f = stats::reformulate(formula_i)),
                                   curvetype = "uncertainty")
      tpc_bootpreds <- dplyr::bind_rows(tpc_bootpreds, tpc_boot_iter_temp)
    }
    bootstrap_tpcs_all <- dplyr::bind_rows(bootstrap_tpcs_all, tpc_bootpreds) |>
      dplyr::filter(pred >= 0)
    central_curve <- tpc_fits_boot |>
      dplyr::select(temp, preds, model_name) |>
      dplyr::mutate(iter = NA) |>
      dplyr::rename(pred = preds) |>
      dplyr::mutate(curvetype = "estimate")
    central_and_bootstrap_tpcs <- bootstrap_tpcs_all |>
      dplyr::bind_rows(central_curve)
    pb$tick()
    }
  cat("\n Bootstrapping simulations completed \n")
  }
  if(!any(central_and_bootstrap_tpcs$curvetype == "uncertainty")){
   warning("No bootstrap was accomplished. Your model might not be suitable for bootstrapping
due to convergence problems")
  }
      return(central_and_bootstrap_tpcs)
}

