#' Propagate parameter uncertainty of TPC fits using bootstrap with residual resampling
#'
#' @inheritParams fit_devmodels
#'
#' @param fitted_parameters a `tibble` obtained with [fit_devmodels()] function,
#' including parameter names, estimates, standard errors, AICs, and <nls> objects
#' (fitted_models) using the [nls.multstart::nls_multstart()] approach.

#' @param model_name_2boot A vector of strings including one or several TPC models
#' fitted by [fit_devmodels()]. Contrarily to that function, `model_name_2boot = "all"`
#' is not allowed in this function due to the slow bootstrapping procedure.
#' We recommend applying this function only to a small pre-selection of models
#' (e.g., one to four) based on statistical and ecological criteria with the help
#' of [plot_devmodels()] function.
#'
#'
#' @param propagate_uncertainty A logical argument that specifies whether to
#' propagate parameter uncertainty by bootstrap with residual resampling.
#' If `FALSE`, the function returns predictions from the fitted TPCs for the
#' selected model(s). If `TRUE`, bootstrap is applied using residual resampling
#' to obtain multiple TPCs as detailed in vignettes of the `rTPC` package.
#' Defaults to `TRUE`.
#'
#' @param n_boots_samples Number of bootstrap resampling iterations (default is 100).
#' A larger number of iterations makes the resampling procedure more robust,
#' but typically 100 is sufficient for propagating parameter uncertainty,
#' as increasing `n_boots_samples` will increase computation time for predicting resampled TPCs.
#'
#'
#' @returns A tibble object with as many curves (TPCs) as the number of iterations provided
#' in the `n_boots_samples` argument if `propagate_uncertainty = TRUE` minus the bootstrap samples that
#' could not be fitted (i.e., new nonlinear regression models did not converge for them).
#' Otherwise, it returns just one prediction TPC from model fit estimates.
#' Each resampled TPC consists of a collection of predictions for a set of temperatures
#' from `temp - 20` to `temp + 15` with a resolution of 0.1°C and a unique identifier
#' called `boots_iter`. In addition to the uncertainty TPCs, the estimated TPC
#' is also explicit in the output tibble.
#'
#' @seealso `browseVignettes("rTPC")` for model names, start values searching workflows, and
#'  bootstrapping procedures using both [rTPC::get_start_vals()] and [nls.multstart::nls_multstart()]
#'
#'  [fit_devmodels()] for fitting Thermal Performance Curves to development rate data,
#'  which is in turn based on [nls.multstart::nls_multstart()].
#'
#' @inherit fit_devmodels references source
#'
#' @export
#'
#'
#' @examplesIf interactive()
#' data("aphid")
#'
#' fitted_tpcs <- fit_devmodels(temp = aphid$temperature,
#'                              dev_rate = aphid$rate_value,
#'                              model_name = "all")
#'
#' plot_devmodels(temp = aphid$temperature,
#'                dev_rate = aphid$rate_value,
#'                fitted_parameters = fitted_tpcs,
#'                species = "Brachycaudus schwartzi",
#'                life_stage = "Nymphs")
#'
#' # Obtain prediction TPCs with bootstraps for propagating uncertainty:
#' boot_tpcs <- predict_curves(temp = aphid$temperature,
#'                             dev_rate = aphid$rate_value,
#'                             fitted_parameters = fitted_tpcs,
#'                             model_name_2boot = c("lactin2", "briere2", "beta"),
#'                             propagate_uncertainty = TRUE,
#'                             n_boots_samples = 10)
#'
#' boot_tpcs


predict_curves <- function(temp = NULL,
                           dev_rate = NULL,
                           fitted_parameters = NULL,
                           model_name_2boot = NULL,
                           propagate_uncertainty = TRUE,
                           n_boots_samples = 100) {


  ## Checks

  check_data(temp, dev_rate)

  if (is.null(fitted_parameters)) {
    stop("`fitted_parameters` must be provided.")
  }

  if (!all(model_name_2boot %in% fitted_parameters$model_name)) {
    message(paste0("Models available: ", paste0(unique(fitted_parameters$model_name), collapse = ", ")))
    stop("model not available. Check the models that converged in `fitted_parameters`")
  }

  if (!is_positive_integer(n_boots_samples)) {
    stop("`n_boots_samples` must be a positive integer. Please change it within 1 and 5000 (Default 100)")
  }

  if (n_boots_samples > 5000) {
    stop("computation time will be extremely high. Please adjust `n_boots_samples` to be < 5000. Usually 100 is fine.")
  }

  if (n_boots_samples < 100) {
    warning("100 iterations might be desirable. Consider increasing `n_boots_samples` if possible")
  }

  if (!is.logical(propagate_uncertainty)) {
    stop("`propagate_uncertainty` must be `TRUE` or `FALSE` (def. `TRUE`)")
  }

  ## end checks

  devdata <- dplyr::tibble(temp,
                           dev_rate)

  # define temperature range (common to all models)
  temp.min <- min(devdata$temp) - 20 # allow user to change this?
  temp.max <- max(devdata$temp) + 15 # allow user to change this?
  temp.step <- 0.1 # allow user to change this? Or change to 0.1 (1 decimal degree!) to save memory and time?
  temp.range <- seq(from = temp.min, to = temp.max, by = temp.step)

  # get predictions for each model
  fitted_df <- fitted_parameters |>
    dplyr::filter(model_name %in% model_name_2boot)

  estimate_curve_list <- fitted_df |>
    split(fitted_df$model_name) |>
    lapply(predict_from_model,
           temps = temp.range)

  estimate_curve <- dplyr::bind_rows(estimate_curve_list) |>
    dplyr::mutate(boot_iter = NA,
                  curvetype = "estimate") |>
    dplyr::relocate(boot_iter, .after = "model_name")

  if (nrow(estimate_curve) < 1) {stop("No bootstrap was attempted. Check your model(s)")}

  if (!isTRUE(propagate_uncertainty)) {
    warning("No bootstrap was performed. We strongly recommend to propagate uncertainty.")
    return(estimate_curve)
  }


  if (isTRUE(propagate_uncertainty)) {

    message("\nNote: the simulation of new bootstrapped curves takes some time. Await patiently or reduce your `n_boots_samples`\n")

    boot_models_list <- lapply(model_name_2boot,
                               bootstrap_model,
                               fitted_df = fitted_df,
                               devdata = devdata,
                               nboot = n_boots_samples,
                               temp.range = temp.range)
    boot_models <- dplyr::bind_rows(boot_models_list)

    if (!any(boot_models$curvetype == "uncertainty")) {
      warning("No bootstrap was accomplished. Your model might not be suitable for bootstrapping
due to convergence problems")
    }

    out <- dplyr::bind_rows(estimate_curve, boot_models)

  }

  return(out)

}



# function to generate dataframe with predictions from a single model
predict_from_model <- function(fitted_df = NULL, temps = NULL, model_name = NULL) {

  stopifnot(is.data.frame(fitted_df))
  stopifnot(is.numeric(temps)) # temperatures to generate predictions for
  if (is.null(model_name)) {
    model_name <- unique(fitted_df$model_name)
    stopifnot(length(model_name) == 1)
    # will predict from single model in dataframe
  }

  df <- dplyr::tibble(model_name, temp = temps)
  model_fit <- get_fitted_model(fitted_df, model_name = model_name)
  df$dev_rate <- stats::predict(model_fit, newdata = data.frame(temp = temps))
  # filter values >= 0
  dplyr::filter(df, dev_rate >= 0)
}


# function to generate predictions from residual resampling
bootstrap_model <- function(model_name, fitted_df, devdata, nboot, temp.range) {

  # Extract residuals and fitted values
  model_fit <- get_fitted_model(fitted_df, model_name = model_name)
  resids <- stats::residuals(model_fit)
  fit_vals <- stats::fitted(model_fit)

  ## residual resampling
  pb <- progress::progress_bar$new(
    format = paste0(model_name,": Predicting bootstrapped TPCs [:bar] :percent"),
    total = nboot,
    clear = FALSE)

  resampled_data <- dplyr::tibble(
    model_name = model_name,
    boot_iter = rep(seq_len(nboot), each = length(resids)),
    temp = rep(devdata$temp, times = nboot),
    fittedvals = rep(fit_vals, times = nboot)) |>
    dplyr::group_by(boot_iter) |>
    dplyr::mutate(resampled_resid = sample(resids, size = length(resids), replace = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(dev_rate = fittedvals + resampled_resid) |>
    dplyr::filter(dev_rate >= 0) # ensure resampled rates are non-negative


  ## Fit models to resampled datasets

  refit <- resampled_data |>
    split(resampled_data$boot_iter) |>
    lapply(function(x) {
      pb$tick()
      try(suppressMessages(fit_devmodels(temp = x$temp,
                                         dev_rate = x$dev_rate,
                                         model_name = unique(x$model_name))),
          silent = TRUE)
    })
  names(refit) <- unique(resampled_data$boot_iter)

  # remove list elements that are errors (failing models)
  refit2 <- Filter(is.data.frame, refit)

  ## Predict from bootstrapped models
  refit_pred <- lapply(refit2,
                       predict_from_model,
                       temps = temp.range)
  # add a column with the boot_iter to each data frame
  for (iter in names(refit_pred)) {
    refit_pred[[iter]]$boot_iter <- iter
  }

  out <- dplyr::bind_rows(refit_pred) |>
    dplyr::mutate(curvetype = "uncertainty") |>
    dplyr::relocate(boot_iter, .after = model_name)

  if (nrow(out) == 0) {
    warning("For model '", model_name, "', no bootstrap iterations converged successfully.")
    return(NULL)
  } else {
    message("\n Bootstrapping simulations completed for ", model_name, "\n")
  }

  return(out)

}

