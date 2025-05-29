#' Determine Thermal Boundaries for Optimal Performance Level
#'
#' This function calculates thermal boundaries that define the suitable region of a Thermal Performance Curve (TPC) corresponding to a user-defined optimal performance level.
#'
#' @param preds_tbl a `tibble` object inherited from [mappestRisk::predict_curves()]
#' containing as many thermal performance curves as the number of iterations provided in `n_boots_samples` argument
#' therein and whenever `propagate_uncertainty` was set to `TRUE`; or alternatively the
#' estimated TPC predictions if `propagate_uncertainty` was set to `FALSE`.
#' Each TPC consists of predictions for temperatures ranging from `temp - 20` to `temp + 15`
#' with a resolution of 0.1°C.
#'
#' @param model_name a string with one or several TPC models of those fitted first in `fit_devmodels()` and predicted
#' next in `predict_curves()`. Setting `model_name = "all"` is not allowed in this function. If the user wants to
#' calculate thermal boundaries for several models predicted and/or bootstrapped in `predict_curves()`, typing manually
#' all desired model names within a vector will be required.
#'
#' @param suitability_threshold A numeric value from 50 to 100 representing the quantile of the curve that provides the user-defined optimal performance.
#' For instance, setting `suitability_threshold` to 80 identifies the top 20% (or quartile 80) of the maximum values of the development rate predicted by the chosen TPC model.
#' If `suitability_threshold` equals 100, the function returns the optimum temperature for development rate.
#'
#' @returns A tibble with six columns:
#'  - `model_name`: A string indicating the selected TPC model used for projections.
#'  - `suitability`: A string indicating the suitability threshold in percentage (see `suitability_threshold`).
#'  - `tval_left`: A number representing the lower thermal boundary delimiting the suitable region of the TPC.
#'  - `tval_right`: A number representing the upper thermal boundary delimiting the suitable region of the TPC.
#'  - `pred_suit`: A number corresponding to the predicted development rate value determining the chosen quantile threshold of the maximum rate (i.e., suitability percentage of maximum rate).
#'  - `iter`: A factor determining the TPC identity from the bootstrapping procedure in [predict_curves()] function, or `estimate` when it represents the estimated TPC fitted in [fit_devmodels()].
#'
#' @seealso `browseVignettes("rTPC")` for model names, start values searching workflows, and
#'  bootstrapping procedures using both [rTPC::get_start_vals()] and [nls.multstart::nls_multstart()]
#'
#'  [fit_devmodels()] for fitting Thermal Performance Curves to development rate data, which is in turn based on [nls.multstart::nls_multstart()].
#'  [predict_curves()] for bootstrapping procedure based on the above-mentioned `rTPC` vignettes.
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
#'                                         dev_rate = aphid$rate_value,
#'                                         model_name = c("lactin2", "briere2", "mod_weibull"))
#' plot_devmodels(temp = aphid$temperature,
#'                dev_rate = aphid$rate_value,
#'                fitted_parameters = fitted_tpcs_aphid,
#'                species = "Brachycaudus schwartzi",
#'                life_stage = "Nymphs") #choose "lactin2"
#'
#' # Obtain prediction TPCs with bootstraps for propagating uncertainty:
#' tpc_preds_boots_aphid <- predict_curves(temp = aphid$temperature,
#'                                              dev_rate = aphid$rate_value,
#'                                              fitted_parameters = fitted_tpcs_aphid,
#'                                              model_name_2boot = "lactin2",
#'                                              propagate_uncertainty = TRUE,
#'                                              n_boots_samples = 10)
#'
#' head(tpc_preds_boots_aphid)
#'
#' # Plot bootstrapped curves:
#'
#' plot_uncertainties(bootstrap_uncertainties_tpcs = tpc_preds_boots_aphid,
#'                    temp = aphid$temperature,
#'                    dev_rate = aphid$rate_value,
#'                    species = "Brachycaudus schwartzi",
#'                    life_stage = "Nymphs")
#'
#' #5. Calculate Q80 thermal bounds
#'
#' boundaries_aphid <- therm_suit_bounds(preds_tbl = tpc_preds_boots_aphid,
#'                                              model_name = "lactin2",
#'                                              suitability_threshold = 80)
#' head(boundaries_aphid)

therm_suit_bounds <- function(preds_tbl = NULL,
                              model_name = NULL,
                              suitability_threshold = NULL) {
  if (is.null(preds_tbl)) {
    stop("The `preds_tbl` argument is absent. Please provide a `tibble` or `data.frame` object
         from `predict_curves()`")
  }

  if (nrow(preds_tbl) == 0) {
    stop("The `preds_tbl` table is empty; check out the output of `fit_devmodels()` and `predict_curves()`.")
  }

  if (!is.data.frame(preds_tbl) |
      suppressWarnings(any(!c("model_name", "iter",
                              "temp", "pred",
                              "curvetype") %in% colnames(preds_tbl)))) {
    stop("`preds_tbl` must be a  `data.frame` inherited   from the output of `mappestRisk::predict_curves()` function")
  }

  if(is.null(suitability_threshold)){
    suitability_threshold <- 75
    message("No suitability_threshold value input. Default to `suitability_threshold = 75`")
  }

  if(suitability_threshold < 50) {
    warning("Suitability thresholds under 50% indicate thermal boundaries for positive development but not
necessarily optimal for pest risk assessment. Subsequent map risk analysis will imply
risk of thermal tolerance at each location rather than risk of optimal performance or high pest pressure.")
  }

  if(is.null(model_name)) {
    stop("No model name was provided by the user. Please provide any model present in `pred_tbl`")
  }

  if(length(model_name) > 1 || model_name == "all") {
    stop("Only one model is allowed in `therm_suit_bounds()` at a time.
Please use this function repetedly for each of your models one by one.")
  }

  if(!is.null(model_name) && any(!model_name %in% unique(preds_tbl$model_name))) {
    stop(paste("Model", model_name, "did not fitted well to your data or is not available. Try using another fitted model in your table instead"))
  }

  if(!any(preds_tbl$curvetype == "uncertainty")) {
    warning("No bootstrapped predictions were performed.
We strongly recommend to propagate uncertainty by setting the `predict_curves()`
arguments to `propagate_uncertainty = TRUE` and `n_boots_samples = 100`")
  }

  tvals <- dplyr::tibble(model_name = model_name,
                         tval_left = NULL,
                         tval_right = NULL,
                         pred_suit = NULL,
                         suitability = paste(suitability_threshold, "%"),
                         iter = NULL)

  for(iter_i in unique(preds_tbl$iter)){
    pred_tbl_i <- preds_tbl[preds_tbl$iter == iter_i, ]
    devrate_max_i <- max(pred_tbl_i$pred, na.rm = TRUE)
    possible_error <- tryCatch(expr =
                                 suppressWarnings({topt_pred <- pred_tbl_i |> #the custom error message is more informative than this warning
                                   dplyr::slice_max(pred) |>
                                   dplyr::pull(temp)
                                 half_left <- pred_tbl_i |>
                                   dplyr::filter(temp < topt_pred)
                                 half_right <- pred_tbl_i |>
                                   dplyr::filter(temp >= topt_pred)
                                 therm_suit_left <- half_left |>
                                   dplyr::filter(pred <= q80_thresh) |>
                                   dplyr::slice_tail(n = 1) |>
                                   dplyr::pull(temp)
                                 therm_suit_right <- half_right |>
                                   dplyr::filter(pred <= q80_thresh) |>
                                   dplyr::slice_head(n = 1) |>
                                   dplyr::pull(temp)
                                 dev_rate_suit <- devrate_max_i*0.01*suitability_threshold
                                 }),
                               error = function(e) e)
    if(inherits(possible_error, "error")) {
      therm_suit_right <- NA
      therm_suit_left <- NA
    }
    if(is.na(therm_suit_right) |
       is.na(therm_suit_left)) {
      warning(paste("Simulation", iter_i, "yielded NA value and then has been discarded for thermal suitability"))
    }

    tvals_i <- dplyr::tibble(model_name = model_name,
                             tval_left = therm_suit_left,
                             tval_right = therm_suit_right,
                             pred_suit = devrate_max_i*0.01*suitability_threshold,
                             suitability = paste(suitability_threshold, "%"),
                             iter = iter_i) |>
      dplyr::mutate(iter = ifelse(is.integer(iter),
                                  iter,
                                  as.factor("estimate")))
    if(any(tvals$tval_right >= 50, na.rm = TRUE))
    { warning("upper value of thermal suitability  might be non-realistic")
    }
    tvals <- dplyr::bind_rows(tvals,
                              tvals_i)
  }
  return(tvals)
}

