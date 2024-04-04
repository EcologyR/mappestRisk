#' Calculate thermal boundaries delimiting the optimal region of the fitted TPC.
#'
#' @param preds_tbl a `tibble` object inherited from [mappestRisk::predict_curves()]
#' with as many curves -TPCs- as the number of iterations provided in `n_boots_samples` argument
#' therein whenever `propagate_uncertainty` was set to `TRUE`, or alternatively simply the
#' estimated TPC predictions if `propagate_uncertainty` was set to `FALSE`.
#' Each TPC in this `tibble`  consist of a collection of predictions for a set
#' of temperatures from `temp - 20` to `temp + 15` with a resolution of 0.1ºC and a unique identifier called `iter`.
#' In addition to the uncertainty TPCs, the estimate TPC is also explicit in the output tibble.
#'
#' @param model_name a string with one or several TPC models of those fitted first in `fit_devmodels()` and predicted
#' next in `predict_curves()`. Setting `model_name = "all"` is not allowed in this function. If the user wants to
#' calculate thermal boundaries for several models predicted and/or bootstrapped in `predict_curves()`, typing manually
#' all desired model names within a vector will be required.
#'
#' @param suitability_threshold a numeric value from 50 to 100 representing the quantile of
#' the curve that provides the user-defined optimal performance. For example, by setting `suitability_threshold` to
#' 80, the function will give the thermal boundaries determining the  top 20% (or quartile 80) of the maximum
#' values of the development rate predicted by the chosen TPC model. If `suitability_threshold` equals 100, the function returns
#' the optimum temperature for development rate.
#'
#'
#' @returns a `tibble` with six columns:
#'  - `model_name`: a string indicating the selected TPC model to make projections
#'  - `suitability`: a string indicating the suitability threshold in percentage (see `suitability_threshold`)
#'  - `tval_left`:a number representing the lower thermal boundary delimiting suitable region of the TPC
#'  - `tval_right`: a number representing the upper thermal boundary delimiting the suitable region of the TPC
#'  - `pred_suit`: a number corresponding to the predicted development rate value determining que chosen quantile threshold
#'  of the maximum rate (i.e., suitability percentage of maximum rate)
#'  - `iter`:  a factor determining the TPC identity from bootstrapping procedure in
#'  `predict_curves()` function, or `estimate` when it represents the estimated TPC fitted in
#'  `fit_devmodels()`.
#'
#' @export
#'
#' @examples
#'
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
#' tpc_preds_boots_bschwartzi <- predict_curves(temp = b.swartzi_satar2002$temperature,
#'                                              dev_rate = b.swartzi_satar2002$rate_value,
#'                                              fitted_parameters = fitted_tpcs_bswartzi,
#'                                              model_name_2boot = c("briere2", "thomas", "lactin2"),
#'                                              propagate_uncertainty = TRUE,
#'                                              n_boots_samples = 100)
#'
#'
#' head(tpc_preds_boots_bschwartzi)
#'
#' #4. Plot bootstrapped curves:
#'
#' plot_uncertainties(bootstrap_uncertainties_tpcs = tpc_preds_boots_bschwartzi,
#'                    temp = b.swartzi_satar2002$temperature,
#'                    dev_rate = b.swartzi_satar2002$rate_value,
#'                    species = "Brachycaudus schwartzi",
#'                    life stage = "Nymphs")
#'
#' #5. Calculate Q80 thermal bounds
#'
#' boundaries_bschwartzi <- therm_suitability_bounds(preds_tbl = tpc_preds_boots_bschwartzi,
#'                                                   model_name = "lactin2",
#'                                                   suitability_threshold = 80)
#' head(boundaries_bschwartzi)
#'
therm_suit_bounds <- function(preds_tbl,
                              model_name,
                              suitability_threshold = 75) {

  if (!is.data.frame(preds_tbl) |
       suppressWarnings(any(!c("model_name", "iter",
                               "temp", "pred",
                               "curvetype") %in% colnames(preds_tbl)))) {
     stop("`preds_tbl` must be a  `data.frame` inherited   from the output of `mappestRisk::predict_curves()` function")
   }
   if (nrow(preds_tbl) == 0) {
     stop("The `preds_tbl` table is NULL; check out the output of `fit_devmodels()` and `predict_curves()`.")
    }
  if(is.null(suitability_threshold)){
     suitability_threshold <- 75
     message("No suitability_threshold value input. Using by default suitability_threshold = 75%")
    }
  if(suitability_threshold < 50) {
    stop("Suitability must be higher than 50% in order to have applied sense. If set to NULL, suitability_threshold = 75% by default")
  }
  if (!is.null(model_name) && any(!model_name %in% dev_model_table$model_name)) {
    stop("Model name not available. For available model names, see `dev_model_table`.")
  }
  if(!is.null(model_name) && any(!model_name %in% preds_tbl)) {
    stop(paste("Model", model_name, "did not fitted well to your data. Try using another fitted model in your table instead"))
  }
  if(length(model_name) > 1 || model_name == "all") {
    stop("Only one model is allowed in `thermal_suitability_bounds()`.
         Please calculate thermal boundaries one by one and apply repeatedly this function as many times as desired.")
  }
  if(length(unique(preds_tbl$iter)) < 2){
    warning("No bootstrapped predictions were performed.
            We strongly recommend to propagate uncertainty by setting the `predict_curves()`
            arguments to `propagate_uncertainty = TRUE` and `n_boots_samples = 100`")
  }
  if(any(is.na(dev_rate))) {
    stop("development rate data have NAs; please consider removing them or fixing them")
  }
  if (any(is.na(temp))) {
    stop("temperature data have NAs; please consider removing them or fixing them")
  }
  if (!is.numeric(temp)) {
    stop("temperature data is not numeric. Please check it.")
  }
  if (!is.numeric(dev_rate)) {
    stop("development rate data is not numeric. Please check it.")
  }
  if (length(temp) != length(dev_rate)) {
    stop("development rate and temperature inputs are not of same length. Please check it.")
  }
  if (any(dev_rate < 0) | any(dev_rate > 10)){
    warning("negative or extremely high values of dev_rate development rate data might contain a typo error. Please check it.")
  }
  if(any(temp < -10) | any(temp > 56)) {
    warning("experienced temperatures by active organisms (i.e. not in diapause) are usually between 0 and 50ºC")
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
                                   slice_max(pred) |>
                                   pull(temp)
                                 half_left <- pred_tbl_i |>
                                   dplyr::filter(temp < topt_pred)
                                 half_right <- pred_tbl_i |>
                                   dplyr::filter(temp >= topt_pred)
                                 therm_suit_left <- half_left |>
                                   dplyr::slice(max(which(half_left$pred <= devrate_max_i*0.01*suitability_threshold),
                                                    na.rm = TRUE)) |>
                                   dplyr::pull(temp)
                                 therm_suit_right <- half_right |>
                                   dplyr::slice(max(which(half_right$pred <= devrate_max_i*0.01*suitability_threshold),
                                                    na.rm = TRUE)) |>
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
      warning(paste("Simulation", iter_i, "yielded NA value and then has been discarded for thermal suitability "))
    }

    tvals_i <- dplyr::tibble(model_name = model_name,
                             tval_left = therm_suit_left,
                             tval_right = therm_suit_right,
                             pred_suit = devrate_max_i*0.01*suitability_threshold,
                             suitability = paste(suitability_threshold, "%"),
                             iter = iter_i) |>
      mutate(iter = ifelse(is.integer(iter),
                           iter,
                           as.factor("estimate")))
    if(any(tvals$tval_right >= 50, na.rm = TRUE))
    { warning("upper value of thermal suitability  might be non-realistic")
    }
    tvals <- bind_rows(tvals,
                       tvals_i)
  }
  return(tvals)
}


