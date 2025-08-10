#' Determine Thermal Boundaries for Optimal Performance Level
#'
#' Calculate thermal boundaries that define the suitable region of a
#' Thermal Performance Curve (TPC) corresponding to a user-defined optimal performance level.
#'
#' @param preds_tbl a `tibble` object as produced by [predict_curves()].
#'
#' @param model_name character. Name of one or several of the TPC models fitted
#' first in `fit_devmodels()` and predicted next in `predict_curves()`.
#' If using `model_name = "all"` all models contained in `preds_tbl` will be used.
#'
#' @param suitability_threshold A numeric value from 50 to 100 representing
#' the quantile of the curve that provides the user-defined optimal performance.
#' For instance, setting `suitability_threshold` to 80 identifies the top 20%
#' (or quantile 80) of the maximum values of the development rate predicted by
#' the chosen TPC model. If `suitability_threshold` equals 100, the function
#' returns the optimum temperature for development rate.
#'
#' @returns A tibble with six columns:
#'  - `model_name`: A string indicating the selected TPC model used for projections.
#'  - `suitability`: A string indicating the suitability threshold in percentage
#'    (see `suitability_threshold`).
#'  - `tval_left`: A number representing the lower thermal boundary delimiting
#'    the suitable region of the TPC.
#'  - `tval_right`: A number representing the upper thermal boundary delimiting
#'    the suitable region of the TPC.
#'  - `pred_suit`: A number corresponding to the predicted development rate value
#'    determining the chosen quantile threshold of the maximum rate
#'    (i.e., suitability percentage of maximum rate).
#'  - `iter`: A string determining the TPC identity from the bootstrapping
#'    procedure in [predict_curves()] function, or `estimate` when it represents
#'    the estimated TPC fitted in [fit_devmodels()].
#'
#'
#' @inherit fit_devmodels references source
#' @inherit plot_uncertainties seealso
#'
#' @export
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
#' print(boot_tpcs)
#'
#' # Plot bootstrapped curves:
#'
#' plot_uncertainties(temp = aphid$temperature,
#'                    dev_rate = aphid$rate_value,
#'                    bootstrap_tpcs = boot_tpcs,
#'                    species = "Brachycaudus schwartzi",
#'                    life_stage = "Nymphs")
#'
#' # Calculate Q80 thermal bounds
#'
#' boundaries <- therm_suit_bounds(preds_tbl = boot_tpcs,
#'                                 model_name = "lactin2",
#'                                 suitability_threshold = 80)
#' head(boundaries)


therm_suit_bounds <- function(preds_tbl = NULL,
                              model_name = NULL,
                              suitability_threshold = NULL) {

  ## Checks ##

  if (is.null(preds_tbl)) {
    stop("The `preds_tbl` argument is missing. Please provide a `tibble` or `data.frame` object
         from `predict_curves()`")
  }

  if (nrow(preds_tbl) == 0) {
    stop("The `preds_tbl` table is empty; check out the output of `fit_devmodels()` and `predict_curves()`.")
  }

  if (!inherits(preds_tbl, "data.frame") |
      !all(c("model_name", "boot_iter", "temp", "dev_rate", "curvetype") %in% names(preds_tbl))) {
    stop("`preds_tbl` must be a `data.frame` with columns as produced by the `predict_curves()` function")
  }

  if (is.null(suitability_threshold)) {
    suitability_threshold <- 75
    message("No suitability_threshold value provided. Default to `suitability_threshold = 75`")
  }

  if (suitability_threshold < 50) {
    warning("Suitability thresholds under 50% indicate thermal boundaries for positive development but not
    necessarily optimal for pest risk assessment. Subsequent map risk analysis will imply
    risk of thermal tolerance at each location rather than risk of optimal performance or high pest pressure.")
  }

  if (is.null(model_name)) {
    stop("No model name was provided by the user. Please provide any model present in `pred_tbl`")
  }

  # If model_name = "all", use all models in preds_tbl
  if (length(model_name) == 1 && model_name == "all") {
    model_name <- unique(preds_tbl$model_name)
  }

  if (!all(model_name %in% unique(preds_tbl$model_name))) {
    stop("Model ", model_name[which(!model_name %in% unique(preds_tbl$model_name))],
    " is not available in `preds_tbl`.
    Try using another fitted model in your table instead")
  }

  if (!any(preds_tbl$curvetype == "uncertainty")) {
    warning("No bootstrapped predictions were performed.
    We strongly recommend to propagate uncertainty by setting the `predict_curves()`
            arguments to `propagate_uncertainty = TRUE` and `n_boots_samples = 100`")
  }

  ## End checks ##


  preds_mod <- preds_tbl |>
    dplyr::rename(model = model_name) |>
    dplyr::filter(model %in% model_name) |>
    # boot_iter is NA for predicted curves without bootstrapping ("estimate")
    # cf. predict_curves()
    dplyr::mutate(iter = dplyr::if_else(
      is.na(boot_iter),
      "estimate",
      as.character(boot_iter)
    ))

  ## Calculate boundaries for each bootstrap iteration as well as "estimate"
  boundaries <- preds_mod |>
    dplyr::group_by(model, iter) |>
    dplyr::reframe(bounds_iter(dplyr::pick(dplyr::everything()), suitability_threshold)) |>
    dplyr::ungroup()


  ## Warnings ##

  failed_iters <- boundaries |>
    dplyr::filter(is.na(tval_left) | is.na(tval_right)) |>
    dplyr::group_by(model) |>
    dplyr::summarise(n_failed = dplyr::n()) |>
    dplyr::ungroup()

  if (nrow(failed_iters) > 0) {
    failed_warnings <- purrr::map2_chr(failed_iters$model, failed_iters$n_failed,
                                       ~ paste0("  - For model '", .x, "': ", .y, " curve(s) failed."))
    warning("Could not calculate boundaries for some curves:\n", paste(failed_warnings, collapse = "\n"))
  }

  ## temp values too high
  temp_50 <- boundaries |>
    dplyr::filter(tval_right >= 50)
  if (nrow(temp_50) > 0) {
    warning(nrow(temp_50), " curve(s) had an upper thermal boundary >= 50°C, which may be unrealistic.")
  }


  ## Output ##
  out <- boundaries |>
    # dplyr::filter(!is.na(tval_left)) |>
    dplyr::mutate(
      model_name = model,
      suitability = paste0(suitability_threshold, "%")
    ) |>
    dplyr::select(model_name, suitability, tval_left, tval_right, pred_suit, iter)

  return(out)

}




# calculate boundaries for each iteration
bounds_iter <- function(df = NULL, suit_threshold = NULL) {

  stopifnot(is.data.frame(df))
  stopifnot(is.numeric(suit_threshold))

  devrate_max <- max(df$dev_rate, na.rm = TRUE)

  # Return empty row if the curve is empty or flat
  if (nrow(df) < 2 || devrate_max <= 0) {
    return(dplyr::tibble(tval_left = NA_real_,
                         tval_right = NA_real_,
                         pred_suit = NA_real_))
  }

  q_threshold <- devrate_max * 0.01 * suit_threshold

  # Find all points on the curve that are above the suitability threshold
  suitable_points <- df |>
    dplyr::filter(dev_rate >= q_threshold)

  # If no points are above the threshold, no suitable range exists
  if (nrow(suitable_points) == 0) {
    return(dplyr::tibble(tval_left = NA_real_,
                         tval_right = NA_real_,
                         pred_suit = q_threshold))
  }

  # The boundaries are the minimum and maximum temperatures of this suitable range
  tval_left <- min(suitable_points$temp, na.rm = TRUE)
  tval_right <- max(suitable_points$temp, na.rm = TRUE)

  dplyr::tibble(
    tval_left = tval_left,
    tval_right = tval_right,
    pred_suit = q_threshold
  )

}
