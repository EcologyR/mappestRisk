#' Obtain thermal traits representing boundaries of optimal thermal suitability for pest risk occurrence assessment
#'
#' Similar to calculating thermal breadth, `thermal_suitability_bounds()` computes the temperature bounds delimiting the optimum performance
#' as defined by user's suitability threshold.

#' @param fitted_parameters a `tibble` obtained with `fit_devmodels()` function including parameter names,
#'  estimates, se, AICs and gnls objects (i.e. fitted_models) from `fit_devmodels()`.
#'
#' @param model_name one or several of the models in fitted_parameters.
#' Must belong to available models in `dev_model_table`.
#'
#' @param suitability_threshold Quantile of the curve; e.g. 75 would give
#'  the temperatures within the values at which development rate is in the
#'  top 25% of its possible values. Def. 50. Note that `suitability_threshold < 50` is not allowed.
#'
#' @returns a tibble/data.frame with name of the model and two thermal traits representing the above-mentioned suitability boundaries
#' @export
#'
#'
#'
#' @examples
#' data("h.vitripennis_pilkington2014")
#' homalodisca_fitted <- fit_devmodels(temp = h.vitripennis_pilkington2014$temperature,
#'                                     dev_rate = h.vitripennis_pilkington2014$rate_development,
#'                                     model_name = c("all"),
#'                                     variance_model = "exp") #might be a bit slow
#'
#' ## examine plots to decide an appropriate model
#' plot_devmodels(temp = h.vitripennis_pilkington2014$temperature,
#'                dev_rate = h.vitripennis_pilkington2014$rate_development,
#'                fitted_parameters = homalodisca_fitted)
#'
#' thermal_suitability_bounds(fitted_parameters = homalodisca_fitted,
#'                            model_name = "briere1", # <- seems realistic
#'                            suitability_threshold = 75)
#'

thermal_suitability_bounds <- function(pred_tbl,
                                       model_name,
                                       suitability_threshold = NULL) {

  #  if (!is.data.frame(fitted_parameters) |
  #      suppressWarnings(any(!c("param_name", "start_vals",
  #                              "param_est", "param_se",
  #                              "model_name", "model_AIC",
  #                              "model_fit", "fit") %in% colnames(fitted_parameters)))) {
  #    stop("`fitted_parameters` must be a  data.frame inherited   from the output of `mappestRisk::fit_devmodels()` function.
  #  No modifications of columns of the fitted_parameters data.frame are allowed, but you can subset observations by filter(ing
  #  or subsetting by rows if desired.")
  #  }
  #  if (nrow(fitted_parameters) == 0) {
  #    stop("The fitted_parameters table is NULL; use `fit_devmodels()` to check that at least one model converged.")
  #  }
  #  if(is.data.frame(fitted_parameters) && is.null(suitability_threshold)){
  #    suitability_threshold <- 50
  #    message("No suitability_threshold value input. Using by default suitability_threshold = 50%")
  #  }
  #  if(suitability_threshold < 50) {
  #    stop("Suitability must be higher than 50% in order to have applied sense. If set to NULL, suitability_threshold = 50% by default")
  #  }
  #  if (!is.null(model_name) && any(!model_name %in% dev_model_table$model_name)) {
  #    stop("Model name not available. For available model names, see `dev_model_table`.")
  #  }
  #  if(!is.null(model_name) && any(!model_name %in% fitted_param_names)) {
  #    stop(paste("Model", model_name[which(!model_name %in% fitted_param_names)], "did not fitted well to your data. Try using another fitted model in your table instead"))
  #  }
  #  if(!is.null(model_name) && any(model_name == "linear_campbell")) {
  #    stop("Thermal Suitability predictions require nonlinear models. Try another fitted model in your `fitted_parameters` table instead")
  #  }
  #  if(length(model_name) > 1 || model_name == "all") {
  #    stop("Only one model is allowed in `thermal_suitability_bounds()`. Please calculate thermal boundaries one by one and apply repeatedly this function as many times as desired.")
  #  }
  #  if(any(is.na(dev_rate))) {
  #    stop("development rate data have NAs; please consider removing them or fixing them")
  #  }
  #  if (any(is.na(temp))) {
  #    stop("temperature data have NAs; please consider removing them or fixing them")
  #  }
  #  if (!is.numeric(temp)) {
  #    stop("temperature data is not numeric. Please check it.")
  #  }
  #  if (!is.numeric(dev_rate)) {
  #    stop("development rate data is not numeric. Please check it.")
  #  }
  #  if (length(temp) != length(dev_rate)) {
  #    stop("development rate and temperature inputs are not of same length. Please check it.")
  #  }
  #  if (any(dev_rate < 0) | any(dev_rate > 10)){
  #    warning("negative or extremely high values of dev_rate development rate data might contain a typo error. Please check it.")
  #  }
  #  if(any(temp < -10) | any(temp > 56)) {
  #    warning("experienced temperatures by active organisms (i.e. not in diapause) are usually between 0 and 50ºC")
  #  }
  #
  #
  tvals <- dplyr::tibble(model_name = model_name,
                         tval_left = NULL,
                         tval_right = NULL,
                         pred_suit = NULL,
                         suitability = paste(suitability_threshold, "%"),
                         iter = NULL)

  for(iter_i in unique(pred_tbl$iter)){
    pred_tbl_i <- pred_tbl[pred_tbl$iter == iter_i, ]
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
                             iter = iter_i)
    if(any(tvals$tval_right >= 50, na.rm = TRUE))
    { warning("upper value of thermal suitability  might be non-realistic")
    }
    tvals <- bind_rows(tvals,
                       tvals_i)
  }
  return(tvals)
}


example_therm_bounds <- thermal_suitability_bounds(pred_tbl = boots_thrips,
                                                   model_name <- "lactin1",
                                                   suitability_threshold = 80) |>
  drop_na()
