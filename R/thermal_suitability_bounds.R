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

thermal_suitability_bounds <- function(fitted_parameters,
                                       model_name,
                                       temp,
                                       dev_rate,
                                       suitability_threshold = NULL) {
  fitted_parameters_nonas <- fitted_parameters |>
    tidyr::drop_na()
  fitted_param_names <- fitted_parameters_nonas |>
    distinct(model_name) |>
    pull(model_name)
  if (!is.data.frame(fitted_parameters) |
      suppressWarnings(any(!c("param_name", "start_vals",
                              "param_est", "param_se",
                              "model_name", "model_AIC",
                              "model_fit", "fit") %in% colnames(fitted_parameters)))) {
    stop("`fitted_parameters` must be a  data.frame inherited   from the output of `mappestRisk::fit_devmodels()` function.
  No modifications of columns of the fitted_parameters data.frame are allowed, but you can subset observations by filter(ing
  or subsetting by rows if desired.")
  }
  if (nrow(fitted_parameters) == 0) {
    stop("The fitted_parameters table is NULL; use `fit_devmodels()` to check that at least one model converged.")
  }
  if(is.data.frame(fitted_parameters) && is.null(suitability_threshold)){
    suitability_threshold <- 50
    message("No suitability_threshold value input. Using by default suitability_threshold = 50%")
  }
  if(suitability_threshold < 50) {
    stop("Suitability must be higher than 50% in order to have applied sense. If set to NULL, suitability_threshold = 50% by default")
  }
  if (!is.null(model_name) && any(!model_name %in% dev_model_table$model_name)) {
    stop("Model name not available. For available model names, see `dev_model_table`.")
  }
  if(!is.null(model_name) && any(!model_name %in% fitted_param_names)) {
    stop(paste("Model", model_name[which(!model_name %in% fitted_param_names)], "did not fitted well to your data. Try using another fitted model in your table instead"))
  }
  if(!is.null(model_name) && any(model_name == "linear_campbell")) {
    stop("Thermal Suitability predictions require nonlinear models. Try another fitted model in your `fitted_parameters` table instead")
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
  fitted_parameters_nonas <- fitted_parameters
  model2fit <- model_name
  tvals <- tibble::tibble(model_name = NULL,
                          tval_left = NULL,
                          tval_right = NULL,
                          suitability = NULL)
  for(i in model2fit){
    model_i <- dev_model_table |>
      dplyr::filter(model_name == i)
    params_i <- fitted_parameters_nonas |>
      dplyr::filter(model_name == i)  |>
      dplyr::pull(param_est)

    ##predict based on parameters
    explore_preds <- dplyr::tibble(temp = seq(0,50, 0.001),
                                   model_name = i,
                                   preds = NULL,
    )
    fit_vals_tbl <- explore_preds |>
      dplyr::mutate(formula = model_i$params_formula) |>
      dplyr::mutate(preds = purrr::map_dbl(.x = temp,
                                           .f = reformulate(unique(formula)))) |>
      dplyr::filter(preds >= 0) |>
      dplyr::select(-formula) |>
      dplyr::mutate(preds = dplyr::case_when(model_name == "ratkowsky" & temp > params_i[2] ~ NA_real_,
                                             model_name == "ratkowsky" & temp < params_i[1] ~ NA_real_,
                                             model_name == "briere1" & temp < params_i[1] ~ NA_real_,
                                             model_name == "briere2" & temp < params_i[1] ~ NA_real_,
                                             TRUE ~ preds)
      ) # to exclude biological non-sense predictions due to model mathematical properties
    possible_error <- tryCatch(expr =
                                 suppressWarnings({topt_pred <- fit_vals_tbl |> #the custom error message is more informative than this warning
                                   slice_max(preds) |>
                                   pull(temp)
                                 devrate_max <- fit_vals_tbl |>
                                   dplyr::slice_max(preds) |>
                                   dplyr::pull(preds)
                                 half_left <- fit_vals_tbl |>
                                   dplyr::filter(temp < topt_pred)
                                 half_right <- fit_vals_tbl |>
                                   dplyr::filter(temp >= topt_pred)
                                 therm_suit_left <- half_left |>
                                   dplyr::slice(max(which(half_left$preds <= devrate_max*0.01*suitability_threshold), na.rm = TRUE)) |>
                                   dplyr::pull(temp)
                                 therm_suit_right <- half_right |>
                                   dplyr::slice(min(which(half_right$preds <= devrate_max*0.01*suitability_threshold), na.rm = TRUE)) |>
                                   dplyr::pull(temp)
                                 }),
                               error = function(e) e)
    if(inherits(possible_error, "error")) {
      therm_suit_right <- NA
      therm_suit_left <- NA
    }
    if(is.na(therm_suit_right) |
       is.na(therm_suit_left)) {
      warning(paste("Model", i, "is not appropriate to model thermal suitability. Try another instead (use `plot_devmodel()` to see curve shapes)."))
    }
    tvals_i <- dplyr::tibble(model_name = i,
                             tval_left = therm_suit_left,
                             tval_right = therm_suit_right,
                             suitability = paste(suitability_threshold, "%"))
    tvals <- dplyr::bind_rows(tvals, tvals_i)
  } # <- loop ends
  if(any(tvals$tval_right >= 50, na.rm = TRUE))
  { warning("upper value of thermal suitability  might be non-realistic")
  }
  dev_rate_suitable <- max(fit_vals_tbl$preds, na.rm = TRUE)*0.01*suitability_threshold
  cband_tpcs  <- sim_tpcs_uncertainty(fitted_parameters = fitted_parameters,
                                      model_name = model_name,
                                      temp = temp,
                                      dev_rate = dev_rate)
  n_sim <- cband_tpcs |> distinct(n_sim) |> pull(n_sim) |> length()

  plot_all_curves <- ggplot()+
    xlim(c(min(temp) - 9,
           max(temp) + 9))+
    geom_line(data = cband_tpcs,
              aes(x = temperature,
                  y = pred_devrate,
                  color = as_factor(color),
                  linewidth = linewidth,
                  group = n_sim,
                  alpha = alpha))+
    ggthemes::theme_clean()+
    scale_color_identity()+
    scale_alpha_identity()+
    scale_linewidth_identity()+
    labs(title = paste("TPC:", model_name),
         subtitle = paste0("Suitability Threshold: Q", suitability_threshold),
         x = "Temperature (ºC)",
         y = "Development Rate (1/days)")+
  annotate(geom = "segment",
           y = dev_rate_suitable,
           yend = dev_rate_suitable,
           x = tvals$tval_left,
           xend = tvals$tval_right,
           color = "goldenrod1",
           linewidth = 1.2)+
    annotate(geom = "point",
             color = "#ff006e",
             x = tvals$tval_left,
             y = dev_rate_suitable,
             size = 3)+
    annotate(geom = "point",
             color = "#ff006e",
             x = tvals$tval_right,
             y = dev_rate_suitable,
             size = 3)+
    annotate(geom = "segment",
             x = tvals$tval_left,
             xend = tvals$tval_left,
             y = 0,
             yend = dev_rate_suitable,
             color = "#ff006e",
             linetype = "dashed")+
    annotate(geom = "segment",
             x = tvals$tval_right,
             xend = tvals$tval_right,
             y = 0,
             yend = dev_rate_suitable,
             color = "#ff006e",
             linetype = "dashed")+
    annotate(geom = "point",
             color = "#ff006e",
             x = tvals$tval_left,
             y = 0,
             size = 3)+
    annotate(geom = "point",
             color = "#ff006e",
             x = tvals$tval_right,
             y = 0,
             size = 3)
  print(plot_all_curves)

  tvals_bounds <- tvals |> mutate(plot_uncertainty = list(plot_all_curves))
  return(tvals_bounds)

}

