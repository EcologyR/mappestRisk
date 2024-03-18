#' Plot predictions of fitted thermal performance curves to your data
#'
#' The function`plot_devmodels()` displays a faceted plot of fitted TPCs
#' to help the user to select an appropriate model based both on statistical and ecological criteria.
#'
#' @param temp a vector containing temperature treatments (predictor variable),
#'  must have at least three different temperature treatments. The function works for both
#'  aggregated data (i.e. one development rate value for each temperature treatment, which is representative of the cohort average development
#'  rate) or individual data (i.e. one observation of development rate for each individual in the experiment at each temperature)
#'
#' @param dev_rate a vector containing development rate estimates (1/days of development); must be of same length than temp.
#'  The function works for both aggregated data (i.e. one development rate value for each temperature treatment, which is representative of the cohort average development
#'  rate) or individual data (i.e. one observation of development rate for each individual in the experiment at each temperature)
#'
#' @param fitted_parameters a `tibble` obtained with `fit_devmodels()` function including parameter names,
#'  estimates, se, AICs and gnls objects (i.e. fitted_models) from `fit_devmodels()`.
#'
#' @returns takes the fitted parameters table from `fit_devmodels()` and returns a plot with the predicted values -i.e. development rate-
#' across temperatures each for the models that adequately converged. Facets of the resulting plots are automatically sorted by lowest AIC values in descending order
#' and more information such as number of parameters are displayed.
#'
#' @seealso `fit_devmodels()` for fitting Thermal Performance Curves to development rate data
#'
#' @export
#'
#' @examples
#'
#' data("h.vitripennis_pilkington2014")
#' homalodisca_fitted <- fit_devmodels(temp = h.vitripennis_pilkington2014$temperature,
#'                                     dev_rate = h.vitripennis_pilkington2014$rate_development,
#'                                     model_name = c("all"),
#'                                     variance_model = "exp") #might be a bit slow
#'
#' plot_devmodels(temp = h.vitripennis_pilkington2014$temperature,
#'                dev_rate = h.vitripennis_pilkington2014$rate_development,
#'                fitted_parameters = homalodisca_fitted)
#'

plot_devmodels <- function(temp, dev_rate, fitted_parameters,
                           species = NULL,
                           life_stage = NULL) {
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
  if (is.null(fitted_parameters)) {
    stop("`fitted_parameters` is NULL; use `mappestRisk::fit_devmodels()` to check that at least one model converged")
  }
  if(!is.data.frame(fitted_parameters)) {
    stop("fitted_parameters` must be a  data.frame inherited from the output of `mappestRisk::fit_devmodels()` function.
    No modifications of columns of the fitted_parameters data.frame are allowed, but you can subset observations by filtering
    or subsetting by rows if desired.")
  }
  if(suppressWarnings(any(!c("param_name", "start_vals", "param_est", "param_se", "model_name", "model_AIC", "model_fit") %in% colnames(fitted_parameters)))){
    stop("fitted_parameters` must be a  data.frame inherited from the output of `mappestRisk::fit_devmodels()` function.
    No modifications of columns of the fitted_parameters data.frame are allowed, but you can subset observations by filtering
    or subsetting by rows if desired.")
  }
  devdata <- tibble(temperature = temp,
                    development_rate = dev_rate)
  fitted_tbl <- fitted_parameters # |> tidyr::drop_na()
  if(nrow(fitted_tbl) == 0){
    stop("no model has converged in your `fitted_parameters` data.frame. Is it the appropriate object coming from converged
    `fit_devmodels()`?")
  }
  predict2fill <- tibble(temp = NULL,
                         dev_rate = NULL,
                         model_name = NULL,
                         model_AIC = NULL)
  model_names2plot <- fitted_tbl |>
    distinct(model_name) |>
    pull(model_name)

  for(i in model_names2plot){
    fitted_tbl_i <- fitted_tbl |>
      filter(model_name == i)
    model_AIC_i <-fitted_tbl_i |>
      pull(model_AIC)
    params_i <- fitted_tbl_i |>
      pull(param_est)
    formula_i <- dev_model_table |> filter(model_name == i) |>
      pull(working_formula)
    ##predict based on parameters
    explore_preds <- tibble(temp = seq(min(devdata$temperature)-15,
                                       max(devdata$temperature) +15,
                                       .01),
                            model_name = i,
                            model_AIC = model_AIC_i[1],
                            preds = NULL,
                            n_params = length(params_i))
    fit_vals_tbl <- explore_preds |>
      dplyr::select(temp, model_name, model_AIC, n_params) |>
      mutate(formula = formula_i) |>
      mutate(preds = purrr::map_dbl(.x = temp,
                                    .f = reformulate(unique(formula_i)))) |>
      filter(preds >= 0) |>
      dplyr::select(-formula) |>
      mutate(preds = case_when(model_name == "ratkowsky" & temp > params_i[2] ~ NA_real_,
                               model_name == "ratkowsky" & temp < params_i[1] ~ NA_real_,
                               model_name == "briere1" & temp < params_i[1] ~ NA_real_,
                               model_name == "briere2" & temp < params_i[1] ~ NA_real_,
                               TRUE ~ preds)
      ) # to exclude biological non-sense predictions due to model mathematical properties
    predict2fill <- predict2fill |>
      bind_rows(fit_vals_tbl)
  }

  aic_text <-  predict2fill  |>
    group_by(model_name)  |>
    summarise(aic = mean(model_AIC),
              n_params = paste(mean(n_params), "parameters"))  |>
    arrange(aic)
  aic_order <- aic_text  |>
    pull(model_name)
  aic_values <- aic_text |>
    mutate(aic =   paste("AIC =",
                         round(aic, 2)),
           temp = min(devdata$temperature),
           preds = 1.5*max(devdata$development_rate))
  if(max(predict2fill$preds, na.rm = TRUE) > 5* max(devdata$development_rate, na.rm = TRUE) | max(devdata$development_rate, na.rm = TRUE) > 5* max(predict2fill$preds, na.rm = TRUE)) {
    warning("scale in the plot might not be appropriate to your data or at least for some points or some regions of the curve.
    Please consider to check out the input of `dev_rate`")
  }
  my_title <- substitute(italic(paste(especie)), list(especie = species))
  ggplot_models <- ggplot()+
    geom_line(data = predict2fill |>
                filter(preds < (1.5*max(devdata$development_rate))),
              aes(x = temp, y = preds, color = model_name),
              linewidth = 1.3)+
    geom_point(data = devdata, aes(x = temperature,
                                   y = development_rate),
               color = "darkslategray",
               alpha = .8,
               size = 1.5)+
    facet_wrap(~factor(model_name, levels = aic_order))+
    theme_bw()+
    theme(legend.position = "none")+
    labs(x = "Temperature (ºC)",
         y = expression(italic(R(T))~(d^-1)),
         title = my_title,
         subtitle = life_stage)+
    geom_label(data = aic_values,
               aes(label = aic,
                   x = temp,
                   y = preds,
                   fill = model_name),
               color = "white",
               size = 3)+
    geom_label(data = aic_values,
               aes(x = temp,
                   y = preds-preds/8,
                   label = n_params,
                   fill = model_name),
               color = "white",
               size = 3)
  return(ggplot_models)
}

