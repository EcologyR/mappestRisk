### script with auxiliary functions for fit_devmodels()

#### a) working functions ----
model_name_translate <- function(user_model_name) {
  if (!all(user_model_name %in% c("all", dev_model_table$model_name))) {
    stop("model name not available. Please check ?available_models")
  }

  model_eq <- dev_model_table |>
    dplyr::filter(model_name == user_model_name) |>
    dplyr::select(source_model_name) |>
    dplyr::pull()

  return(model_eq)

}

# take names from a fitted model to assign them as names for start values later
extract_param_names <- function(nls_object){
  parameter_est <- coef(nls_object)
  param_names <- names(parameter_est)
  return(param_names)
}

## change names of parameters in devRate to easier argument names of parameters
startvals_names_translate_devrate <- function(start_vals, model_name){
  start_vals_names <- if(model_name == "briere1"){
    c("tmin", "tmax", "a")
  } else if (model_name == "lactin1"){
    c("a", "tmax", "delta_t")
  } else if (model_name == "lactin2"){
    c("a", "tmax", "delta_t", "b")
  } else if (model_name == "janisch"){
    c("dmin", "topt", "a", "b")
  } else if (model_name == "linear_campbell"){
    c("intercept", "slope")
  } else if (model_name == "wang"){
    c("k", "r", "topt", "tmin", "tmax", "a")
  } else if (model_name == "mod_polynomial") {
    c("a_0", "a_1", "a_2", "a_3", "a_4")
  } else if (model_name == "ssi") {
    c("p25", "a", "b", "c", "d", "e")
  } else if (model_name == "regniere") {
    c("tmin", "tmax", "phi", "delta_b", "delta_m", "b")
  }  else (NA)

  return(start_vals_names)
}

sim_tpc_gridparams <- function(grid_parameters, temperature, model_name){
  params_i <- grid_parameters
  model_i <- model_name
  model_eq <- dev_model_table |>
    filter(model_name == model_i)
  tpc_sim_i <- purrr::map(.x = temperature,
                          .f = reformulate(termlabels = unique(model_eq$params_formula))
  )
  tpc_sim_tbl <- tibble(temperature,
                        pred_devrate = tpc_sim_i) |>
    mutate(pred_devrate = unlist(pred_devrate))
  return(tpc_sim_tbl)
}

#### b) obtain start values for model fitting ----

## start_vals rTPC function, defined by the package (rTPC::get_start_vals) to adapt starting values to the input data

## start_vals devRate function & example. Since the devRate package does not provide a function for start values,
## we use nls2::nls2() function with literature starting values given in devRate packate to iteratively calculate parameters given the input data
## and use them later as starting values for nonlinear regression

#' start_vals_devRate(): obtain starting values for TPC fitting
#'
#' Auxiliary function working under `fit_devmodels()`; user won't be using this function in a normal workflow of [mappestRisk] package
#' @param model_name  one or several of the models listed in `dev_model_table` corresponding to
#' source package [devRate] to parameterise thermal performance curves
#' @param temperature a vector containing temperature treatments (predictor variable). It must have at least four different temperature treatments. The function works for both
#' aggregated data (i.e. one development rate value for each temperature treatment, which is representative of the cohort average development
#' rate) or individual data (i.e. one observation of development rate for each individual in the experiment at each temperature)
#'
#' @param  dev_rate a vector containing development rate estimates (1/days of development); must be of same length than temp.
#' The function works for both aggregated data (i.e. one development rate value for each temperature treatment, which is representive of the cohort average development
#' rate) or individual data (i.e. one observation of development rate for each individual in the experiment at each temperature)
#'
#'
#' @returns a vector with parameter estimates approximated iteratively with [nls.multstart::nls_multstart()] based on
#' estimates compilated in [devRate::devRateEqStartVal()].
#' @export
#'
#'
start_vals_devRate <- function(model_name, temperature, dev_rate){
  mod2fit <- model_name  #avoid repeating name of available_models column `model_name`
  if (!mod2fit %in% dev_model_table$model_name) {
    stop("model not available. For available model names, see `dev_model_table`")
  }
  if(any(is.na(dev_rate))) {
    stop("development rate data have NAs; please consider removing them or fixing them")
  }
  if(any(is.na(temperature))) {
    stop("temperature data have NAs; please consider removing them or fixing them")
  }
  if(any(!is.numeric(temperature))) {
    stop("temperature data is not numeric; please consider transforming it")
  }
  if(any(!is.numeric(dev_rate))) {
    stop("dev_rate data is not numeric; please consider transforming it")
  }
  if(any(is.na(dev_rate))) {
    stop("development rate data have NAs; please consider removing them or fixing them")
  }
  if(any(is.na(temperature))) {
    stop("temperature data have NAs; please consider removing them or fixing them")
  }
  if(length(unique(temperature)) < 4) {
    stop("At least four different temperature treatments in the data are required.")
  }
if(length(temperature) != length(dev_rate)){
  stop("development rate and temperature inputs are not of same length. Please check it.")
}
  if (mod2fit == "briere1") {
      start_vals_prev <- c(tmin = 6, tmax = 32, a = 1e-04) # devRate start values are not representative, especially the a parameter, we use them manually
      names(start_vals_prev) <- startvals_names_translate_devrate(start_vals = start_vals_prev,
                                                                  mod2fit)
      warning("briere1 start values are uninformative; default to `c(tmin = 6, tmax = 32, a = 1e-04)`")
    } else {
      start_vals_prev <- devRate::devRateEqStartVal[[model_name_translate(mod2fit)]] # take literature start values from devRate
      names(start_vals_prev) <- startvals_names_translate_devrate(start_vals = start_vals_prev,
                                                                  mod2fit)
    }
      start_upper_vals <- purrr::map(.x = start_vals_prev,
                                     .f = ~.x + abs(.x))
      start_lower_vals <- purrr::map(.x = start_vals_prev,
                                     .f = ~.x - abs(.x))
      devdata <- dplyr::tibble(temp = temperature,
                               rate_development = dev_rate)
      set.seed(2023) #ensure reproducibility
    multstart_vals_fit <- nls.multstart::nls_multstart(formula = reformulate(response = "rate_development",
                                                                                         termlabels = dev_model_table |>
                                                                                           dplyr::filter(model_name == mod2fit) |>
                                                                                           dplyr::pull(formula)),
                                                               data = devdata,
                                                               iter = 500,
                                                               start_lower = start_lower_vals,
                                                               start_upper = start_upper_vals,
                                                               supp_errors = "Y")
    sum_start_vals_fit <- summary(multstart_vals_fit)
    if(is.null(multstart_vals_fit)){
      start_vals_explore <- dplyr::tibble(param_name = names(start_vals_prev),
                                                             start_value = unlist(start_vals_prev),
                                                             model_name = mod2fit) |>
      dplyr::pull(start_value)

    message("generic starting values")
    } else { start_vals_names <- extract_param_names(multstart_vals_fit)
    start_vals <- sum_start_vals_fit$parameters[1:length(start_vals_names), 1]
    start_vals_explore <- dplyr::tibble(param_name = start_vals_names,
                                         start_value = start_vals,
                                         model_name = mod2fit) |>
      dplyr::pull(start_value)
    }
  return(start_vals_explore)
}

#### c) Simulate and Plot Thermal Performance Curves' Uncertainties ----

#' Plot predictions of fitted thermal performance curves to your data propagating parameter uncertainty
#'
#' The function`sim_tpcs_uncertainty()` displays a faceted plot of fitted TPCs
#' to help the user to select an appropriate model based both on statistical and ecological criteria.
#'
#' @param fitted_parameters a `tibble` obtained with `fit_devmodels()` function including parameter names,
#'  estimates, se, AICs and gnls objects (i.e. fitted_models) from `fit_devmodels()`.
#'
#' @param model_name "all" or alternatively one or several of the models listed in `?available_models`
#' to parameterise thermal performance curves.
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
#' @param species_name a character string indicating the name of the species for plot title
#'
#' @seealso `fit_devmodels()` for fitting Thermal Performance Curves to development rate data
#'
#' @returns a ggplot with the simulated thermal performance curves based on estimate and standard error of
#' fitted parameters of the models with `fit_devmodels()`. The plot depicts the central estimate in cyan color and
#' 1,000 TPCs simulating the uncertainty band in light gray color.
#'
#' @export
#'
#' @examples
#' data("h.vitripennis_pilkington2014")
#' homalodisca_fitted <- fit_devmodels(temp = h.vitripennis_pilkington2014$temperature,
#'                                     dev_rate = h.vitripennis_pilkington2014$rate_development,
#'                                     model_name = c("all"),
#'                                     variance_model = "exp") #might be a bit slow
#'
#' sim_tpcs_uncertainty(fitted_parameters = fit_table,
#'                      model_name = "briere1",
#'                      temp = h.vitripennis_pilkington2014$temperature,
#'                      dev_rate = h.vitripennis_pilkington2014$rate_development,
#'                      species_name = "glassy-winged sharpshooter")

sim_tpcs_uncertainty <- function(fitted_parameters,
                                    model_name,
                                    temp,
                                    dev_rate,
                                    species_name = NULL) {

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
  if(suppressWarnings(any(!c("param_name", "start_vals", "param_est", "param_se", "model_name", "model_AIC", "model_fit", "fit") %in% colnames(fitted_parameters)))){
    stop("fitted_parameters` must be a  data.frame inherited from the output of `mappestRisk::fit_devmodels()` function.
    No modifications of columns of the fitted_parameters data.frame are allowed, but you can subset observations by filtering
    or subsetting by rows if desired.")
  }
    fitted_tbl <- fitted_parameters |>
      tidyr::drop_na()
  if(nrow(fitted_tbl) == 0){
    stop("no model has converged in your `fitted_parameters` data.frame. Is it the appropriate object coming from converged
    `fit_devmodels()`?")
  }
  if(is.null(species_name)){
    species_name <- " " }
  model_f <- model_name
  model_ext_list <- fitted_parameters |>
    filter(model_name == model_f) |>
    slice(1) |>
    pull(model_fit)
  model_extracted <- model_ext_list[[1]]
  sum_model<- summary(model_extracted)
  n_parameters_model <- length(sum_model$coefficients)
  params_model <- sum_model$tTable[,1]
  ## avoid extremely costly simulations
  n_sims <- case_when(n_parameters_model == 2 ~ 50,
                      n_parameters_model == 3 ~ 10,
                      n_parameters_model == 4 ~ 6,
                      n_parameters_model == 5 ~ 5,
                      n_parameters_model == 6 ~ 4,
                      n_parameters_model == 7 ~ 3)

  allparams_sim_list <- list(simulations = NULL)
  for(parameter_index in 1:n_parameters_model){
    param_i_est <- sum_model$tTable[parameter_index, 1]
    param_i_se <- sum_model$tTable[parameter_index, 2]
    param_i_tbl <- tibble(estimate = param_i_est,
                          se = param_i_se)
    params_tbl_i <- rnorm(n = n_sims,
                          mean = param_i_tbl$estimate,
                          sd = param_i_tbl$se
    )
    allparams_sim_list[[parameter_index]] <- params_tbl_i
  }
  params_grid_raw <- expand.grid(allparams_sim_list)
  set.seed(2023)
  params_grid <- params_grid_raw |>
    slice_sample(n = 1000)
  params_names <- extract_param_names(model_extracted)
  colnames(params_grid) <- params_names
  all_tpcs_sim <- tibble(temperature = NULL,
                         dev_rate = NULL,
                         n_sim = NULL)


  for(n_sim_curve in c(1:nrow(params_grid))){
    print(paste0("Simulating Uncertainty TPC:","   ", n_sim_curve,"/",nrow(params_grid)))
    params_grid_i <- params_grid |>
      slice(n_sim_curve)
    curve_sim_i <- sim_tpc_gridparams(grid_parameters = params_grid_i,
                                      temperature = seq(min(temp)-5,
                                                        max(temp) +5,
                                                        .1),
                                      model_name = model_f)
    curve_sim_isim <- curve_sim_i |>
      mutate(n_sim = n_sim_curve,
             color = "gray78",
             linewidth = 0.5,
             alpha = 0.15)
    all_tpcs_sim <- all_tpcs_sim |>
      bind_rows(curve_sim_isim)
  }
  central_tpc <- sim_tpc_gridparams(grid_parameters = params_model,
                                    temperature = seq(min(h.vitripennis_pilkington2014$temperature -5),
                                                      max(h.vitripennis_pilkington2014$temperature +5),
                                                      0.1),
                                    model_name = model_f) |>
    mutate(color = "darkcyan",
           linewidth = 1.5,
           alpha = 1)
  simulated_tpcs <- bind_rows(all_tpcs_sim,
                              central_tpc)

  plot_all_curves <- ggplot()+
    xlim(c(min(temp) - 9,
           max(temp) + 9))+
    geom_line(data = simulated_tpcs,
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
    labs(title = paste("TPC simulation:", species_name),
         subtitle = model_f,
         x = "Temperature (ºC)",
         y = "Development Rate (1/days)")
  print("Please wait a moment for the TPC to be plotted :)")
  return(plot_all_curves)
  }

