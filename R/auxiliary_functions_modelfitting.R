

### script with auxiliary functions for fit_devmodels()
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
