### script with auxiliary functions for fit_devmodels()

## translate user-friendly models into dependencies
load(file = here::here("data/available_models.rda"))


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

##### TEST THIS:
start_vals_devRate <- function(model_name, temperature, dev_rate){
  mod2fit <- model_name  #avoid repeating name of available_models column `model_name`
  if (!mod2fit %in% dev_model_table$model_name) {
    stop("model name not available. Please check ?available_models")
  }
if(length(temperature) != length(dev_rate)){
  stop("lengths of temperature and dev_rate are not equal. Please check missing values or deletions")
}
  if (mod2fit == "briere1") {
      start_vals_prev <- c(tmin = 6, tmax = 32, a = 1e-04) # devRate start values are not representative, especially the a parameter, we use them manually
      names(start_vals_prev) <- startvals_names_translate_devrate(start_vals = start_vals_prev,
                                                                  mod2fit)
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

    warning("generic starting values")
    } else { start_vals_names <- extract_param_names(multstart_vals_fit)
    start_vals <- sum_start_vals_fit$parameters[1:length(start_vals_names), 1]
    start_vals_explore <- dplyr::tibble(param_name = start_vals_names,
                                         start_value = start_vals,
                                         model_name = mod2fit) |>
      dplyr::pull(start_value)
    }
  return(start_vals_explore)
}

start_vals_devRate(model_name = "regniere",
                   temperature = p.xylostella_liu2002$temperature,
                   dev_rate = p.xylostella_liu2002$rate_development)