### script with auxiliary functions for fit_devmodels()

is_positive_integer <- function(x) {
  is.numeric(x) && x > 0 && x == as.integer(x)
}

crop_palette <- function(palette_vector, n_breaks) {
  palette_flex <- palette_vector[1:(n_breaks+1)]
  return(palette_flex)
}

#### a) working functions ----
model_name_translate <- function(user_model_name) {
  if (!all(user_model_name %in% c("all", available_models$model_name))) {
    stop("model name not available. Please check ?available_models")
  }

  model_eq <- available_models |>
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
    c("a", "tmin", "tmax")
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
  model_eq <- available_models |>
    dplyr::filter(model_name == model_i)
  tpc_sim_i <- purrr::map(.x = temperature,
                          .f = reformulate(termlabels = unique(model_eq$params_formula))
  )
  tpc_sim_tbl <- dplyr::tibble(temperature,
                               pred_devrate = tpc_sim_i) |>
    dplyr::mutate(pred_devrate = unlist(pred_devrate))
  return(tpc_sim_tbl)
}

#### b) obtain start values for model fitting ----

start_vals_devRate <- function (model_name_2fit, temperature, dev_rate) {
  check_data(temp = temperature,
             dev_rate)

  if (model_name_2fit$model_name == "briere1") {
    start_vals_explore <- c(a = 2e-04, tmin = 8, tmax = 32)
  message("Poorly informative start values for briere1 model")
  } else {
    model_name_devrate <- model_name_2fit$source_model_name
    devdata <- dplyr::tibble(temp = temperature,
                             rate_development = dev_rate)
    start_vals_prev <- devRate::devRateEqStartVal[[model_name_devrate]]
    names(start_vals_prev) <- startvals_names_translate_devrate(start_vals_prev,
                                                                model_name = model_name_2fit$model_name)
    start_upper_vals <- purrr::map(.x = start_vals_prev,
                                   .f = ~.x + abs(.x/2))
    start_lower_vals <- purrr::map(.x = start_vals_prev,
                                   .f = ~.x - abs(.x/2))

    multstart_vals_fit <- nls.multstart::nls_multstart(formula = reformulate(response = "rate_development",
                                                                             termlabels = model_name_2fit |>
                                                                               dplyr::pull(formula)),
                                                       data = devdata,
                                                       iter = 500,
                                                       start_lower = start_lower_vals,
                                                       start_upper = start_upper_vals,
                                                       supp_errors = "Y")
    sum_start_vals_fit <- summary(multstart_vals_fit)

    if (is.null(multstart_vals_fit)) {
      start_vals_explore <- dplyr::tibble(param_name = names(start_vals_prev),
                                          start_value = unlist(start_vals_prev),
                                          model_name = model_name_2fit$model_name) |>
        dplyr::pull(start_value)

      message("generic starting values")
    } else { start_vals_names <- extract_param_names(multstart_vals_fit)
    start_vals <- sum_start_vals_fit$parameters[1:length(start_vals_names), 1]
    start_vals_explore <- dplyr::tibble(param_name = start_vals_names,
                                        start_value = start_vals,
                                        model_name = model_name_2fit$model_name) |>
      dplyr::pull(start_value)
    }
  }
  return(start_vals_explore)
}

