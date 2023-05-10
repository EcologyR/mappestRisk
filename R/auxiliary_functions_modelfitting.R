
model_name_translate <- function(user_model_name) {

  if (!user_model_name %in% available_models$model_name) {
    stop("model name not available. Please check ?available_models ")
  }

  model_eq <- available_models |>
    dplyr::filter(model_name == user_model_name) |>
    dplyr::select(source_model_name) |>
    dplyr::pull()

  return(model_eq)

}

# take names from a fitted model to assign them as names for start values later
#### NO TEST
extract_param_names <- function(nls_object){
  parameter_est <- coef(nls_object)
  param_names <- names(parameter_est)
  return(param_names)
}

## change names of parameters in devRate to easier argument names of parameters

## NO TEST
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
  } else (NA)

  return(start_vals_names)
}

#### b) obtain start values for model fitting ----

## start_vals rTPC function, defined by the package (rTPC::get_start_vals) to adapt starting values to the input data

## start_vals devRate function & example. Since the devRate package does not provide a function for start values,
## we use nls2::nls2() function with literature starting values given in devRate packate to iteratively calculate parameters given the input data
## and use them later as starting values for nonlinear regression

##### TEST THIS:
start_vals_devRate <- function(model_name, temperature, dev_rate){
  if (model_name == "wang"){ ## it has 6 parameters, which is excessively time costly, we'll accept uisng the generic literature start values from devRate package
    start_vals <- devRate::devRateEqStartVal[[model_name_translate(model_name)]] # take literature start values from devRate
    names(start_vals) <- startvals_names_translate_devrate(start_vals = start_vals,
                                                           model_name)
    start_vals_explore <- dplyr::tibble(param_name = c("k", "r", "topt", "tmin", "tmax", "a"),
                                         start_value = unlist(start_vals),
                                         model_name = model_name) |>
      dplyr::pull(start_value)
    print("poorly informative starting values for Wang fitting")

  } else {
    if (model_name == "briere1") {
      start_vals_prev <- c(tmin = 6, tmax = 32, a = 1e-04) # devRate start values are not representative, especially the a parameter, we use them manually
      names(start_vals_prev) <- startvals_names_translate_devrate(start_vals = start_vals_prev,
                                                                  model_name)
    } else {
      start_vals_prev <- devRate::devRateEqStartVal[[model_name_translate(model_name)]] # take literature start values from devRate
      names(start_vals_prev) <- startvals_names_translate_devrate(start_vals = start_vals_prev,
                                                                  model_name)
    }
    set.seed(2023) #to ensure reproducibility
    grid_startvals <- purrr::map(.x = start_vals_prev,
                                 .f = ~runif(10, min = .x - .x/2,
                                      max = .x + .x/2)) #grid with different combinations of possible starting values to expand next
    exp_grid_startvals <- expand.grid(grid_startvals) #expanded grid with combinatory to iteratively adjust models
    devdata <- tibble::tibble(temp = temperature,
                              dev_rate = dev_rate,
                              model_name = model_name,
                              formula = case_when(model_name == "briere1" ~  "briere1(temp, tmin, tmax, a)",
                                                  model_name == "lactin1" ~ "lactin1(temp, a, tmax, delta_t)",
                                                  model_name == "lactin2" ~ "lactin2(temp, a, tmax, delta_t, b)",
                                                  model_name == "janisch" ~ "janisch(temp, dmin, topt, a, b)",
                                                  model_name == "linear_campbell" ~ "linear_campbell(temp,intercept, slope)",
                                                  model_name == "wang" ~ "wang(temp, k, r, topt, tmin, tmax, a)",
                                                  model_name == "mod_polynomial" ~ "mod_polynomial(temp, a_0, a_1, a_2, a_3, a_4)"
                              )
    )
    capture.output(type = "message",
                   start_vals_fit <- message("warning: model did not converged adequately. Try other models instead from available_models" #try(object, silent = TRUE) to avoid printing usual uninformative errors
    ))
    sum_start_vals_fit <- summary(start_vals_fit)
    if(is.null(start_vals_fit)){start_vals_explore <- dplyr::tibble(param_name = names(start_vals_prev),
                                                                     start_value = unlist(start_vals_prev),
                                                                     model_name = model_name) |>
      dplyr::pull(start_value)

    warning("generic starting values")
    }
    else {start_vals_names <- extract_param_names(start_vals_fit)
    start_vals <- sum_start_vals_fit$parameters[1:length(start_vals_names),1]
    start_vals_explore <- dplyr::tibble(param_name = start_vals_names,
                                         start_value = start_vals,
                                         model_name = model_name) |>
      dplyr::pull(start_value)}
  }
  return(start_vals_explore)
}

