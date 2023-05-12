#' Fit nonlinear regression models to development rate data across temperatures (i.e. Thermal Performance Curves)
#'
#' @param temp a vector containing temperature treatments (predictor variable),
#' must have at least three different temperature treatments. The function works for both
#' aggregated data (i.e. one development rate value for each temperature treatment, which is representive of the cohort average development
#' rate) or individual data (i.e. one observation of development rate for each individual in the experiment at each temperature)
#'
#' @param dev_rate a vector containing development rate estimates (1/days of development); must be of same length than temp.
#' The function works for both aggregated data (i.e. one development rate value for each temperature treatment, which is representive of the cohort average development
#' rate) or individual data (i.e. one observation of development rate for each individual in the experiment at each temperature)
#'
#' @param model_name "all" or alternatively one or several of the models listed in `?available_models`
#'
#' @return this function returns a tibble with estimate and standard error for each parameter of the models from the user call
#' that have adequately converged to the data. It also shows an AIC value and a comment on those models whose parameter uncertainty
#' is high (`fit = "bad"` in the tibble). Fitted models are included in list format, and can be accessed
#' via `your_parameters_tbl$fit[[x]]` with `x` being the desired row in the table.
#' For model selection, also ecological criteria should be followed by the user. To help that purpose,
#' we recommend use¡ing `plot_devmodels()` and look into the literature rather than focusing only on statistical information.
#'
#' #' @export
#'
#' @examples
#' data(p.xylostella_liu2002)
#' data(available_models)
#'
#' cabbage_moth_fitted <- fit_devmodels(temp = p.xylostella_liu2002$temperature,
#'                                      dev_rate = p.xylostella_liu2002$rate_development,
#'                                      model_name = c("all")) #might be a bit slow
#'print(cabbage_moth_fitted)

fit_devmodels <- function(temp = NULL,
                          dev_rate = NULL,
                          model_name = NULL){

  if(!is.numeric(temp)) {
    stop("temperature data is not numeric. Please check it.")
  }
  if(length(unique(temp)) <= 3 ) {
    stop("fit_devmodels() require at least three different temperature treatments in the data")
  }
  if(!is.numeric(dev_rate)) {
    stop("development rate data is not numeric. Please check it.")
  }
  if(length(temp) != length(dev_rate)) {
    stop("development rate and temperature inputs are not of same length. Please check it.")
  }
  if(!is.character(model_name)){
    stop("model_name must be a string in ?available_models")}

  if (!all(model_name %in% c("all", dev_model_table$model_name))) {
    stop("model not available. For available model names, see ?available_models")
  }
  if (any(dev_rate < 0) | any(dev_rate > 10)){
    warning("development rate data might contain a typo error. Please check it.")}
  if(any(temp < -10) | any(temp > 56)){
    warning("experienced temperatures by active organisms (i.e. not in diapause) are usually between 0 and 50ºC")}

  if (model_name[1] == "all") { # it will be probably the most commonly used option for user's experience
    model_names <- dev_model_table$model_name
  } else {model_names <- model_name}

  list_param <- dplyr::tibble(param_name = NULL,
                               start_vals = NULL,
                               param_est = NULL,
                               param_se = NULL,
                               model_name = NULL,
                               model_AIC = NULL)
  list_fit_models <- vector("list", length = length(dev_model_table$model_name))
  for (i in model_names) {
    model_i <- dev_model_table |>
      dplyr::filter(model_name == i)

    message(paste0("fitting model ", i)) # to let people know that the function is working and R is not crashing

    if (model_i$package == "devRate") {
      start_vals <- start_vals_devRate(model_name = i,
                                       temperature = temp,
                                       dev_rate = dev_rate)
    }

    if (model_i$package == "rTPC") {
      # start_vals <- start_vals_rtpc(model_name = i,
      #                               temperature = temp,
      #                               dev_rate = dev_rate)
      start_vals <- rTPC::get_start_vals(x = temp,
                                         y = dev_rate,
                                         model_name = model_name_translate(i))
    }

    devdata <- dplyr::tibble(temp = temp,
                              dev_rate = dev_rate)

    ## then fit model with nlme::gnls function
    fit_gnls <- suppressWarnings(nlme::gnls(
        model = reformulate(response = "dev_rate",
                            termlabels = unique(model_i$formula)),
        data = devdata,
        start = tidyr::replace_na(start_vals, 0), #to avoid error if start values compute a NA, probably not converging
        na.action = na.exclude, #to avoid problems in the model
        weights = nlme::varPower(form = ~temp), #usually development at higher temperatures has higher variability due to higher mortality
        control = nlme::gnlsControl(maxIter = 100,
                                    nlsTol = 1e-07,
                                    returnObject = TRUE)))

    if (is.null(fit_gnls)){
       list_fit_models[[which(dev_model_table$model_name == i)]] <- NA
       list_param_tbl <- dplyr::tibble(param_name = names(start_vals),
                                        start_vals = tidyr::replace_na(start_vals, 0),
                                        param_est = NA,
                                        param_se = NA,
                                        model_name = i,
                                        model_AIC = NA,
                                        model_fit = list(fit_gnls))

        list_param <- list_param |>
          dplyr::bind_rows(list_param_tbl)
        message(paste0("ERROR: model ", i, " did not converge. Please try other models listed in `available_models`"))
    }

    else {
      list_fit_models[[which(dev_model_table$model_name == i)]] <- fit_gnls
        sum_fit_gnls <- summary(fit_gnls)
      list_param_tbl <- dplyr::tibble(param_name = names(coef(fit_gnls)),
                                      start_vals = tidyr::replace_na(start_vals, 0),
                                      param_est = fit_gnls$coefficients,
                                      param_se = sum_fit_gnls$tTable[1:length(fit_gnls$coefficients), 2],
                                      model_name = i,
                                      model_AIC = sum_fit_gnls$AIC,
                                      model_fit = list(fit_gnls)
      )

      list_param <- list_param |>
        bind_rows(list_param_tbl)
    }
  } # <- loop ends

  if (!is.null(fit_gnls) && nrow(list_param) == 0) {
    stop(paste("Model(s)", model_name, "did not converge. Please try other models listed in `available_models`"))
    ## Maybe we can keep running all the other models rather than terminating?
  } else { list_param <- list_param |>
    tidyr::drop_na() |>
    mutate(false_convergence = purrr::map2_dbl(.x = start_vals,
                                               .y = param_est,
                                               .f = ~if_else(.x == .y,
                                                             NA_real_,
                                                             1))) |>
    tidyr::drop_na() |>
    select(-false_convergence) |>
    mutate(fit = purrr::map2_chr(.x = param_est,
                                         .y = param_se,
                                         .f = ~ifelse(.y > .x,
                                                      "bad",
                                                      "okay")))
  }
  if(any(list_param$fit == "bad")) {
  message("
  ---------------------------------------------------------------------------------------------------
  CAUTION: where `fit = bad` in your output fitted parameters table, parameter uncertainty is very high;
           we DO NOT recommend to select them for predictions solely based on their AIC.
  ---------------------------------------------------------------------------------------------------" )
    return(list_param)}
}

data(p.xylostella_liu2002)
data(available_models)

cabbage_moth_fitted <- fit_devmodels(temp = p.xylostella_liu2002$temperature,
                                     dev_rate = p.xylostella_liu2002$rate_development,
                                     model_name = c("all")) #might be a bit slow

print(cabbage_moth_fitted)
