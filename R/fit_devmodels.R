#' Fit nonlinear regression models to development rate data across temperatures (i.e. Thermal Performance Curves)
#'
#' @param temp a vector containing temperature treatments (predictor variable),
#' must have at least three different temperature treatments. The function works for both
#' aggregated data (i.e. one development rate value for each temperature treatment, which is representative of the cohort average development
#' rate) or individual data (i.e. one observation of development rate for each individual in the experiment at each temperature)
#'
#' @param dev_rate a vector containing development rate estimates (1/days of development); must be of same length than temp.
#' The function works for both aggregated data (i.e. one development rate value for each temperature treatment, which is representive of the cohort average development
#' rate) or individual data (i.e. one observation of development rate for each individual in the experiment at each temperature)
#'
#' @param model_name "all" or alternatively one or several of the models listed in `?available_models`
#' to parameterise thermal performance curves.
#'
#' @param variance_model argument to pass to [nlme::gnls()] in `weights` argument
#' to take into account heterogeneity of variance. Due to higher mortalities and variability at
#' high temperatures, variance usually increases along temperature, so an exponential or power function
#' with `exp` (default) or `power` are recommended. Alternatively, a constant function
#' may be used if the user defines this argument as `constant`.

#'
#' @return this function returns a tibble with estimate and standard error for each parameter of the models from the user call
#' that have adequately converged to the data. It also shows an AIC value and a comment on those models whose parameter uncertainty
#' is high (`fit = "bad"` in the tibble). Fitted models are included in list format, and can be accessed
#' via `your_parameters_tbl$fit[[x]]` with `x` being the desired row in the table.
#' For model selection, also ecological criteria should be followed by the user. To help that purpose,
#' we recommend using [mappestRisk::plot_devmodels()] and look into the literature rather than focusing only on statistical information.
#'
#' #' @export
#'
#' @examples
#' data("h.vitripennis_pilkington2014")
#'
#' homalodisca_fitted <- fit_devmodels(temp = h.vitripennis_pilkington2014$temperature,
#'                                     dev_rate = h.vitripennis_pilkington2014$rate_development,
#'                                     model_name = c("all"),
#'                                     variance_model = "exp") #might be a bit slow
#'


fit_devmodels <- function(temp = NULL,
                          dev_rate = NULL,
                          model_name = NULL,
                          variance_model = NULL){
varfun <- variance_model
if(any(is.na(dev_rate))) {
  stop("development rate data have NAs; please consider removing them or fixing them")
}
if(any(is.na(temp))) {
  stop("temperature data have NAs; please consider removing them or fixing them")
}
if(is.null(variance_model)){
  varfun <- "exp"
  warning("no variance_model input has been provided by the user. Using exp by default")

} else if(!varfun %in% c("exp", "power", "constant")) {
  stop("variance_model input not available. Use `exp`, `power` or `constant` - see ?fit_devmodels() for more information")
}
  if(!is.numeric(temp)) {
    stop("temperature data is not numeric. Please check it.")
  }
  if(length(unique(temp)) < 4) {
    stop("At least four different temperature treatments in the data are required.")
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
    stop("model not available. For available model names, see `dev_model_table`")
  }
  if (any(dev_rate < 0) | any(dev_rate > 10)){
    warning("negative or extremely high values of dev_rate development rate data might contain a typo error. Please check it.")}
  if(any(temp < -10) | any(temp > 56)){
    warning("experienced temperatures by active organisms (i.e. not in diapause) are usually between 0 and 50ºC")}

  if (model_name[1] == "all") { # it will be probably the most commonly used option for user's experience
    model_names <- dev_model_table$model_name
  } else {model_names <- model_name}

  if(any(model_names == "regniere") && length(temp) < 6) {
  model_names <- model_names[model_names != "regniere"]
  warning("regniere model (6 parameters) needs larger data sets to converge. Model.")
  }
  if(any(model_names == "wang") && length(temp) < 6) {
  model_names <- model_names[model_names != "wang"]
  warning("wang model (6 parameters) needs larger data sets to converge. Model discarded.")
  }
  if(any(model_names == "ssi") && length(temp) < 6) {
  model_names <- model_names[model_names != "ssi"]
  warning("ssi model (6 parameters) needs larger data sets to converge. Model discarded.")
  }
  if(any(model_names == "mod_polynomial") && length(temp) < 5) {
  model_names <- model_names[model_names != "mod_polynomial"]
  warning("mod_polynomial model (5 parameters) needs larger data sets to converge. Model discarded.")
  }
  if(length(temp) == 3){
    model_names <- c("briere1", "mod_gaussian", "linear_campbell", "lactin1")
    warning("with three temperature treatments, only models with 3 parameters or less are fitted.
  Fitting briere1, mod_gaussian, linear_campbell and lactin1")
  }
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
    if(varfun == "exp") {
   possible_error <- tryCatch(expr =
      fit_gnls <- suppressWarnings(nlme::gnls(
        model = reformulate(response = "dev_rate",
                            termlabels = unique(model_i$formula)),
        data = devdata,
        start = tidyr::replace_na(start_vals, 0), #to avoid error if start values compute a NA, probably not converging
        na.action = na.exclude, #to avoid problems in the model
        weights = nlme::varExp(form = ~temp),
        control = nlme::gnlsControl(maxIter = 50,
                                    nlsTol = 1e-09,
                                    minScale = 1e-01,
                                    returnObject = TRUE))),
      error = function(e) e)
    if(inherits(possible_error, "error")) {
      fit_gnls <- NULL
    }
   }
    else if(varfun == "power") {
      possible_error <- tryCatch(expr =
                                   fit_gnls <- suppressWarnings(nlme::gnls(
        model = reformulate(response = "dev_rate",
                            termlabels = unique(model_i$formula)),
        data = devdata,
        start = tidyr::replace_na(start_vals, 0), #to avoid error if start values compute a NA, probably not converging
        na.action = na.exclude, #to avoid problems in the model
        weights = nlme::varPower(form = ~temp),
        control = nlme::gnlsControl(maxIter = 50,
                                    nlsTol = 1e-09,
                                    minScale = 1e-01,

                                    returnObject = TRUE))),
      error = function(e) e)
    if(inherits(possible_error, "error")) {
      fit_gnls <- NULL
      }
  } else if (varfun == "constant") {
    possible_error <- tryCatch(expr =
                                 fit_gnls <- suppressWarnings(nlme::gnls(
        model = reformulate(response = "dev_rate",
                            termlabels = unique(model_i$formula)),
        data = devdata,
        start = tidyr::replace_na(start_vals, 0), #to avoid error if start values compute a NA, probably not converging
        na.action = na.exclude, #to avoid problems in the model
        weights = nlme::varIdent(),
        control = nlme::gnlsControl(maxIter = 50,
                                    nlsTol = 1e-09,
                                    minScale = 1e-01,
                                    returnObject = TRUE))),
      error = function(e) e)
  if(inherits(possible_error, "error")) {fit_gnls <- NULL}
    }

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
        message(paste0("ERROR: model ", i, " did not converge. Please try other models listed in `dev_model_table`"))
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
        dplyr::bind_rows(list_param_tbl)
    }
  } # <- loop ends

  if (!is.null(fit_gnls) && nrow(list_param) == 0) {
    stop(paste("Model(s)", model_name, "did not converge. Please try other models listed in `available_models`"))
    ## Maybe we can keep running all the other models rather than terminating?
  } else { list_param <- list_param |>
    tidyr::drop_na() |>
    dplyr::mutate(false_convergence = purrr::map2_dbl(.x = start_vals,
                                               .y = param_est,
                                               .f = ~dplyr::if_else(.x == .y,
                                                             NA_real_,
                                                             1))) |>
    tidyr::drop_na() |>
    dplyr::select(-false_convergence) |>
    dplyr::mutate(fit = purrr::map2_chr(.x = param_est,
                                         .y = param_se,
                                         .f = ~ifelse(abs(.y) > abs(.x),
                                                      "bad",
                                                      "okay")))
  }
  if(any(list_param$fit == "bad")) {
  message("
  ---------------------------------------------------------------------------------------------------
  CAUTION: where `fit = bad` in your output fitted parameters table, parameter uncertainty is very high;
  we DO NOT recommend to select them for predictions solely based on their AIC. We strongly
  recommend to take a look at predictions using `plot_devmodels()` function for your data.
  ---------------------------------------------------------------------------------------------------" )
    }
  if(nrow(list_param) == 0 |
     all(list_param |>
         dplyr::group_by(model_name) |>
         dplyr::summarise(length(unique(fit))) == 1) &&
                            !all(list_param$fit == "okay")) {
      stop("no model converged adequately for fitting your data")
    } else {
      return(list_param)
      }
}


