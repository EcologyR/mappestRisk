#' Fit nonlinear regression models to development rate data across temperatures (i.e. Thermal Performance Curves)
#'
#' @param temp a vector containing temperature treatments (predictor variable). It must have at least four different temperature treatments. The function works for both
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
#' @returns `fit_devmodels()` returns a [tibble()] with estimate and standard error for each parameter of the models from the user call
#' that have adequately converged to the data, sorting from lowest to highest AIC values, which are also shown. A comment on those models whose parameter uncertainty
#' is high (`fit = "bad"` in the tibble) is advised. Fitted models are included in list format, and can be accessed
#' via `your_parameters_tbl$fit[[x]]` with `x` being the desired row in the table.
#' For model selection, also ecological criteria should be followed by the user. To help that purpose,
#' we recommend using `plot_devmodels()` and look into the literature rather than focusing only on statistical information.
#'
#' @seealso
#'  [nlme::varClasses()] for more information on variance structure modelling under `gnls` approaches.

#'  [devRate::devRateEqList()] for information on several equations

#'  `browseVignettes("rTPC")` for model names and functioning of start values searching workflows using [nls.multstart::nls_multstart()]
#'
#' @export
#'
#' @examples
#' data("h.vitripennis_pilkington2014")
#'
#' fit_devmodels(temp = h.vitripennis_pilkington2014$temperature,
#'               dev_rate = h.vitripennis_pilkington2014$rate_development,
#'               model_name = c("all"),
#'               variance_model = "exp") #might be a bit slow
#'


fit_devmodels <- function(temp = NULL,
                          dev_rate = NULL,
                          model_name = NULL){
  if(any(is.na(dev_rate))) {
  stop("development rate data have NAs; please consider removing them or fixing them")
  }
  if(any(is.na(temp))) {
  stop("temperature data have NAs; please consider removing them or fixing them")
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
    warning("experienced temperatures by active organisms are usually between 0 and 50ºC")}

  if (model_name[1] == "all") { # it will be probably the most commonly used option for user's experience
    model_names <- dev_model_table$model_name
  } else {model_names <- model_name}
  if(model_name == "all") {
    models_2fit <- dev_model_table |>
      filter(n_params <= n_distinct(temp)) |>
      pull(model_name)
  } else {
    models_2fit <- model_name
  }

    list_fit_models <- vector("list", length = length(models_2fit))
  list_param <- dplyr::tibble(param_name = NULL,
                              start_vals = NULL,
                              param_est = NULL,
                              param_se = NULL,
                              model_name = NULL,
                              model_AIC = NULL,
                              model_BIC = NULL,
                              model_fit = NULL)

  for (i in models_2fit) {
    message(paste0("fitting model ", i)) # to let people know that the function is working and R is not crashing
    model_i <- dev_model_table |>
      filter(model_name == i)
    if (model_i$package == "devRate") {
      start_vals <- start_vals_devRate(model_name_2fit = model_i,
                                       temperature = temp,
                                       dev_rate = dev_rate)

      possible_error <- tryCatch(expr = {
        model_i <- dev_model_table |>
          filter(model_name == i)
        therm_perf_df <- tibble(temp, dev_rate)
        start_upper_vals <- purrr::map(.x = start_vals,
                                       .f = ~.x + abs(.x/2))
        start_lower_vals <- purrr::map(.x = start_vals,
                                       .f = ~.x - abs(.x/2))
        fit_nls <- nls_multstart(formula =reformulate(response = "dev_rate",
                                                      termlabels = unique(model_i$formula)),
                                 data = therm_perf_df,
                                 iter = 500,
                                 start_lower = start_lower_vals,
                                 start_upper = start_upper_vals,
                                 supp_errors = "Y")
        list_fit_models[[which(dev_model_table$model_name == i)]] <- fit_nls
        sum_fit_nls <- summary(fit_nls)
        list_param_tbl <- dplyr::tibble(param_name = extract_param_names(fit_nls),
                                        start_vals = tidyr::replace_na(start_vals, 0),
                                        param_est = sum_fit_nls$parameters[1:model_i$n_params, 1],
                                        param_se = sum_fit_nls$parameters[1:model_i$n_params, 2],
                                        model_name = i,
                                        model_AIC = AIC(fit_nls),
                                        model_BIC = BIC(fit_nls),
                                        model_fit = list(fit_nls))
      }, # <- inside tryCatch
      error = function(e) e)
      if(inherits(possible_error, "error")) {
        fit_nls <- NULL
      }
      if(is.null(fit_nls)) {
        list_param <- list_param
      } else {list_param <- list_param |>
        dplyr::bind_rows(list_param_tbl)}
    } else {
      possible_error <- tryCatch(expr = {start_vals <- rTPC::get_start_vals(x = temp,
                                           y = dev_rate,
                                           model_name = model_name_translate(i))
        model_i <- dev_model_table |>
          filter(model_name == i)
        therm_perf_df <- tibble(temp, dev_rate)
        start_upper_vals <- purrr::map(.x = start_vals,
                                       .f = ~.x + abs(.x/2))
        start_lower_vals <- purrr::map(.x = start_vals,
                                       .f = ~.x - abs(.x/2))
        fit_nls <- nls_multstart(formula =reformulate(response = "dev_rate",
                                                      termlabels = unique(model_i$formula)),
                                 data = therm_perf_df,
                                 iter = 500,
                                 start_lower = start_lower_vals,
                                 start_upper = start_upper_vals,
                                 lower = get_lower_lims(therm_perf_df$temp,
                                                        therm_perf_df$dev_rate,
                                                        model_name = model_name_translate(i)),
                                 upper = get_upper_lims(therm_perf_df$temp,
                                                        therm_perf_df$dev_rate,
                                                        model_name = model_name_translate(i)),
                                 supp_errors = "Y")
        list_fit_models[[which(dev_model_table$model_name == i)]] <- fit_nls
        sum_fit_nls <- summary(fit_nls)
        list_param_tbl <- dplyr::tibble(param_name = extract_param_names(fit_nls),
                                        start_vals = tidyr::replace_na(start_vals, 0),
                                        param_est = sum_fit_nls$parameters[1:model_i$n_params, 1],
                                        param_se = sum_fit_nls$parameters[1:model_i$n_params, 2],
                                        model_name = i,
                                        model_AIC = AIC(fit_nls),
                                        model_BIC = BIC(fit_nls),
                                        model_fit = list(fit_nls))
      }, # <- inside tryCatch
      error = function(e) e)
      if(inherits(possible_error, "error")) {
        fit_nls <- NULL
      }
      if(is.null(fit_nls)) {
        list_param <- list_param
      } else {list_param <- list_param |>
        dplyr::bind_rows(list_param_tbl)}
      } # <- if else ends

    } # <- loop ends
    return(list_param)
  }

