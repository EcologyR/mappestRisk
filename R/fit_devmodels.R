#' Fits nonlinear regression models to data representing how development rate changes
#' with temperature (known as Thermal Performance Curves).
#'
#' @description
#' Fits nonlinear regression models (or Thermal Performance Curves) based on
#' [nls.multstart::nls_multstart()] approach to development rate data across temperatures.
#' The fitting procedure is built upon previous packages for starting values estimation,
#' namely `rTPC` and `devRate`.
#'
#' @param temp a vector of temperatures used in the experiment.
#' It should have at least four different temperatures and must contain only numbers
#' without any missing values.
#'
#' @param dev_rate a vector of estimated development rates corresponding to each temperature.
#' These rates are calculated as the inverse of the number of days to complete the transition
#' from the beginning of a certain life stage to the beginning of the following at each temperature.
#' It must be numeric and of the same length as `temp`.
#'
#' @param model_name a string or a vector that specifies the model(s) to use for
#' fitting the Thermal Performance Curves. Options include "all" or specific
#' models listed in [available_models]. These models typically exhibit a common unimodal, left-skewed shape.
#'
#' @returns returns a table in `tibble` format with estimates and
#' standard errors for each parameter of the models specified by the user that have adequately
#' converged. Models are sorted based on their Akaike Information Criterion (AIC) values,
#' with the best fitting models shown first. Fitted models are also provided in list format and
#' can be accessed using `your_parameters_tbl$fit[[x]]` with `x` refers to the desired row in the table.
#' It's important to consider ecological criteria alongside statistical information. For additional help in model selection,
#' we recommend using [plot_devmodels()] and consulting relevant literature.
#'
#' @source
#' The dataset used in the example was originally published in Satar & Yokomi (2022) under the CC-BY-NC license
#'
#' @seealso
#'  [nls.multstart::nls_multstart()] for structure of model fitting approach
#'
#'  [devRate::devRateEqList()] for information on several equations
#'
#'  `browseVignettes("rTPC")` for model names, start values searching workflows and
#'  bootstrapping procedures using both `rTPC` and `nls.multstart` packages.
#'
#' @references
#'  Angilletta, M.J., (2006). Estimating and comparing thermal performance curves. <i>J. Therm. Biol.</i> 31: 541-545.
#'  (for reading on model selection in TPC framework)
#'
#'  Padfield, D., O'Sullivan, H. and Pawar, S. (2021). <i>rTPC</i> and <i>nls.multstart</i>: A new pipeline to fit thermal performance curves in `R`. <i>Methods Ecol Evol</i>. 00: 1-6
#'
#'  Rebaudo, F., Struelens, Q. and Dangles, O. (2018). Modelling temperature-dependent development rate and phenology in arthropods: The `devRate` package for `R`. <i>Methods Ecol Evol</i>. 9: 1144-1150.
#'
#'  Satar, S. and Yokomi, R. (2002). Effect of temperature and host on development of <i>Brachycaudus schwartzi</i> (Homoptera: Aphididae). <i>Ann. Entomol. Soc. Am.</i> 95: 597-602.
#'
#' @export
#'
#'
#' @examples
#' data("aphid")
#'
#' fitted_tpcs_aphid <- fit_devmodels(temp = aphid$temperature,
#'                                         dev_rate = aphid$rate_value,
#'                                         model_name = "all")
#' print(fitted_tpcs_aphid)

fit_devmodels <- function(temp = NULL,
                          dev_rate = NULL,
                          model_name = NULL){

  check_data(temp, dev_rate)

  if (!is.character(model_name)){
    stop("model_name must be a string in ?available_models")}

  if (!all(model_name %in% c("all", available_models$model_name))) {
    stop("model not available. For available model names, see `available_models`")
  }

  if (model_name == "all") {
    models_2fit <- available_models |>
      dplyr::filter(n_params <= dplyr::n_distinct(temp)) |>
      dplyr::pull(model_name)
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
    model_i <- available_models |>
      dplyr::filter(model_name == i)

    if (available_models$package[available_models$model_name == i] == "devRate") {
      start_vals <- start_vals_devRate(model_name_2fit = model_i,
                                       temperature = temp,
                                       dev_rate = dev_rate)

      possible_error <- tryCatch(expr = {
        devdata <- dplyr::tibble(temp, dev_rate)
        start_upper_vals <- purrr::map(.x = start_vals,
                                       .f = ~.x + abs(.x/2))
        start_lower_vals <- purrr::map(.x = start_vals,
                                       .f = ~.x - abs(.x/2))
        fit_nls <- nls.multstart::nls_multstart(formula = stats::reformulate(response = "dev_rate",
                                                      termlabels = unique(model_i$formula)),
                                 data = devdata,
                                 iter = 500,
                                 start_lower = start_lower_vals,
                                 start_upper = start_upper_vals,
                                 supp_errors = "Y")
        list_fit_models[[which(available_models$model_name == i)]] <- fit_nls
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
      if (inherits(possible_error, "error")) {
        fit_nls <- NULL
      }
      if (is.null(fit_nls)) {
        list_param <- list_param
      } else {
        list_param <- list_param |>
        dplyr::bind_rows(list_param_tbl)
        }
    }
    ## end of devRate

    if (available_models$package[available_models$model_name == i] == "rTPC") {
      possible_error <- tryCatch(expr = {start_vals <- rTPC::get_start_vals(x = temp,
                                           y = dev_rate,
                                           model_name = model_name_translate(i))
        devdata <- dplyr::tibble(temp, dev_rate)
        start_upper_vals <- purrr::map(.x = start_vals,
                                       .f = ~.x + abs(.x/2))
        start_lower_vals <- purrr::map(.x = start_vals,
                                       .f = ~.x - abs(.x/2))
        fit_nls <- nls.multstart::nls_multstart(formula = stats::reformulate(response = "dev_rate",
                                                      termlabels = unique(model_i$formula)),
                                 data = devdata,
                                 iter = 500,
                                 start_lower = start_lower_vals,
                                 start_upper = start_upper_vals,
                                 lower = rTPC::get_lower_lims(devdata$temp,
                                                              devdata$dev_rate,
                                                              model_name = model_name_translate(i)),
                                 upper = rTPC::get_upper_lims(devdata$temp,
                                                              devdata$dev_rate,
                                                              model_name = model_name_translate(i)),
                                 supp_errors = "Y")
        list_fit_models[[which(available_models$model_name == i)]] <- fit_nls
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
      if (inherits(possible_error, "error")) {
        fit_nls <- NULL
      }
      if (is.null(fit_nls)) {
        list_param <- list_param
      } else {list_param <- list_param |>
        dplyr::bind_rows(list_param_tbl)}
    }
    # end of rTPC processing

    } # <- loop ends
    return(list_param)
  }

