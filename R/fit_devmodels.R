#' Fit nonlinear regression models to development rate data
#'
#' @param temp a vector containing temperature treatments (predictor variable), must have at least three different temperature treatments
#' @param dev_rate a vector containing development rate estimates (1/days of development); must be of same length than temp
#' @param model_name "all" or alternatively one or several of the models listed in available_models
#'
#' @return this function returns a tibble with estimate and standard error for each parameter of the selected models and the AIC
#' @export
#'
#' @examples
#' data(a.citricidus_tsai1999)
#'
#' aphis_citricida_fitted <- fit_devmodels(temp = a.citricidus_tsai1999$temperature,
#'                                         dev_rate = a.citricidus_tsai1999$rate_development,
#'                                         model_name = c("all")) #might be a bit slow
#'

fit_devmodels <- function(temp = NULL, dev_rate = NULL, model_name = NULL){

  # source("data-raw/auxiliary_functions_modelfitting.R")
  # data("list_available_models")
  # `%!in%` <- Negate(`%in%`)

  stopifnot(is.numeric(temp) & length(temp) >= 3)
  stopifnot(is.numeric(dev_rate))
  stopifnot(length(dev_rate) == length(temp))
  stopifnot(is.character(model_name))

  if (model_name != "all" && any(!model_name %in% available_models)) {
    stop("model not available. For available model names, see ?available_models")
  }

  if (any(model_name == "all")) { # it will be probably the most commonly used option for user's experience
    model_names <- available_models
  } else {
    model_names <- model_name
  }

  list_param <- tibble::tibble(param_name = NULL,
                               start_vals = NULL,
                               param_est = NULL,
                               param_se = NULL,
                               model_name = NULL,
                               model_AIC = NULL)

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
      start_vals <- start_vals_rtpc(model_name = i,
                                    temperature = temp,
                                    dev_rate = dev_rate)
    }
    devdata <- tibble::tibble(temp = temp,
                              dev_rate = dev_rate)

    ## then fit model with nlme::gnls function
    fit_gnls <- nlme::gnls(
        model = reformulate(response = "dev_rate",
                            termlabels = unique(model_i$formula)),
        data = devdata,
        start = replace_na(start_vals, 0), #to avoid error if start values compute a NA, probably not converging
        na.action = na.exclude, #to avoid problems in the model
        weights = varExp(form = ~temp), #usually development at higher temperatures has higher variability due to higher mortality
        control = gnlsControl(maxIter = 100,
                              nlsTol = 1e-07,
                              returnObject = TRUE))
    if (is.null(fit_gnls)){ #means that it has not converged, we'll return a NA
      list_param_tbl <- tibble:: tibble(param_name = names(start_vals),
                                        start_vals = replace_na(start_vals, 0),
                                        param_est = NA,
                                        param_se = NA,
                                        model_name = i,
                                        model_AIC = NA)

      list_param <- list_param |>
        dplyr::bind_rows(list_param_tbl)
      message(paste0("ERROR: model ", i, " did not converge. Please try other models listed in `available_models`"))
    }

    else {
      sum_fit_gnls <- summary(fit_gnls)
      list_param_tbl <- tibble(param_name = names(coef(fit_gnls)),
                               start_vals = replace_na(start_vals, 0),
                               param_est = fit_gnls$coefficients,
                               param_se = sum_fit_gnls$tTable[1:length(fit_gnls$coefficients), 2],
                               model_name = i,
                               model_AIC = sum_fit_gnls$AIC
      )

      list_param <- list_param |>
        dplyr::bind_rows(list_param_tbl)
      list_param <- list_param |>
        mutate(false_convergence_detect = purrr::map2_dbl(.x = start_vals,
                                                          .y = param_est,
                                                          .f = ~if_else(.x == .y,
                                                                        NA_real_,
                                                                        .y)))
    }
  }

  list_param <- list_param |>
    tidyr::drop_na() |>  # exclude false convergence
    select(-false_convergence_detect)

  if (!is.null(fit_gnls) && nrow(list_param) == 0) {
    stop(paste("Model(s)", model_name, "did not converge. Please try other models listed in `available_models`"))
    ## Maybe we can keep running all the other models rather than terminating?
  } else {
    return(list_param)
  }
}

