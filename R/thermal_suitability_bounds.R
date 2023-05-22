#' Obtain thermal traits representing boundaries of optimal thermal suitability for pest risk occurrence assessment
#'
#'
#'
#' @param fitted_parameters a tibble/data.frame obtained with fit_models() function
#' @param model_name one or several of the models in fitted_parameters.
#' Must belong to available models in list_available_models
#' @param suitability_threshold Quantile (%) of the curve; e.g. 75 would give
#'  the temperatures within the values at which development rate is in the
#'  top 25% of its possible values
#'
#' @return a tibble/data.frame with name of the model and two thermal traits representing the above-mentioned suitability boundaries
#' @export
#'
#'
#'
#' @examples  a.citricidus_tsai1999 <- readRDS("data/a.citricidus_tsai1999.rds")
#' source("R/dario_model_fitting.R")
#' aphis_citricida_fitted <- fit_devmodels(temp = a.citricidus_tsai1999$temperature,
#'                                         dev_rate = a.citricidus_tsai1999$rate_development,
#'                                         model_name = "all") #might be a bit slow
#' ## examine them visually to better choose on ecological criteria and not only on statistical fitting
#' plot_devmodels(temp = a.citricidus_tsai1999$temperature,
#'                dev_rate = a.citricidus_tsai1999$rate_development,
#'                fitted_parameters = aphis_citricida_fitted)
#'
#' therm_bounds_moth <- thermal_suitability_bounds(fitted_parameters = cabbage_moth_fitted,
#'                                                 model_name = "lactin2",
#'                                                 suitability_threshold = 75)

thermal_suitability_bounds <- function(fitted_parameters,
                                       model_name,
                                       suitability_threshold = NULL) {
  `%!in%` <- Negate(`%in%`)
  data("available_models_table")
  fitted_parameters_nonas <- fitted_parameters |>
    tidyr::drop_na()
  fitted_param_names <- fitted_parameters_nonas |>
    distinct(model_name) |>
    pull(model_name)
  if(is.null(suitability_threshold)){
    suitability_threshold <- 50
    message("No suitability_threshold value input. Using by default suitability_threshold = 50%")
  }
  if(suitability_threshold < 50) {
    stop("suitability must be higher than 50% in order to have applied sense. Default 50%")
  } else if (!is.null(model_name) && any(model_name %!in% available_models)) {
      stop("model name not available. For available model names, run 'data(list_available_models); list_available_models'.")
  } else if(!is.null(model_name) && any(model_name %!in% fitted_param_names)) {
      stop(paste("Model", model_name[which(model_name %!in% fitted_param_names)], "did not fitted well to your data.
    Try using another fitted model in your table instead or remove it from `model_name` argument"))
  } else if(!is.null(model_name) && any(model_name == "linear_campbell")) {
      stop("Thermal Suitability predictions require nonlinear models.
   Try another fitted model in your table instead")
  } else {
    fitted_parameters_nonas <- fitted_parameters
    model2fit <- model_name
    tvals <- tibble::tibble(model_name = NULL,
                            tval_left = NULL,
                            tval_right = NULL)
    for(i in model2fit){
      model_i <- dev_model_table |>
        dplyr::filter(model_name == i)
      params_i <- fitted_parameters_nonas |>
        dplyr::filter(model_name == i)  |>
        dplyr::pull(param_est)

      ##predict based on parameters
      explore_preds <- dplyr::tibble(temp = seq(0,50, 0.001),
                                     model_name = i,
                                     preds = NULL,
      )
      fit_vals_tbl <- explore_preds |>
        dplyr::mutate(formula = model_i$working_formula) |>
        dplyr::mutate(preds = purrr::map_dbl(.x = temp,
                                             .f = reformulate(unique(formula)))) |>
        dplyr::filter(preds >= 0) |>
        dplyr::select(-formula) |>
        dplyr::mutate(preds = dplyr::case_when(model_name == "ratkowsky" & temp > params_i[2] ~ NA_real_,
                                               model_name == "ratkowsky" & temp < params_i[1] ~ NA_real_,
                                               model_name == "briere1" & temp < params_i[1] ~ NA_real_,
                                               model_name == "briere2" & temp < params_i[1] ~ NA_real_,
                                               TRUE ~ preds)
        ) # to exclude biological non-sense predictions due to model mathematical properties
      tryCatch(expr =
                 {topt_pred <- fit_vals_tbl |>
                   slice_max(preds) |>
                   pull(temp)
                  devrate_max <- fit_vals_tbl |>
                    dplyr::slice_max(preds) |>
                    dplyr::pull(preds)
                  half_left <- fit_vals_tbl |>
                    dplyr::filter(temp < topt_pred)
                  half_right <- fit_vals_tbl |>
                   dplyr::filter(temp >= topt_pred)
                  therm_suit_left <- half_left |>
                    dplyr::slice(max(which(half_left$preds <= devrate_max*0.01*suitability_threshold), na.rm = TRUE)) |>
                    dplyr::pull(temp)
                  therm_suit_right <- half_right |>
                    dplyr::slice(min(which(half_right$preds <= devrate_max*0.01*suitability_threshold), na.rm = TRUE)) |>
                    dplyr::pull(temp)
                 },
               error = function(error_message) {
                 message(paste("Model", model_name, "is not appropriate to model thermal suitability.
Try another instead (use `plot_devmodel()` to see curve shapes).

(ignore errors and warning below this line)
--------------------------------------"))
                 }
               )
      tvals_i <- dplyr::tibble(model_name = i,
                               tval_left = therm_suit_left,
                               tval_right = therm_suit_right)
      tvals <- dplyr::bind_rows(tvals, tvals_i)
    } # <- loop ends
  }
    if(any(tvals$tval_right >= 50))
    { warning("upper value of thermal suitability  might be non-realistic")
      }
    return(tvals)
}

