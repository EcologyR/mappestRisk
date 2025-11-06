#' Plot fitted thermal performance curves
#'
#' Plot the predicted development rates across temperatures
#' based on fitted Thermal Performance Curves (TPCs)
#' for one or several models displayed in facets.
#'
#' @inheritParams fit_devmodels
#'
#' @param fitted_parameters a `tibble` obtained with [fit_devmodels()],
#' including parameter names, estimates, standard errors, AICs, and
#' nls objects (fitted_models) using the [nls.multstart::nls_multstart()] approach.
#'
#' @param species optional a string of the target species that
#' will constitute the plot title. Must be of type "character".
#'
#' @param life_stage optional a string of the target life stage that
#' will constitute the plot subtitle. Must be of type "character".
#'
#' @returns A plot with predicted values (development rate) across temperatures
#' for models that have adequately converged using [fit_devmodels()] function.
#' It's a ggplot object, which can be assigned to a user-defined object.
#'
#' @seealso [fit_devmodels()] for fitting Thermal Performance Curves to
#' development rate data, which is in turn based on [nls.multstart::nls_multstart()].
#'
#'
#' @inherit fit_devmodels references
#'
#' @export
#'
#' @examples
#' data("aphid")
#'
#' fitted_tpcs <- fit_devmodels(temp = aphid$temperature,
#'                              dev_rate = aphid$rate_value,
#'                              model_name = c("lactin2", "briere2", "mod_weibull"))
#'
#' plot_devmodels(temp = aphid$temperature,
#'                dev_rate = aphid$rate_value,
#'                fitted_parameters = fitted_tpcs,
#'                species = "Brachycaudus schwartzi",
#'                life_stage = "Nymphs")


plot_devmodels <- function(temp = NULL,
                           dev_rate = NULL,
                           fitted_parameters = NULL,
                           species = NULL,
                           life_stage = NULL) {

  check_data(temp, dev_rate)

  if (is.null(fitted_parameters)) {
    stop("`fitted_parameters` is NULL; use `mappestRisk::fit_devmodels()` to check that at least one model converged")
  }

  if (!inherits(fitted_parameters, "data.frame")) {
    stop("The argument `fitted_parameters` must be a tibble or data.frame as produced by `mappestRisk::fit_devmodels()` function. No modifications of columns of the fitted_parameters are allowed, but you can subset observations by filtering or subsetting by rows if desired.")
  }

  if (suppressWarnings(any(!c("param_name", "start_vals", "param_est",
                              "param_se", "model_name", "model_AIC",
                              "model_BIC") %in% names(fitted_parameters)))) {
    stop("The argument `fitted_parameters` must be a tibble or data.frame as produced by `mappestRisk::fit_devmodels()` function. No modifications of columns of the fitted_parameters are allowed, but you can subset observations by filtering or subsetting by rows if desired.")
  }

  if (nrow(fitted_parameters) == 0) {
    stop("no model has converged in your `fitted_parameters` data.frame. Is it the appropriate object coming from converged `fit_devmodels()`?")
  }

  if (typeof(species) != "character" && !is.null(species)) {
    stop("`species` must be a character or NULL")
  }

  if (typeof(life_stage) != "character" && !is.null(life_stage)) {
    stop("`life_stage` must be a character or NULL")
  }

  devdata <- dplyr::tibble(temperature = temp,
                           development_rate = dev_rate)

  predict2fill <- dplyr::tibble(temp = NULL,
                                dev_rate = NULL,
                                model_name = NULL,
                                model_AIC = NULL)

  model_names2plot <- fitted_parameters |>
    dplyr::distinct(model_name) |>
    dplyr::pull(model_name)

  for (i in model_names2plot) {
    fitted_parameters_i <- fitted_parameters |>
      dplyr::filter(model_name == i)
    model_AIC_i <- fitted_parameters_i |>
      dplyr::pull(model_AIC)
    params_i <- fitted_parameters_i |>
      dplyr::pull(param_est)
    formula_i <- available_models |>
      dplyr::filter(model_name == i) |>
      dplyr::pull(working_formula)
    ##predict based on parameters
    explore_preds <- dplyr::tibble(temp = seq(min(devdata$temperature) - 15,
                                              max(devdata$temperature) + 15,
                                              .01),
                                   model_name = i,
                                   model_AIC = model_AIC_i[1],
                                   preds = NULL,
                                   n_params = length(params_i))
    fit_vals_tbl <- explore_preds |>
      dplyr::select(temp, model_name, model_AIC, n_params) |>
      dplyr::mutate(formula = formula_i) |>
      dplyr::mutate(
        preds = purrr::map_dbl(.x = temp,
                               .f = stats::reformulate(unique(formula_i)))) |>
      dplyr::filter(preds >= 0) |>
      dplyr::select(-formula) |>
      dplyr::mutate(preds = dplyr::case_when(
        model_name == "ratkowsky" & temp > params_i[2] ~ NA_real_,
        model_name == "ratkowsky" & temp < params_i[1] ~ NA_real_,
        model_name == "briere1" & temp < params_i[1] ~ NA_real_,
        model_name == "briere2" & temp < params_i[1] ~ NA_real_,
        TRUE ~ preds)
      ) # to exclude biological non-sense predictions due to model
    #     mathematical properties
    predict2fill <- predict2fill |>
      dplyr::bind_rows(fit_vals_tbl)
  }

  aic_text <-  predict2fill  |>
    dplyr::group_by(model_name)  |>
    dplyr::summarise(aic = mean(model_AIC),
                     n_params = paste(mean(n_params), "parameters"))  |>
    dplyr::arrange(aic)
  aic_order <- aic_text  |>
    dplyr::pull(model_name)
  aic_values <- aic_text |>
    dplyr::mutate(aic =   paste("AIC =", round(aic, 2)),
                  temp = min(devdata$temperature),
                  preds = 1.5*max(devdata$development_rate))
  my_title <- substitute(italic(paste(especie)), list(especie = species))
  ggplot_models <- ggplot2::ggplot() +
    ggplot2::geom_line(data = predict2fill |>
                         dplyr::filter(
                           preds < (1.5*max(devdata$development_rate)
                                    )),
                       ggplot2::aes(x = temp, y = preds, color = model_name),
                       linewidth = 1.3) +
    ggplot2::geom_point(data = devdata, ggplot2::aes(x = temperature,
                                                     y = development_rate),
                        color = "darkslategray",
                        alpha = .8,
                        size = 1.5) +
    ggplot2::facet_wrap(~factor(model_name)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Temperature",
                  y = expression(italic(R(T))~(d^-1)),
                  title = my_title,
                  subtitle = life_stage) +
    ggplot2::geom_label(data = aic_values,
                        ggplot2::aes(label = aic,
                                     x = temp,
                                     y = preds,
                                     fill = model_name),
                        color = "white",
                        size = 3) +
    ggplot2::geom_label(data = aic_values,
                        ggplot2::aes(x = temp,
                                     y = preds-preds/8,
                                     label = n_params,
                                     fill = model_name),
                        color = "white",
                        size = 3)

  return(ggplot_models)
}

