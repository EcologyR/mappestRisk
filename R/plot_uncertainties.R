#' Draw bootstrapped Thermal Performance Curves (TPCs) to visualize uncertainty
#' in parameter estimation of TPC fitting
#'
#' @param bootstrap_tpcs a `tibble` A tibble object as produced by
#' [predict_curves()], containing bootstrapped TPCs to propagate uncertainty.
#'
#' @param alpha a number between 0 and 1 to choose transparency of the bootstrapped
#' curves (0 = complete transparency, 1 = solid line).
#'
#' @inheritParams plot_devmodels
#'
#' @returns A ggplot object containing the visual representation of the estimate TPC and the bootstrapped uncertainty
#' curves as a ribbon. Each model is represented in a facet, and data points are also explicit.
#'
#' @seealso `browseVignettes("rTPC")` for model names, start values searching workflows, and
#'  bootstrapping procedures using both [rTPC::get_start_vals()] and [nls.multstart::nls_multstart()]
#'
#'  [fit_devmodels()] for fitting Thermal Performance Curves to development rate data,
#'  which is in turn based on [nls.multstart::nls_multstart()].
#'  [predict_curves()] for bootstrapping procedure based on the above-mentioned `rTPC` vignettes.
#'
#' @inherit fit_devmodels references source
#'
#'
#' @export
#'
#' @examplesIf interactive()
#' data("aphid")
#'
#' fitted_tpcs <- fit_devmodels(temp = aphid$temperature,
#'                              dev_rate = aphid$rate_value,
#'                              model_name = "all")
#'
#' plot_devmodels(temp = aphid$temperature,
#'                dev_rate = aphid$rate_value,
#'                fitted_parameters = fitted_tpcs,
#'                species = "Brachycaudus swartzi",
#'                life_stage = "Nymphs")
#'
#' boot_tpcs <- predict_curves(temp = aphid$temperature,
#'                             dev_rate = aphid$rate_value,
#'                             fitted_parameters = fitted_tpcs,
#'                             model_name_2boot = c("lactin2", "briere2", "beta"),
#'                             propagate_uncertainty = TRUE,
#'                             n_boots_samples = 10)
#'
#' print(boot_tpcs)
#'
#'
#' plot_uncertainties(temp = aphid$temperature,
#'                    dev_rate = aphid$rate_value,
#'                    bootstrap_tpcs = boot_tpcs,
#'                    species = "Brachycaudus schwartzi",
#'                    life_stage = "Nymphs")


plot_uncertainties <- function(temp = NULL,
                               dev_rate = NULL,
                               bootstrap_tpcs = NULL,
                               species = NULL,
                               life_stage = NULL,
                               alpha = 0.2) {

  ## Checks

  check_data(temp, dev_rate)

  if (!is.character(species) && !is.null(species)) {
    stop("`species` must be a character or `NULL`")
  }

  if (!is.character(life_stage) && !is.null(life_stage)) {
    stop("`life_stage` must be a character or `NULL`")
  }

  if (!is.data.frame(bootstrap_tpcs)) {
    stop("`bootstrap_tpcs` must be a  `data.frame` or `tibble`
    as produced by `mappestRisk::predict_curves()` function with
    `propagate_uncertainty = TRUE` and `n_boots_samples > 0`.")
  }

  if (!all(c("model_name", "boot_iter", "temp", "dev_rate", "curvetype") %in% names(bootstrap_tpcs))) {
    stop("`bootstrap_tpcs` must be a  `data.frame` or `tibble`
    as produced by `mappestRisk::predict_curves()` function with
    `propagate_uncertainty = TRUE` and `n_boots_samples > 0`.")
  }

  if (nrow(bootstrap_tpcs) == 0) {
    stop("No bootstrapped or estimate predictions are available.
         Please check `bootstrap_tpcs` and consider using a different model or
         setting `propagate_uncertainty` to `FALSE` in `predict_curves()`")
  }

  if (!any(bootstrap_tpcs$curvetype == "uncertainty")) {
    warning("No bootstrapped predictions available. Please check `bootstrap_tpcs`.
             Plotting only the central curve.")
  }

  if (alpha < 0 | alpha > 1) {
    stop("alpha must be a number between 0 (complete transparency) and 1 (solid line).")
  }

  ## end checks ##

  devdata <- dplyr::tibble(temp,
                           dev_rate)

  central_curve <- bootstrap_tpcs |>
    dplyr::filter(curvetype == "estimate")

  uncertainty_curves <- bootstrap_tpcs |>
    dplyr::filter(curvetype == "uncertainty")

  my_title <- substitute(italic(paste(x)), list(x = species))

  plot_boot_tpcs <- ggplot2::ggplot() +
    ggplot2::geom_line(data = uncertainty_curves,
                       ggplot2::aes(x = temp,
                                    y = dev_rate,
                                    group = boot_iter),
                       col = "#0E4D62", #'#586A64',
                       alpha = alpha,
                       linewidth = 0.32) +
    ggplot2::geom_line(data = central_curve,
                       ggplot2::aes(x = temp,
                                    y = dev_rate),
                       col = "#CF8143", #'#B1492E',
                       linewidth = .85) +
    ggplot2::geom_point(data = devdata,
                        ggplot2::aes(temp, dev_rate),
                        size = 2) +
    ggplot2::facet_wrap(~model_name, scales = "free") +
    ggplot2::scale_x_continuous(limits = c(0, 50)) +
    ggplot2::scale_y_continuous(limits = c(0, 1.5*max(devdata$dev_rate, na.rm = TRUE))) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(x = 'Temperature',
                  y = italic(R)(T)~(d^-1),
                  title = my_title,
                  subtitle = life_stage,
                  caption = "Bootstrapping with residual resampling, see `rTPC` package vignettes"
    )
  return(plot_boot_tpcs)
}
