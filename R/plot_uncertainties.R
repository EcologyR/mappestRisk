#' Draw bootstrapped Thermal Performance Curves (TPCs) to visualize uncertainty bands in parameter estimation of TPC fitting
#'
#' @param bootstrap_uncertainties_tpcs a `tibble` A tibble object output by [predict_curves()], containing TPCs
#' with uncertainty bands. Each TPC consists of predictions for temperatures ranging from `temp - 20` to `temp + 15`
#' with a resolution of 0.1°C. The tibble also includes an estimate TPC.
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
#' @param species A string containing the name of the species in the study. The function converts the string to a title
#' in the ggplot object in italics.
#'
#' @param life_stage A string containing the life stage studied for this rate-temperature relationship. The function
#' converts the string to a subtitle in the ggplot object.
#'
#' @returns A ggplot object containing the visual representation of the estimate TPC and the bootstrapped uncertainty
#' curves as a ribbon. Each model is represented in a facet, and data points are also explicit.
#'
#' @seealso `browseVignettes("rTPC")` for model names, start values searching workflows, and
#'  bootstrapping procedures using both [rTPC::get_start_vals()] and [nls.multstart::nls_multstart()]
#'
#'  [fit_devmodels()] for fitting Thermal Performance Curves to development rate data, which is in turn based on [nls.multstart::nls_multstart()].
#'  [predict_curves()] for bootstrapping procedure based on the above-mentioned `rTPC` vignettes.
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
#' @source
#' The dataset used in the example was originally published in Satar & Yokomi (2022) under the CC-BY-NC license
#'
#'
#' @export
#'
#' @examples
#' data("aphid")
#'
#' fitted_tpcs_aphid <- fit_devmodels(temp = aphid$temperature,
#'                                         dev_rate = aphid$rate_value,
#'                                         model_name = "all")
#'
#' plot_devmodels(temp = aphid$temperature,
#'                dev_rate = aphid$rate_value,
#'                fitted_parameters = fitted_tpcs_aphid,
#'                species = "Brachycaudus swartzi",
#'                life_stage = "Nymphs") # choose "lactin2"
#'
#' # Obtain prediction TPCs with bootstraps for propagating uncertainty:
#' tpc_preds_boots_aphid <- predict_curves(temp = aphid$temperature,
#'                                              dev_rate = aphid$rate_value,
#'                                              fitted_parameters = fitted_tpcs_aphid,
#'                                              model_name_2boot = "lactin2",
#'                                              propagate_uncertainty = TRUE,
#'                                              n_boots_samples = 100)
#'
#' print(tpc_preds_boots_aphid)
#'
#' # Plot bootstrapped curves:
#'
#' plot_uncertainties(bootstrap_uncertainties_tpcs = tpc_preds_boots_aphid,
#'                    temp = aphid$temperature,
#'                    dev_rate = aphid$rate_value,
#'                    species = "Brachycaudus schwartzi",
#'                    life_stage = "Nymphs")

plot_uncertainties <- function(bootstrap_uncertainties_tpcs,
                               temp,
                               dev_rate,
                               species,
                               life_stage) {

  check_data(temp, dev_rate)

  if(!is.character(species) &&
     !is.null(species)) {
    stop("`species` must be a character with the scientific name of your species, or `NULL`")
  } else if (is.null(species)){
    species <- NULL
  }
  if(!is.character(life_stage) &&
     !is.null(life_stage)) {
    stop("`life_stage` must be a character or `NULL`")
  } else if(is.null(life_stage)){
    life_stage <- NULL
  }
  if(!is.data.frame(bootstrap_uncertainties_tpcs)) {
    stop("`bootstrap_uncertainties_tpcs` must be a  `data.frame` or `tibble`
    inherited from the output of `mappestRisk::predict_curves()` function with
    `propagate_uncertainty = TRUE` and `n_boots_samples > 10`.")
  }
  if(suppressWarnings(any(!c("model_name", "iter", "temp", "pred", "curvetype") %in% colnames(bootstrap_uncertainties_tpcs)))){
    stop("`bootstrap_uncertainties_tpcs` must be a  data.frame inherited from the
    output of `mappestRisk::predict_curves()` function with
    `propagate_uncertainty = TRUE` and `n_boots_samples > 10`.")
  }
  if(nrow(bootstrap_uncertainties_tpcs) == 0){
    warning("No bootstrapped predictions were obtained. Please check `bootstrap_uncertainties_tpcs`")
  }
  devdata <- dplyr::tibble(temp,
                           dev_rate)
  central_curve <- bootstrap_uncertainties_tpcs |>
    dplyr::filter(curvetype == "estimate")
  uncertainty_curves <- bootstrap_uncertainties_tpcs |>
    dplyr::filter(curvetype == "uncertainty")

  my_title <- substitute(italic(paste(x)), list(x = species))
  plot_boot_tpcs <- ggplot2::ggplot() +
    ggplot2::geom_line(data = uncertainty_curves,
                       ggplot2::aes(x = temp,
                                    y = pred,
                                    group = iter),
                       col = "#0E4D62", #'#586A64',
                       alpha = 0.08,
                       linewidth = 0.32) +
    ggplot2::geom_line(data = central_curve,
                       ggplot2::aes(x = temp,
                                    y = pred),
                       col = "#CF8143", #'#B1492E',
                       linewidth = .85) +
    ggplot2::geom_point(data = devdata,
                        ggplot2::aes(temp, dev_rate),
                        size = 2) +
    ggplot2::facet_wrap(~model_name, scales = "free")+
    ggplot2::scale_x_continuous(limits = c(0, 50))+
    ggplot2::scale_y_continuous(limits = c(0, max(bootstrap_uncertainties_tpcs$pred, na.rm = TRUE)))+
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(x = 'Temperature (°C)',
                  y = italic(R)(T)~(d^-1),
                  title = my_title,
                  subtitle = life_stage,
                  caption = "Bootstrapping with residual resampling, see `rTPC` package vignettes"
    )
  return(plot_boot_tpcs)
}
