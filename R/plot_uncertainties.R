#' Draw bootstrapped TPCs to visualize uncertainty bands in parameter estimation of TPC fitting
#'
#' @param bootstrap_uncertainties_tpcs a `tibble` object output by [mappestRisk::predict_curves()]
#' with as many curves -TPCs- as the number of iterations provided in `n_boots_samples` argument
#' therein whenever `propagate_uncertainty` was `TRUE`. Each TPC in this `tibble`  consist of
#' a collection of predictions for a set of temperatures from `temp - 20` to `temp + 15` with a
#' resolution of 0.1ºC and a unique identifier called `iter`.
#' In addition to the uncertainty TPCs, the estimate TPC is also explicit in the output tibble.
#'
#' @param temp a vector containing temperature treatments (predictor variable).
#' It must have at least four different temperature treatments. It must be numeric
#' and not containing NAs.
#'
#' @param dev_rate a vector containing development rate estimates, calculated as
#' the reciprocal of days of development at each temperature (i.e., 1/days of development).
#' It must be numeric and of same length as `temp`.
#'
#' @param species a string containing the name of the species in the study. The function
#' converts the string to a title in the <ggplot> object in italics.
#'
#' @param life_stage a string containing the life stage studied for this rate-temperature
#' relationship. The function converts the string to a subtitle in the <ggplot> object.
#'
#' @returns a <ggplot> object containing the visual representation of the estimate TPC and the
#' bootstrapped uncertainty curves as a ribbon. Each model is represented in a facet, and data points
#' are also explicit.
#'
#' @export
#'
#' @examples
#' data("b.schwartzi_satar2002")
#'
#' fitted_tpcs_bschwartzi <- fit_devmodels(temp = b.schwartzi_satar2002$temperature,
#'                                       dev_rate = b.schwartzi_satar2002$rate_value,
#'                                       model_name = "all")
#'
#' plot_devmodels(temp = b.schwartzi_satar2002$temperature,
#'                dev_rate = b.schwartzi_satar2002$rate_value,
#'                fitted_parameters = fitted_tpcs_bschwartzi,
#'                species = "Brachycaudus swartzi",
#'                life_stage = "Nymphs") #choose "briere2", "thomas" and "lactin2"
#'
#' #3. Obtain prediction TPCs with bootstraps for propagating uncertainty:
#' tpc_preds_boots_bschwartzi <- predict_curves(temp = b.swartzi_satar2002$temperature,
#'                                              dev_rate = b.swartzi_satar2002$rate_value,
#'                                              fitted_parameters = fitted_tpcs_bswartzi,
#'                                              model_name_2boot = c("briere2", "thomas", "lactin2"),
#'                                              propagate_uncertainty = TRUE,
#'                                              n_boots_samples = 100)
#'
#'
#' head(tpc_preds_boots_bschwartzi)
#'
#' #4. Plot bootstrapped curves:
#'
#' plot_uncertainties(bootstrap_uncertainties_tpcs = tpc_preds_boots_bschwartzi,
#'                    temp = b.swartzi_satar2002$temperature,
#'                    dev_rate = b.swartzi_satar2002$rate_value,
#'                    species = "Brachycaudus schwartzi",
#'                    life stage = "Nymphs")
#'

plot_uncertainties <- function(bootstrap_uncertainties_tpcs,
                               temp,
                               dev_rate,
                               species,
                               life_stage) {

  if(any(is.na(dev_rate))) {
    stop("development rate data have NAs; please consider removing them or fixing them")
  }
  if(any(is.na(temp))) {
    stop("temperature data have NAs; please consider removing them or fixing them")
  }
  if(!is.numeric(temp)) {
    stop("temperature data is not numeric. Please check it.")
  }
  if(!is.numeric(dev_rate)) {
    stop("development rate data is not numeric. Please check it.")
  }
  if(length(temp) != length(dev_rate)) {
    stop("development rate and temperature inputs are not of same length. Please check it.")
  }
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
  devdata <- tibble(temp,
                    dev_rate)
  central_curve <- bootstrap_uncertainties_tpcs |>
    filter(curvetype == "estimate")
  uncertainty_curves <- bootstrap_uncertainties_tpcs |>
    filter(curvetype == "uncertainty")

  my_title <- substitute(italic(paste(x)), list(x = species))
  plot_boot_tpcs <- ggplot() +
    geom_line(data = uncertainty_curves,
              aes(x = temp,
                  y = pred,
                  group = iter),
              col = "#0E4D62", #'#586A64',
              alpha = 0.08,
              linewidth = 0.32) +
    geom_line(data = central_curve,
              aes(x = temp,
                  y = pred),
              col = "#CF8143", #'#B1492E',
              linewidth = .85) +
    geom_point(data = devdata,
               aes(temp, dev_rate),
               size = 2) +
    facet_wrap(~model_name, scales = "free")+
    scale_x_continuous(limits = c(0, 50))+
    scale_y_continuous(limits = c(0, max(bootstrap_uncertainties_tpcs$pred)))+
    ggthemes::theme_few(base_size = 12) +
    labs(x = 'Temperature (ºC)',
         y = italic(R)(T)~(d^-1),
         title = my_title,
         subtitle = life_stage,
         caption = "Bootstrapping with residual resampling, see `rTPC` package vignettes"
    )
  return(plot_boot_tpcs)
}
