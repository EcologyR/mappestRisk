#' Get fitted model object
#'
#' @param fitted_df A table with fitted models, as produced by [fit_devmodels()].
#' @param model_name Character. Name of a fitted model, see [available_models].
#'
#' @returns A model object
#' @export
#'
#' @examples
#' data("aphid")
#'
#' fitted_tpcs_aphid <- fit_devmodels(temp = aphid$temperature,
#'                                    dev_rate = aphid$rate_value,
#'                                    model_name = c("lactin2", "briere2", "ratkowsky")
#'                                    )
#' get_fitted_model(fitted_tpcs_aphid, "briere2")
get_fitted_model <- function(fitted_df = NULL, model_name = NULL) {

  stopifnot(inherits(fitted_df, "data.frame"))
  stopifnot(is.character(model_name) & length(model_name) == 1)
  if (!model_name %in% unique(fitted_df$model_name)) {
    stop(model_name, " does not appear in the table of fitted models.")
  }

  fitted_df$model_fit[fitted_df$model_name == model_name][[1]]

}
class(fitted_tpcs_aphid)
