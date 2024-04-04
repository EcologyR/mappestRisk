#' <b>Available Models Table</b>
#'
#' This data.frame, called `dev_model_table` in your environment once the package is loaded,
#' contains the available models to fit using [fit_devmodels()]. It is useful for the user to know it
#' before applying functions in the package.
#'
#' @name available_models
#' @docType data
#' @usage data("available_models")
#' @format ## `dev_model_table`
#' A data.frame/tibble with 13 rows and 6 columns. This package contain the equation names of the models that are
#' able to be fitted with [fit_devmodels()] and the equivalent names in their respective
#' source packages underlying the start value exploration function and formulas used in-code to fit models.
#' These models come from two packages that are already published:
#'  <a href="https://github.com/frareb/devRate">`devRate` </a> and <a href="https://github.com/padpadpadpad/rTPC">`rTPC` </a>. </br>
#'
#' \describe{
#'   \item{model_name}{This is the name of the model equations that the user can use as input in the package functions.}
#'   \item{package}{names of the packages used by [fit_devmodels()] to obtain appropriate start values for the user-providen data.
#'    When the package is <a href="https://github.com/padpadpadpad/rTPC">rTPC package</a>, start values are
#'    automatically computed with [rTPC::get_start_vals()], which in turn relies on [nls.multstart::nls_multstart()]. When the package is  <a href="https://github.com/frareb/devRate">devRate package</a>,
#'    iterative starting values are computed using [nls.multstart::nls_multstart()], using the parameters published in
#'    [devRate::devRateEqStartVal()] as first attempts to iterate. As an exception, if `model_name == "briere1"`,
#'    generic starting values are provided and advised to the user due to unrealistic value of some parameters in the
#'    [devRate] data set.}
#'   \item{source_model_name}{function name in source packages used in the background of this package}
#'   \item{formula, working formula, params_formula}{formulas allowing automation of model fitting, with no information of interest for the user}
#' }
#' @source {<b>Rebaudo, F., Struelens, Q., and Dangles, O. (2018)</b>. Modelling temperature-dependent development rate and phenology in
#' arthropods: The `devRate` package for r. Methods Ecol Evol. 9: 1144-1150. <https://doi.org/10.1111/2041-210X.12935>
#'
#' <b>Padfield, D., O´Sullivan, H., and Pawar, S., (2021)</b>. `rTPC` and `nls.multstart`: a new pipeline to fit thermal
#' performance curves in r. Methods Ecol Evol. 12: 1138-1143 <https://doi.org/10.1111/2041-210X.13585>}.</br>
#'
#'
#'
"dev_model_table"

