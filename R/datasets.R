#' <i>Brachycaudus schwartzi</i> whole life cycle development rate across temperatures
#'
#' A modified data set from Table 1 in Satar and Yokomi (2002) on days of development
#' for *Brachycaudus schwartzi* across different constant temperatures and life stages
#'
#' @name aphid
#' @usage data(aphid)
#' @format ## `aphid`
#' A data frame with 7 rows and 5 columns.
#'    The workflow is reproducible and available in `/data-raw` folder of the <a href="https://github.com/EcologyR/mappestRisk">mappestRisk GitHub repository</a>,
#'    which includes both the original summarized data set -`satar_data.xlsx`- and the
#'    R script with the dev. days to dev. rate conversion in `prepare_aphid.R`.
#'
#'
#' \describe{
#'   \item{reference}{"Satar2002" refers to the source paper as cited below in section `Source`.}
#'   \item{temperature}{Temperature treatments (ºC).}
#'   \item{dev_days}{Development days (i.e., days to fulfill development requirements from a life-stage to the following)}
#'   \item{rate_value}{Rate of Development (1/days), the reciprocal of Development days, see `dev_days`}
#'   \item{stage}{Life stage or instar evaluated. In this case, only data of the whole immature stages (i.e., nymphs) were used}
#' }
#' @source {<b>Satar, S. and Yokomi, R. (2002)</b>. Effect of temperature and host on development of
#'  <i>Brachycaudus schwartzi</i> (Homoptera: Aphididae).
#'   Ann. Entomol. Soc. Am. 95: 597-602. <https://doi.org/10.1603/0013-8746(2002)095[0597:EOTAHO]2.0.CO;2>}.</br>
#'
#'   Licence: <a href="https://creativecommons.org/licenses/by-nc/3.0">CC BY-NC 3.0</a> (modified material).
"aphid"



#' Country names
#'
#' @format ## `country_names`
#' A character vector of country names (length = 231 countries)
#' @source https://gadm.org
"country_names"


#' Available Models Table
#'
#' This data.frame, called `available_models` in your environment once the package is loaded,
#' contains the available models to fit using [fit_devmodels()]. It is useful for the user to know it
#' before applying functions in the package.
#'
#' @name available_models
#' @usage data("available_models")
#' @format ## `available_models`
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
"available_models"
