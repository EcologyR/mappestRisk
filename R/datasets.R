#' <i>Brachycaudus schwartzi</i> whole life cycle development rate across
#' temperatures
#'
#' A modified data set from Table 1 in Satar and Yokomi (2002) on days of
#' development for *Brachycaudus schwartzi* across different constant
#' temperatures and life stages
#'
#' @name aphid
#' @usage data(aphid)
#' @format ## `aphid`
#' A data frame with 7 rows and 5 columns.
#'    The workflow is reproducible and available in `/data-raw` folder of the
#'    <a href="https://github.com/EcologyR/mappestRisk">mappestRisk GitHub repository</a>,
#'    which includes both the original summarized data set -`satar_data.xlsx`-
#'    and the R script with the dev. days to dev. rate conversion
#'    in `prepare_aphid.R`.
#'
#'
#' \describe{
#'   \item{reference}{"Satar2002" refers to the source paper as cited below in section `Source`.}
#'   \item{temperature}{Temperature treatments (ºC).}
#'   \item{dev_days}{Development days (i.e., days to fulfill development requirements from a life-stage to the following)}
#'   \item{rate_value}{Rate of Development (1/days), the reciprocal of Development days, see `dev_days`}
#'   \item{stage}{Life stage or instar evaluated. In this case, only data of the whole immature stages (i.e., nymphs) were used}
#' }
#' @source
#' Satar, S. and Yokomi, R. (2002). Effect of temperature and host on development of
#' *Brachycaudus schwartzi* (Homoptera: Aphididae).
#' Ann. Entomol. Soc. Am. 95: 597-602. \doi{10.1603/0013-8746(2002)095[0597:EOTAHO]2.0.CO;2}.
#'
#' Licence: <a href="https://creativecommons.org/licenses/by-nc/3.0">CC BY-NC 3.0</a> (modified material).
"aphid"



#' Country names
#'
#' @format ## `country_names`
#' A character vector of country names (length = 231 countries)
#' @source https://gadm.org
"country_names"


#' Available Models Table
#'
#' Table containing the available models to be fit using [fit_devmodels()].
#' These models come from two other packages:
#' <a href="https://cran.r-project.org/package=devRate">`devRate` </a> and
#' <a href="https://padpadpadpad.github.io/rTPC/">`rTPC` </a>.
#'
#' @name available_models
#' @usage data("available_models")
#' @format ## `available_models`
#' A data.frame/tibble with 13 rows and 6 columns:
#'
#' \describe{
#'
#'   \item{model_name}{Model name to be used within [fit_devmodels()].}
#'
#'   \item{package}{names of the packages used by [fit_devmodels()] to obtain
#'   appropriate start values for the user-provided data.
#'    When the package is <a href="https://padpadpadpad.github.io/rTPC/">rTPC package</a>,
#'    start values are automatically computed with [rTPC::get_start_vals()],
#'    which in turn relies on [nls.multstart::nls_multstart()].
#'    When the package is <a href="https://github.com/frareb/devRate">devRate package</a>,
#'    iterative starting values are computed using [nls.multstart::nls_multstart()],
#'    using the parameters published in [devRate::devRateEqStartVal()]
#'    as first attempts to iterate.
#'    As an exception, if `model_name == "briere1"`, generic starting values are provided
#'    and advised to the user due to the unrealistic value of some parameters in the
#'    `devRate` data set.}
#'
#'   \item{source_model_name}{name of the function in the source packages `rTPC` and `devRate`.}
#'
#'   \item{formula, working_formula, n_params}{formulas used for model fitting.}
#' }
#'
#' @source
#'
#' Rebaudo, F., Struelens, Q., and Dangles, O. (2018). Modelling temperature-dependent
#'  development rate and phenology in arthropods: The `devRate` package for R.
#'  Methods Ecol Evol. 9: 1144-1150. \doi{10.1111/2041-210X.12935}.
#'
#' Padfield, D., O´Sullivan, H., and Pawar, S., (2021). `rTPC` and `nls.multstart`:
#' a new pipeline to fit thermal performance curves in R. Methods Ecol Evol. 12: 1138-1143.
#' \doi{10.1111/2041-210X.13585}.
#'
#'
#'
"available_models"
