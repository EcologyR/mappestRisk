#' <b><i>Brachycaudus schwartzi</i> whole life cycle development rate across temperatures</b>
#'
#' A modified data set from Table 1 in Satar and Yokomi (2002) on days of development for Brachycaudus schwartzi across different constant temperatures and life stages
#' Scientific Article
#'
#' @name b.schwartzi_satar2002
#' @docType data
#' @usage data(b.schwartzi_satar2002)
#' @format ## `b.schwartzi_satar2002`
#' A data frame with 7 rows and 5 columns.
#'    The workflow is reproducible and available in `/data-raw` folder of the <a href="https://github.com/EcologyR/mappestRisk">mappestRisk GitHub repository</a>,
#'    which includes both the original summarized data set -`homalodisca_vitripennis_pilkington2014.xlsx`- and the
#'    R script with the dev. days to dev. rate conversion in `prepare_b.schwartzi_satar2002.R`.
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
"b.schwartzi_satar2002"
