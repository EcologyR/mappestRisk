#' <b><i>Homalodisca vitripenis</i> whole life cycle development rate across temperatures</b>
#'
#' A reconstructed data set from Pilkington et al. (2014) on days of development across different constant temperatures and life stages
#' Scientific Article
#'
#' @name h.vitripennis_pilkington2014
#' @docType data
#' @usage data(h.vitripennis_pilkington2014)
#' @format ## `h.vitripennis_pilkington2014`
#' A data frame with 357 rows and 4 columns. This data.frame is a simulated reconstruction performed on pooled, summarized data in the source article
#'    (Pilkington et al. 2014) using simulations under normal distribution based on mean estimate,
#'    standard errors and sample sizes, as suggested by <a href="https://doi.org/10.1002/jrsm.1331">Papadimitropoulou et al. (2019)</a>. </br>
#'
#'    The workflow is reproducible and available in `/data-raw` folder of the <a href="https://github.com/EcologyR/mappestRisk">mappestRisk GitHub repository</a>,
#'    which includes both the original summarized data set -`homalodisca_vitripennis_pilkington2014.xlsx`- and the
#'    R script with the simulation procedures in `clean&sim_devdata_homalodisca_vitripennis.R`.
#'    .
#'
#' \describe{
#'   \item{reference}{"Pilkington2014" refers to the source paper as cited below in section `Source`.}
#'   \item{temperature}{Temperature treatments (ºC).}
#'   \item{rate_development}{Rate of Development (1/days), i.e. days necessary to fulfill development of all life stages from egg to adult (see `stage`)}
#'   \item{stage}{Life stage or instar evaluated. In this case, only data of the whole life cycle (from egg to adult) were used}
#' }
#' @source {<b>Pilkington, L., Lewis, M., Jeske, D. and Hoddle, M. (2014)</b>. Calculation and Thematic Mapping of Demographic Parameters
#'   for <i>Homalodisca vitripennis</i> (Hemiptera: Cicadellidae) in California.
#'   Ann. Entomol. Soc. Am. 107: 424-434. <https://doi.org/10.1603/AN13144>}.</br>
#'
#'   Licence: <a href="https://creativecommons.org/licenses/by-nc/3.0">CC BY-NC 3.0</a> (modified material).
"h.vitripennis_pilkington2014"
