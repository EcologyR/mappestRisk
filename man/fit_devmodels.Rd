% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_devmodels.R
\name{fit_devmodels}
\alias{fit_devmodels}
\title{Fit Thermal Performance Curves}
\source{
The dataset used in the example was originally published in Satar & Yokomi (2022) under the CC-BY-NC license
}
\usage{
fit_devmodels(temp = NULL, dev_rate = NULL, model_name = NULL)
}
\arguments{
\item{temp}{a vector of temperatures used in the experiment.
It should have at least four different temperatures and must contain only numbers
without any missing values.}

\item{dev_rate}{a vector of estimated development rates corresponding to each temperature.
These rates are calculated as the inverse of the number of days to complete the transition
from the beginning of a certain life stage to the beginning of the following at each temperature.
It must be numeric and of the same length as \code{temp}.}

\item{model_name}{a string or a vector that specifies the model(s) to use for
fitting the Thermal Performance Curves. Options include "all" or specific
models listed in \link{available_models}. These models typically exhibit a common unimodal, left-skewed shape.}
}
\value{
A table in \code{tibble} format with estimates and standard errors
for each parameter of the models specified by the user that have adequately
converged. Models are sorted based on their Akaike Information Criterion (AIC) values,
with the best fitting models shown first. Fitted models are also provided in list format
in the \code{model_list} column and can be accessed using \code{\link[=get_fitted_model]{get_fitted_model()}} for
for further inspection.
It is important to consider ecological criteria alongside statistical information.
For additional help in model selection,
we recommend using \code{\link[=plot_devmodels]{plot_devmodels()}} and consulting relevant literature.
}
\description{
Fit nonlinear regression models to data representing how development rate changes
with temperature (known as Thermal Performance Curves), based on
\code{\link[nls.multstart:nls_multstart]{nls.multstart::nls_multstart()}} approach to development rate data across temperatures.
The fitting procedure is built upon previous packages for starting values estimation,
namely \code{rTPC} and \code{devRate}.
}
\examples{
data("aphid")

fitted_tpcs_aphid <- fit_devmodels(temp = aphid$temperature,
                                   dev_rate = aphid$rate_value,
                                   model_name = c("lactin2", "briere2", "mod_weibull")
                                   )
fitted_tpcs_aphid

}
\references{
Angilletta, M.J., (2006). Estimating and comparing thermal performance curves. \if{html}{\out{<i>}}J. Therm. Biol.\if{html}{\out{</i>}} 31: 541-545.
(for reading on model selection in TPC framework)

Padfield, D., O'Sullivan, H. and Pawar, S. (2021). \if{html}{\out{<i>}}rTPC\if{html}{\out{</i>}} and \if{html}{\out{<i>}}nls.multstart\if{html}{\out{</i>}}: A new pipeline to fit thermal performance curves in \code{R}. \if{html}{\out{<i>}}Methods Ecol Evol\if{html}{\out{</i>}}. 00: 1-6

Rebaudo, F., Struelens, Q. and Dangles, O. (2018). Modelling temperature-dependent development rate and phenology in arthropods: The \code{devRate} package for \code{R}. \if{html}{\out{<i>}}Methods Ecol Evol\if{html}{\out{</i>}}. 9: 1144-1150.

Satar, S. and Yokomi, R. (2002). Effect of temperature and host on development of \if{html}{\out{<i>}}Brachycaudus schwartzi\if{html}{\out{</i>}} (Homoptera: Aphididae). \if{html}{\out{<i>}}Ann. Entomol. Soc. Am.\if{html}{\out{</i>}} 95: 597-602.
}
\seealso{
\code{\link[nls.multstart:nls_multstart]{nls.multstart::nls_multstart()}} for structure of model fitting approach

\code{\link[devRate:devRateEqList]{devRate::devRateEqList()}} for information on several equations

\code{browseVignettes("rTPC")} for model names, start values searching workflows and
bootstrapping procedures using both \code{rTPC} and \code{nls.multstart} packages.
}
