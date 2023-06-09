% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxiliary_functions_modelfitting.R
\name{start_vals_devRate}
\alias{start_vals_devRate}
\title{start_vals_devRate(): obtain starting values for TPC fitting}
\usage{
start_vals_devRate(model_name, temperature, dev_rate)
}
\arguments{
\item{model_name}{one or several of the models listed in \code{dev_model_table} corresponding to
source package \link{devRate} to parameterise thermal performance curves}

\item{temperature}{a vector containing temperature treatments (predictor variable). It must have at least four different temperature treatments. The function works for both
aggregated data (i.e. one development rate value for each temperature treatment, which is representative of the cohort average development
rate) or individual data (i.e. one observation of development rate for each individual in the experiment at each temperature)}

\item{dev_rate}{a vector containing development rate estimates (1/days of development); must be of same length than temp.
The function works for both aggregated data (i.e. one development rate value for each temperature treatment, which is representive of the cohort average development
rate) or individual data (i.e. one observation of development rate for each individual in the experiment at each temperature)}
}
\value{
a vector with parameter estimates approximated iteratively with \code{\link[nls.multstart:nls_multstart]{nls.multstart::nls_multstart()}} based on
estimates compilated in \code{\link[devRate:devRateEqStartVal]{devRate::devRateEqStartVal()}}.
}
\description{
Auxiliary function working under \code{fit_devmodels()}; user won't be using this function in a normal workflow of \link{mappestRisk} package
}
