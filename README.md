
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mappestRisk

<!-- badges: start -->

[![R-CMD-check](https://github.com/EcologyR/templateRpackage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EcologyR/templateRpackage/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/EcologyR/templateRpackage/branch/master/graph/badge.svg)](https://app.codecov.io/gh/EcologyR/templateRpackage?branch=master)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- [![CodeFactor](https://www.codefactor.io/repository/github/ecologyr/templaterpackage/badge)](https://www.codefactor.io/repository/github/ecologyr/templaterpackage) -->

<!-- badges: end -->

The goal of mappestRisk package is to facilitate the transition from
development data of pests obtained in lab-controlled conditions to
understandable forecasts assessing risk of pest occurrence in a given
region.

For that purpose, mappestRisk is built upon previous efforts such as
`devRate` (Rebaudo, Struelens, and Dangles 2018), `rTPC` and
`nls.multstart` packages (Padfield, O’Sullivan, and Pawar 2021) and a
methodology for predicting climatic suitability based on fundamental
thermal niche as estimated by mechanistic, process-based approaches
suggested in Taylor et al. (2019) . Therefore, mappestRisk has three
different modules: *(1) model fitting & selection* using a set of the
most widely used equations describing developmental responses to
temperature under the `nls.multstart` (Padfield and Matheson 2020) and
`nlme` (Pinheiro, Bates 2022) frameworks, with visualization of model
fitting to help model selection by the user; (2) *thermal traits
extraction:* including selection of the suitability threshold guiding
the forecast (i.e. obtaining the temperatures at which estimated
performance lies upon a performance higher threshold percentage); and
(3) *climatic data extraction & visualization* with either exportable
rasters or interactive maps with `leaflet` (Cheng, Karambelkar, and Xie
2022).

## Installation

``` r
# install.packages("devtools")
# devtools::install_github("EcologyR/mappestRisk")

#or alternatively
# remotes::install_github("EcologyR/mappestRisk")

#and load the package
#library(mappestRisk)
devtools::load_all() #for now, provisionally
#> ℹ Loading mappestRisk
#> The legacy packages maptools, rgdal, and rgeos, underpinning this package
#> will retire shortly. Please refer to R-spatial evolution reports on
#> https://r-spatial.org/r/2023/05/15/evolution4.html for details.
#> This package is now running under evolution status 0
#> Warning: package 'testthat' was built under R version 4.2.2
#> Warning: Objects listed as exports, but not present in namespace:
#> • sim_tpcs_uncertainty

library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 4.2.3
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.2.3
#> 
#> Attaching package: 'dplyr'
#> 
#> The following object is masked from 'package:testthat':
#> 
#>     matches
#> 
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> 
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(nls.multstart)
#> Warning: package 'nls.multstart' was built under R version 4.2.3
library(rTPC)
#> Warning: package 'rTPC' was built under R version 4.2.3
library(devRate)
#> Warning: package 'devRate' was built under R version 4.2.3
library(tidyr)
#> Warning: package 'tidyr' was built under R version 4.2.3
#> 
#> Attaching package: 'tidyr'
#> 
#> The following object is masked from 'package:testthat':
#> 
#>     matches
library(purrr)
#> Warning: package 'purrr' was built under R version 4.2.3
#> 
#> Attaching package: 'purrr'
#> 
#> The following object is masked from 'package:testthat':
#> 
#>     is_null
```

If you want to clone or fork the repository or open and read some
issues, you can find the code
[here](https://github.com/EcologyR/mappestRisk).

## Example: mappestRisk workflow

### 1. Fit a thermal performance curve (TPC) to your data:

In this example, we’ll show how to fit one to several thermal
performance curves to a data set of development rate variation across
temperatures[^1]. The following code provides an example as given in
`fit_devmodels()` function documentation, with a data table showing the
output of fitted models.

``` r
data("b.schwartzi_satar2002")

fitted_tpcs_aphid <- fit_devmodels(temp = b.schwartzi_satar2002$temperature,
                                   dev_rate = b.schwartzi_satar2002$rate_value,
                                   model_name = "all")
print(fitted_tpcs_aphid)
#> # A tibble: 68 × 8
#>    param_name start_vals param_est    param_se model_name model_AIC model_BIC
#>    <chr>           <dbl>     <dbl>       <dbl> <chr>          <dbl>     <dbl>
#>  1 a               0.145     0.143     0.00629 beta           -43.0     -43.3
#>  2 b              25        26.7       1.10    beta           -43.0     -43.3
#>  3 c              25       202.    13601.      beta           -43.0     -43.3
#>  4 d               2       100     12049.      beta           -43.0     -43.3
#>  5 e               2        32.6    2283.      beta           -43.0     -43.3
#>  6 rmax            0.145     0.143     0.00649 boatman        -42.4     -42.7
#>  7 tmin           15         0       102.      boatman        -42.4     -42.7
#>  8 tmax           32.5      43.3      62.7     boatman        -42.4     -42.7
#>  9 a               1.1       1.45      1.03    boatman        -42.4     -42.7
#> 10 b               0.4       2.35     18.7     boatman        -42.4     -42.7
#> # ℹ 58 more rows
#> # ℹ 1 more variable: model_fit <list>
```

### 2. Plot the fitted TPCs and select the most appropriate:

To help select which model might be more appropriate, the function
`plot_devmodels()` draws the predicted TPCs for each
adequately-converged model. This step aims to improve model selection
based not only on statistical criteria (i.e. AIC and number of
parameters) but also on ecological realism, since curves can be
graphically checked to select realistic shapes and thermal traits
–vertical cuts with x-axis such as $CT_\min$, $CT_\max$ and
$T_\text{opt}$ .

``` r
plot_devmodels(temp = b.schwartzi_satar2002$temperature,
               dev_rate = b.schwartzi_satar2002$rate_value,
               fitted_parameters = fitted_tpcs_aphid,
               species = "Brachycaudus schwartzi",
               life_stage = "Nymphs")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

### 3. Calculate predictions and bootstrap 100 TPCs for propagating parameter uncertainty

After careful examination of fitted TPCs in `plot_devmodels()`, the
predictions of the selected models must be allocated in a `data.frame`
in order to continue the package workflow towards climatic projection
for pest risk analysis. Additionally, we recommend here to propagate
uncertainty in parameter estimation of the fitted and selected TPC
models using bootstrap procedures following vignettes in Padfield,
O’Sullivan, and Pawar (2021). This can be done with the function
`predict_curves()` by setting the argument `propagate_uncertainty` to be
`TRUE` and by providing a number of bootstrap sampling iterations in the
argument `n_boots_samples`.

This function requires some time to compute bootstraps and predictions
of the multiple TPCs it generates. Hence, it is important to use only
one or few TPC models in the argument `model_name_2boot` based on visual
examination from `plot_devmodels()` with reasonable selection. We
discourage to bootstrap all TPC models for computational and time
consuming reasons.

In our example, we choose three models that performed similarly and,
unlike others with similar or even higher AICs, had a markedly higher
left-skewness, e.g., *briere2, thomas* and *lactin2.* After a few
minutes, we obtain the `data.frame`:

``` r
library(boot)
library(car)
#> Loading required package: carData
#> 
#> Attaching package: 'car'
#> The following object is masked from 'package:boot':
#> 
#>     logit
#> The following object is masked from 'package:purrr':
#> 
#>     some
#> The following object is masked from 'package:dplyr':
#> 
#>     recode


preds_boots_aphid <-predict_curves(temp = b.schwartzi_satar2002$temperature,
                                   dev_rate = b.schwartzi_satar2002$rate_value,
                                   fitted_parameters = fitted_tpcs_aphid,
                                   model_name_2boot = c("briere2",
                                                        "thomas",
                                                        "lactin2"),
                                   propagate_uncertainty = TRUE,
                                   n_boots_samples = 100)
#> [1] "Predicting bootstrapped TPCs 0.1 %"
#> [1] "Predicting bootstrapped TPCs 0.2 %"
#> [1] "Predicting bootstrapped TPCs 0.2 %"
#> [1] "Predicting bootstrapped TPCs 0.3 %"
#> [1] "Predicting bootstrapped TPCs 0.4 %"
#> [1] "Predicting bootstrapped TPCs 0.5 %"
#> [1] "Predicting bootstrapped TPCs 0.5 %"
#> [1] "Predicting bootstrapped TPCs 0.6 %"
#> [1] "Predicting bootstrapped TPCs 0.7 %"
#> [1] "Predicting bootstrapped TPCs 0.8 %"
#> [1] "Predicting bootstrapped TPCs 0.8 %"
#> [1] "Predicting bootstrapped TPCs 0.9 %"
#> [1] "Predicting bootstrapped TPCs 1 %"
#> [1] "Predicting bootstrapped TPCs 1.1 %"
#> [1] "Predicting bootstrapped TPCs 1.1 %"
#> [1] "Predicting bootstrapped TPCs 1.2 %"
#> [1] "Predicting bootstrapped TPCs 1.3 %"
#> [1] "Predicting bootstrapped TPCs 1.4 %"
#> [1] "Predicting bootstrapped TPCs 1.4 %"
#> [1] "Predicting bootstrapped TPCs 1.5 %"
#> [1] "Predicting bootstrapped TPCs 1.6 %"
#> [1] "Predicting bootstrapped TPCs 1.7 %"
#> [1] "Predicting bootstrapped TPCs 1.7 %"
#> [1] "Predicting bootstrapped TPCs 1.8 %"
#> [1] "Predicting bootstrapped TPCs 1.9 %"
#> [1] "Predicting bootstrapped TPCs 2 %"
#> [1] "Predicting bootstrapped TPCs 2 %"
#> [1] "Predicting bootstrapped TPCs 2.1 %"
#> [1] "Predicting bootstrapped TPCs 2.2 %"
#> [1] "Predicting bootstrapped TPCs 2.3 %"
#> [1] "Predicting bootstrapped TPCs 2.3 %"
#> [1] "Predicting bootstrapped TPCs 2.4 %"
#> [1] "Predicting bootstrapped TPCs 2.5 %"
#> [1] "Predicting bootstrapped TPCs 2.6 %"
#> [1] "Predicting bootstrapped TPCs 2.6 %"
#> [1] "Predicting bootstrapped TPCs 2.7 %"
#> [1] "Predicting bootstrapped TPCs 2.8 %"
#> [1] "Predicting bootstrapped TPCs 2.9 %"
#> [1] "Predicting bootstrapped TPCs 2.9 %"
#> [1] "Predicting bootstrapped TPCs 3 %"
#> [1] "Predicting bootstrapped TPCs 3.1 %"
#> [1] "Predicting bootstrapped TPCs 3.2 %"
#> [1] "Predicting bootstrapped TPCs 3.3 %"
#> [1] "Predicting bootstrapped TPCs 3.3 %"
#> [1] "Predicting bootstrapped TPCs 3.4 %"
#> [1] "Predicting bootstrapped TPCs 3.5 %"
#> [1] "Predicting bootstrapped TPCs 3.6 %"
#> [1] "Predicting bootstrapped TPCs 3.6 %"
#> [1] "Predicting bootstrapped TPCs 3.7 %"
#> [1] "Predicting bootstrapped TPCs 3.8 %"
#> [1] "Predicting bootstrapped TPCs 3.9 %"
#> [1] "Predicting bootstrapped TPCs 3.9 %"
#> [1] "Predicting bootstrapped TPCs 4 %"
#> [1] "Predicting bootstrapped TPCs 4.1 %"
#> [1] "Predicting bootstrapped TPCs 4.2 %"
#> [1] "Predicting bootstrapped TPCs 4.2 %"
#> [1] "Predicting bootstrapped TPCs 4.3 %"
#> [1] "Predicting bootstrapped TPCs 4.4 %"
#> [1] "Predicting bootstrapped TPCs 4.5 %"
#> [1] "Predicting bootstrapped TPCs 4.5 %"
#> [1] "Predicting bootstrapped TPCs 4.6 %"
#> [1] "Predicting bootstrapped TPCs 4.7 %"
#> [1] "Predicting bootstrapped TPCs 4.8 %"
#> [1] "Predicting bootstrapped TPCs 4.8 %"
#> [1] "Predicting bootstrapped TPCs 4.9 %"
#> [1] "Predicting bootstrapped TPCs 5 %"
#> [1] "Predicting bootstrapped TPCs 5.1 %"
#> [1] "Predicting bootstrapped TPCs 5.1 %"
#> [1] "Predicting bootstrapped TPCs 5.2 %"
#> [1] "Predicting bootstrapped TPCs 5.3 %"
#> [1] "Predicting bootstrapped TPCs 5.4 %"
#> [1] "Predicting bootstrapped TPCs 5.4 %"
#> [1] "Predicting bootstrapped TPCs 5.5 %"
#> [1] "Predicting bootstrapped TPCs 5.6 %"
#> [1] "Predicting bootstrapped TPCs 5.7 %"
#> [1] "Predicting bootstrapped TPCs 5.7 %"
#> [1] "Predicting bootstrapped TPCs 5.8 %"
#> [1] "Predicting bootstrapped TPCs 5.9 %"
#> [1] "Predicting bootstrapped TPCs 6 %"
#> [1] "Predicting bootstrapped TPCs 6 %"
#> [1] "Predicting bootstrapped TPCs 6.1 %"
#> [1] "Predicting bootstrapped TPCs 6.2 %"
#> [1] "Predicting bootstrapped TPCs 6.3 %"
#> [1] "Predicting bootstrapped TPCs 6.3 %"
#> [1] "Predicting bootstrapped TPCs 6.4 %"
#> [1] "Predicting bootstrapped TPCs 6.5 %"
#> [1] "Predicting bootstrapped TPCs 6.6 %"
#> [1] "Predicting bootstrapped TPCs 6.7 %"
#> [1] "Predicting bootstrapped TPCs 6.7 %"
#> [1] "Predicting bootstrapped TPCs 6.8 %"
#> [1] "Predicting bootstrapped TPCs 6.9 %"
#> [1] "Predicting bootstrapped TPCs 7 %"
#> [1] "Predicting bootstrapped TPCs 7 %"
#> [1] "Predicting bootstrapped TPCs 7.1 %"
#> [1] "Predicting bootstrapped TPCs 7.2 %"
#> [1] "Predicting bootstrapped TPCs 7.3 %"
#> [1] "Predicting bootstrapped TPCs 7.3 %"
#> [1] "Predicting bootstrapped TPCs 7.4 %"
#> [1] "Predicting bootstrapped TPCs 7.5 %"
#> [1] "Predicting bootstrapped TPCs 7.6 %"
#> [1] "Predicting bootstrapped TPCs 7.6 %"
#> [1] "Predicting bootstrapped TPCs 7.7 %"
#> [1] "Predicting bootstrapped TPCs 7.8 %"
#> [1] "Predicting bootstrapped TPCs 7.9 %"
#> [1] "Predicting bootstrapped TPCs 7.9 %"
#> [1] "Predicting bootstrapped TPCs 8 %"
#> [1] "Predicting bootstrapped TPCs 8.1 %"
#> [1] "Predicting bootstrapped TPCs 8.2 %"
#> [1] "Predicting bootstrapped TPCs 8.2 %"
#> [1] "Predicting bootstrapped TPCs 8.3 %"
#> [1] "Predicting bootstrapped TPCs 8.4 %"
#> [1] "Predicting bootstrapped TPCs 8.5 %"
#> [1] "Predicting bootstrapped TPCs 8.5 %"
#> [1] "Predicting bootstrapped TPCs 8.6 %"
#> [1] "Predicting bootstrapped TPCs 8.7 %"
#> [1] "Predicting bootstrapped TPCs 8.8 %"
#> [1] "Predicting bootstrapped TPCs 8.8 %"
#> [1] "Predicting bootstrapped TPCs 8.9 %"
#> [1] "Predicting bootstrapped TPCs 9 %"
#> [1] "Predicting bootstrapped TPCs 9.1 %"
#> [1] "Predicting bootstrapped TPCs 9.1 %"
#> [1] "Predicting bootstrapped TPCs 9.2 %"
#> [1] "Predicting bootstrapped TPCs 9.3 %"
#> [1] "Predicting bootstrapped TPCs 9.4 %"
#> [1] "Predicting bootstrapped TPCs 9.4 %"
#> [1] "Predicting bootstrapped TPCs 9.5 %"
#> [1] "Predicting bootstrapped TPCs 9.6 %"
#> [1] "Predicting bootstrapped TPCs 9.7 %"
#> [1] "Predicting bootstrapped TPCs 9.8 %"
#> [1] "Predicting bootstrapped TPCs 9.8 %"
#> [1] "Predicting bootstrapped TPCs 9.9 %"
#> [1] "Predicting bootstrapped TPCs 10 %"
#> [1] "Predicting bootstrapped TPCs 10.1 %"
#> [1] "Predicting bootstrapped TPCs 10.1 %"
#> [1] "Predicting bootstrapped TPCs 10.2 %"
#> [1] "Predicting bootstrapped TPCs 10.3 %"
#> [1] "Predicting bootstrapped TPCs 10.4 %"
#> [1] "Predicting bootstrapped TPCs 10.4 %"
#> [1] "Predicting bootstrapped TPCs 10.5 %"
#> [1] "Predicting bootstrapped TPCs 10.6 %"
#> [1] "Predicting bootstrapped TPCs 10.7 %"
#> [1] "Predicting bootstrapped TPCs 10.7 %"
#> [1] "Predicting bootstrapped TPCs 10.8 %"
#> [1] "Predicting bootstrapped TPCs 10.9 %"
#> [1] "Predicting bootstrapped TPCs 11 %"
#> [1] "Predicting bootstrapped TPCs 11 %"
#> [1] "Predicting bootstrapped TPCs 11.1 %"
#> [1] "Predicting bootstrapped TPCs 11.2 %"
#> [1] "Predicting bootstrapped TPCs 11.3 %"
#> [1] "Predicting bootstrapped TPCs 11.3 %"
#> [1] "Predicting bootstrapped TPCs 11.4 %"
#> [1] "Predicting bootstrapped TPCs 11.5 %"
#> [1] "Predicting bootstrapped TPCs 11.6 %"
#> [1] "Predicting bootstrapped TPCs 11.6 %"
#> [1] "Predicting bootstrapped TPCs 11.7 %"
#> [1] "Predicting bootstrapped TPCs 11.8 %"
#> [1] "Predicting bootstrapped TPCs 11.9 %"
#> [1] "Predicting bootstrapped TPCs 11.9 %"
#> [1] "Predicting bootstrapped TPCs 12 %"
#> [1] "Predicting bootstrapped TPCs 12.1 %"
#> [1] "Predicting bootstrapped TPCs 12.2 %"
#> [1] "Predicting bootstrapped TPCs 12.2 %"
#> [1] "Predicting bootstrapped TPCs 12.3 %"
#> [1] "Predicting bootstrapped TPCs 12.4 %"
#> [1] "Predicting bootstrapped TPCs 12.5 %"
#> [1] "Predicting bootstrapped TPCs 12.5 %"
#> [1] "Predicting bootstrapped TPCs 12.6 %"
#> [1] "Predicting bootstrapped TPCs 12.7 %"
#> [1] "Predicting bootstrapped TPCs 12.8 %"
#> [1] "Predicting bootstrapped TPCs 12.8 %"
#> [1] "Predicting bootstrapped TPCs 12.9 %"
#> [1] "Predicting bootstrapped TPCs 13 %"
#> [1] "Predicting bootstrapped TPCs 13.1 %"
#> [1] "Predicting bootstrapped TPCs 13.2 %"
#> [1] "Predicting bootstrapped TPCs 13.2 %"
#> [1] "Predicting bootstrapped TPCs 13.3 %"
#> [1] "Predicting bootstrapped TPCs 13.4 %"
#> [1] "Predicting bootstrapped TPCs 13.5 %"
#> [1] "Predicting bootstrapped TPCs 13.5 %"
#> [1] "Predicting bootstrapped TPCs 13.6 %"
#> [1] "Predicting bootstrapped TPCs 13.7 %"
#> [1] "Predicting bootstrapped TPCs 13.8 %"
#> [1] "Predicting bootstrapped TPCs 13.8 %"
#> [1] "Predicting bootstrapped TPCs 13.9 %"
#> [1] "Predicting bootstrapped TPCs 14 %"
#> [1] "Predicting bootstrapped TPCs 14.1 %"
#> [1] "Predicting bootstrapped TPCs 14.1 %"
#> [1] "Predicting bootstrapped TPCs 14.2 %"
#> [1] "Predicting bootstrapped TPCs 14.3 %"
#> [1] "Predicting bootstrapped TPCs 14.4 %"
#> [1] "Predicting bootstrapped TPCs 14.4 %"
#> [1] "Predicting bootstrapped TPCs 14.5 %"
#> [1] "Predicting bootstrapped TPCs 14.6 %"
#> [1] "Predicting bootstrapped TPCs 14.7 %"
#> [1] "Predicting bootstrapped TPCs 14.7 %"
#> [1] "Predicting bootstrapped TPCs 14.8 %"
#> [1] "Predicting bootstrapped TPCs 14.9 %"
#> [1] "Predicting bootstrapped TPCs 15 %"
#> [1] "Predicting bootstrapped TPCs 15 %"
#> [1] "Predicting bootstrapped TPCs 15.1 %"
#> [1] "Predicting bootstrapped TPCs 15.2 %"
#> [1] "Predicting bootstrapped TPCs 15.3 %"
#> [1] "Predicting bootstrapped TPCs 15.3 %"
#> [1] "Predicting bootstrapped TPCs 15.4 %"
#> [1] "Predicting bootstrapped TPCs 15.5 %"
#> [1] "Predicting bootstrapped TPCs 15.6 %"
#> [1] "Predicting bootstrapped TPCs 15.6 %"
#> [1] "Predicting bootstrapped TPCs 15.7 %"
#> [1] "Predicting bootstrapped TPCs 15.8 %"
#> [1] "Predicting bootstrapped TPCs 15.9 %"
#> [1] "Predicting bootstrapped TPCs 15.9 %"
#> [1] "Predicting bootstrapped TPCs 16 %"
#> [1] "Predicting bootstrapped TPCs 16.1 %"
#> [1] "Predicting bootstrapped TPCs 16.2 %"
#> [1] "Predicting bootstrapped TPCs 16.3 %"
#> [1] "Predicting bootstrapped TPCs 16.3 %"
#> [1] "Predicting bootstrapped TPCs 16.4 %"
#> [1] "Predicting bootstrapped TPCs 16.5 %"
#> [1] "Predicting bootstrapped TPCs 16.6 %"
#> [1] "Predicting bootstrapped TPCs 16.6 %"
#> [1] "Predicting bootstrapped TPCs 16.7 %"
#> [1] "Predicting bootstrapped TPCs 16.8 %"
#> [1] "Predicting bootstrapped TPCs 16.9 %"
#> [1] "Predicting bootstrapped TPCs 16.9 %"
#> [1] "Predicting bootstrapped TPCs 17 %"
#> [1] "Predicting bootstrapped TPCs 17.1 %"
#> [1] "Predicting bootstrapped TPCs 17.2 %"
#> [1] "Predicting bootstrapped TPCs 17.2 %"
#> [1] "Predicting bootstrapped TPCs 17.3 %"
#> [1] "Predicting bootstrapped TPCs 17.4 %"
#> [1] "Predicting bootstrapped TPCs 17.5 %"
#> [1] "Predicting bootstrapped TPCs 17.5 %"
#> [1] "Predicting bootstrapped TPCs 17.6 %"
#> [1] "Predicting bootstrapped TPCs 17.7 %"
#> [1] "Predicting bootstrapped TPCs 17.8 %"
#> [1] "Predicting bootstrapped TPCs 17.8 %"
#> [1] "Predicting bootstrapped TPCs 17.9 %"
#> [1] "Predicting bootstrapped TPCs 18 %"
#> [1] "Predicting bootstrapped TPCs 18.1 %"
#> [1] "Predicting bootstrapped TPCs 18.1 %"
#> [1] "Predicting bootstrapped TPCs 18.2 %"
#> [1] "Predicting bootstrapped TPCs 18.3 %"
#> [1] "Predicting bootstrapped TPCs 18.4 %"
#> [1] "Predicting bootstrapped TPCs 18.4 %"
#> [1] "Predicting bootstrapped TPCs 18.5 %"
#> [1] "Predicting bootstrapped TPCs 18.6 %"
#> [1] "Predicting bootstrapped TPCs 18.7 %"
#> [1] "Predicting bootstrapped TPCs 18.7 %"
#> [1] "Predicting bootstrapped TPCs 18.8 %"
#> [1] "Predicting bootstrapped TPCs 18.9 %"
#> [1] "Predicting bootstrapped TPCs 19 %"
#> [1] "Predicting bootstrapped TPCs 19 %"
#> [1] "Predicting bootstrapped TPCs 19.1 %"
#> [1] "Predicting bootstrapped TPCs 19.2 %"
#> [1] "Predicting bootstrapped TPCs 19.3 %"
#> [1] "Predicting bootstrapped TPCs 19.3 %"
#> [1] "Predicting bootstrapped TPCs 19.4 %"
#> [1] "Predicting bootstrapped TPCs 19.5 %"
#> [1] "Predicting bootstrapped TPCs 19.6 %"
#> [1] "Predicting bootstrapped TPCs 19.7 %"
#> [1] "Predicting bootstrapped TPCs 19.7 %"
#> [1] "Predicting bootstrapped TPCs 19.8 %"
#> [1] "Predicting bootstrapped TPCs 19.9 %"
#> [1] "Predicting bootstrapped TPCs 20 %"
#> [1] "Predicting bootstrapped TPCs 20 %"
#> [1] "Predicting bootstrapped TPCs 20.1 %"
#> [1] "Predicting bootstrapped TPCs 20.2 %"
#> [1] "Predicting bootstrapped TPCs 20.3 %"
#> [1] "Predicting bootstrapped TPCs 20.3 %"
#> [1] "Predicting bootstrapped TPCs 20.4 %"
#> [1] "Predicting bootstrapped TPCs 20.5 %"
#> [1] "Predicting bootstrapped TPCs 20.6 %"
#> [1] "Predicting bootstrapped TPCs 20.6 %"
#> [1] "Predicting bootstrapped TPCs 20.7 %"
#> [1] "Predicting bootstrapped TPCs 20.8 %"
#> [1] "Predicting bootstrapped TPCs 20.9 %"
#> [1] "Predicting bootstrapped TPCs 20.9 %"
#> [1] "Predicting bootstrapped TPCs 21 %"
#> [1] "Predicting bootstrapped TPCs 21.1 %"
#> [1] "Predicting bootstrapped TPCs 21.2 %"
#> [1] "Predicting bootstrapped TPCs 21.2 %"
#> [1] "Predicting bootstrapped TPCs 21.3 %"
#> [1] "Predicting bootstrapped TPCs 21.4 %"
#> [1] "Predicting bootstrapped TPCs 21.5 %"
#> [1] "Predicting bootstrapped TPCs 21.5 %"
#> [1] "Predicting bootstrapped TPCs 21.6 %"
#> [1] "Predicting bootstrapped TPCs 21.7 %"
#> [1] "Predicting bootstrapped TPCs 21.8 %"
#> [1] "Predicting bootstrapped TPCs 21.8 %"
#> [1] "Predicting bootstrapped TPCs 21.9 %"
#> [1] "Predicting bootstrapped TPCs 22 %"
#> [1] "Predicting bootstrapped TPCs 22.1 %"
#> [1] "Predicting bootstrapped TPCs 22.1 %"
#> [1] "Predicting bootstrapped TPCs 22.2 %"
#> [1] "Predicting bootstrapped TPCs 22.3 %"
#> [1] "Predicting bootstrapped TPCs 22.4 %"
#> [1] "Predicting bootstrapped TPCs 22.4 %"
#> [1] "Predicting bootstrapped TPCs 22.5 %"
#> [1] "Predicting bootstrapped TPCs 22.6 %"
#> [1] "Predicting bootstrapped TPCs 22.7 %"
#> [1] "Predicting bootstrapped TPCs 22.8 %"
#> [1] "Predicting bootstrapped TPCs 22.8 %"
#> [1] "Predicting bootstrapped TPCs 22.9 %"
#> [1] "Predicting bootstrapped TPCs 23 %"
#> [1] "Predicting bootstrapped TPCs 23.1 %"
#> [1] "Predicting bootstrapped TPCs 23.1 %"
#> [1] "Predicting bootstrapped TPCs 23.2 %"
#> [1] "Predicting bootstrapped TPCs 23.3 %"
#> [1] "Predicting bootstrapped TPCs 23.4 %"
#> [1] "Predicting bootstrapped TPCs 23.4 %"
#> [1] "Predicting bootstrapped TPCs 23.5 %"
#> [1] "Predicting bootstrapped TPCs 23.6 %"
#> [1] "Predicting bootstrapped TPCs 23.7 %"
#> [1] "Predicting bootstrapped TPCs 23.7 %"
#> [1] "Predicting bootstrapped TPCs 23.8 %"
#> [1] "Predicting bootstrapped TPCs 23.9 %"
#> [1] "Predicting bootstrapped TPCs 24 %"
#> [1] "Predicting bootstrapped TPCs 24 %"
#> [1] "Predicting bootstrapped TPCs 24.1 %"
#> [1] "Predicting bootstrapped TPCs 24.2 %"
#> [1] "Predicting bootstrapped TPCs 24.3 %"
#> [1] "Predicting bootstrapped TPCs 24.3 %"
#> [1] "Predicting bootstrapped TPCs 24.4 %"
#> [1] "Predicting bootstrapped TPCs 24.5 %"
#> [1] "Predicting bootstrapped TPCs 24.6 %"
#> [1] "Predicting bootstrapped TPCs 24.6 %"
#> [1] "Predicting bootstrapped TPCs 24.7 %"
#> [1] "Predicting bootstrapped TPCs 24.8 %"
#> [1] "Predicting bootstrapped TPCs 24.9 %"
#> [1] "Predicting bootstrapped TPCs 24.9 %"
#> [1] "Predicting bootstrapped TPCs 25 %"
#> [1] "Predicting bootstrapped TPCs 25.1 %"
#> [1] "Predicting bootstrapped TPCs 25.2 %"
#> [1] "Predicting bootstrapped TPCs 25.2 %"
#> [1] "Predicting bootstrapped TPCs 25.3 %"
#> [1] "Predicting bootstrapped TPCs 25.4 %"
#> [1] "Predicting bootstrapped TPCs 25.5 %"
#> [1] "Predicting bootstrapped TPCs 25.5 %"
#> [1] "Predicting bootstrapped TPCs 25.6 %"
#> [1] "Predicting bootstrapped TPCs 25.7 %"
#> [1] "Predicting bootstrapped TPCs 25.8 %"
#> [1] "Predicting bootstrapped TPCs 25.9 %"
#> [1] "Predicting bootstrapped TPCs 25.9 %"
#> [1] "Predicting bootstrapped TPCs 26 %"
#> [1] "Predicting bootstrapped TPCs 26.1 %"
#> [1] "Predicting bootstrapped TPCs 26.2 %"
#> [1] "Predicting bootstrapped TPCs 26.2 %"
#> [1] "Predicting bootstrapped TPCs 26.3 %"
#> [1] "Predicting bootstrapped TPCs 26.4 %"
#> [1] "Predicting bootstrapped TPCs 26.5 %"
#> [1] "Predicting bootstrapped TPCs 26.5 %"
#> [1] "Predicting bootstrapped TPCs 26.6 %"
#> [1] "Predicting bootstrapped TPCs 26.7 %"
#> [1] "Predicting bootstrapped TPCs 26.8 %"
#> [1] "Predicting bootstrapped TPCs 26.8 %"
#> [1] "Predicting bootstrapped TPCs 26.9 %"
#> [1] "Predicting bootstrapped TPCs 27 %"
#> [1] "Predicting bootstrapped TPCs 27.1 %"
#> [1] "Predicting bootstrapped TPCs 27.1 %"
#> [1] "Predicting bootstrapped TPCs 27.2 %"
#> [1] "Predicting bootstrapped TPCs 27.3 %"
#> [1] "Predicting bootstrapped TPCs 27.4 %"
#> [1] "Predicting bootstrapped TPCs 27.4 %"
#> [1] "Predicting bootstrapped TPCs 27.5 %"
#> [1] "Predicting bootstrapped TPCs 27.6 %"
#> [1] "Predicting bootstrapped TPCs 27.7 %"
#> [1] "Predicting bootstrapped TPCs 27.7 %"
#> [1] "Predicting bootstrapped TPCs 27.8 %"
#> [1] "Predicting bootstrapped TPCs 27.9 %"
#> [1] "Predicting bootstrapped TPCs 28 %"
#> [1] "Predicting bootstrapped TPCs 28 %"
#> [1] "Predicting bootstrapped TPCs 28.1 %"
#> [1] "Predicting bootstrapped TPCs 28.2 %"
#> [1] "Predicting bootstrapped TPCs 28.3 %"
#> [1] "Predicting bootstrapped TPCs 28.3 %"
#> [1] "Predicting bootstrapped TPCs 28.4 %"
#> [1] "Predicting bootstrapped TPCs 28.5 %"
#> [1] "Predicting bootstrapped TPCs 28.6 %"
#> [1] "Predicting bootstrapped TPCs 28.6 %"
#> [1] "Predicting bootstrapped TPCs 28.7 %"
#> [1] "Predicting bootstrapped TPCs 28.8 %"
#> [1] "Predicting bootstrapped TPCs 28.9 %"
#> [1] "Predicting bootstrapped TPCs 28.9 %"
#> [1] "Predicting bootstrapped TPCs 29 %"
#> [1] "Predicting bootstrapped TPCs 29.1 %"
#> [1] "Predicting bootstrapped TPCs 29.2 %"
#> [1] "Predicting bootstrapped TPCs 29.3 %"
#> [1] "Predicting bootstrapped TPCs 29.3 %"
#> [1] "Predicting bootstrapped TPCs 29.4 %"
#> [1] "Predicting bootstrapped TPCs 29.5 %"
#> [1] "Predicting bootstrapped TPCs 29.6 %"
#> [1] "Predicting bootstrapped TPCs 29.6 %"
#> [1] "Predicting bootstrapped TPCs 29.7 %"
#> [1] "Predicting bootstrapped TPCs 29.8 %"
#> [1] "Predicting bootstrapped TPCs 29.9 %"
#> [1] "Predicting bootstrapped TPCs 29.9 %"
#> [1] "Predicting bootstrapped TPCs 30 %"
#> [1] "Predicting bootstrapped TPCs 30.1 %"
#> [1] "Predicting bootstrapped TPCs 30.2 %"
#> [1] "Predicting bootstrapped TPCs 30.2 %"
#> [1] "Predicting bootstrapped TPCs 30.3 %"
#> [1] "Predicting bootstrapped TPCs 30.4 %"
#> [1] "Predicting bootstrapped TPCs 30.5 %"
#> [1] "Predicting bootstrapped TPCs 30.5 %"
#> [1] "Predicting bootstrapped TPCs 30.6 %"
#> [1] "Predicting bootstrapped TPCs 30.7 %"
#> [1] "Predicting bootstrapped TPCs 30.8 %"
#> [1] "Predicting bootstrapped TPCs 30.8 %"
#> [1] "Predicting bootstrapped TPCs 30.9 %"
#> [1] "Predicting bootstrapped TPCs 31 %"
#> [1] "Predicting bootstrapped TPCs 31.1 %"
#> [1] "Predicting bootstrapped TPCs 31.1 %"
#> [1] "Predicting bootstrapped TPCs 31.2 %"
#> [1] "Predicting bootstrapped TPCs 31.3 %"
#> [1] "Predicting bootstrapped TPCs 31.4 %"
#> [1] "Predicting bootstrapped TPCs 31.4 %"
#> [1] "Predicting bootstrapped TPCs 31.5 %"
#> [1] "Predicting bootstrapped TPCs 31.6 %"
#> [1] "Predicting bootstrapped TPCs 31.7 %"
#> [1] "Predicting bootstrapped TPCs 31.7 %"
#> [1] "Predicting bootstrapped TPCs 31.8 %"
#> [1] "Predicting bootstrapped TPCs 31.9 %"
#> [1] "Predicting bootstrapped TPCs 32 %"
#> [1] "Predicting bootstrapped TPCs 32 %"
#> [1] "Predicting bootstrapped TPCs 32.1 %"
#> [1] "Predicting bootstrapped TPCs 32.2 %"
#> [1] "Predicting bootstrapped TPCs 32.3 %"
#> [1] "Predicting bootstrapped TPCs 32.4 %"
#> [1] "Predicting bootstrapped TPCs 32.4 %"
#> [1] "Predicting bootstrapped TPCs 32.5 %"
#> [1] "Predicting bootstrapped TPCs 32.6 %"
#> [1] "Predicting bootstrapped TPCs 32.7 %"
#> [1] "Predicting bootstrapped TPCs 32.7 %"
#> [1] "Predicting bootstrapped TPCs 32.8 %"
#> [1] "Predicting bootstrapped TPCs 32.9 %"
#> [1] "Predicting bootstrapped TPCs 33 %"
#> [1] "Predicting bootstrapped TPCs 33 %"
#> [1] "Predicting bootstrapped TPCs 33.1 %"
#> [1] "Predicting bootstrapped TPCs 33.2 %"
#> [1] "Predicting bootstrapped TPCs 33.3 %"
#> [1] "Predicting bootstrapped TPCs 33.3 %"
#> [1] "Predicting bootstrapped TPCs 33.4 %"
#> [1] "Predicting bootstrapped TPCs 33.5 %"
#> [1] "Predicting bootstrapped TPCs 33.6 %"
#> [1] "Predicting bootstrapped TPCs 33.6 %"
#> [1] "Predicting bootstrapped TPCs 33.7 %"
#> [1] "Predicting bootstrapped TPCs 33.8 %"
#> [1] "Predicting bootstrapped TPCs 33.9 %"
#> [1] "Predicting bootstrapped TPCs 33.9 %"
#> [1] "Predicting bootstrapped TPCs 34 %"
#> [1] "Predicting bootstrapped TPCs 34.1 %"
#> [1] "Predicting bootstrapped TPCs 34.2 %"
#> [1] "Predicting bootstrapped TPCs 34.2 %"
#> [1] "Predicting bootstrapped TPCs 34.3 %"
#> [1] "Predicting bootstrapped TPCs 34.4 %"
#> [1] "Predicting bootstrapped TPCs 34.5 %"
#> [1] "Predicting bootstrapped TPCs 34.5 %"
#> [1] "Predicting bootstrapped TPCs 34.6 %"
#> [1] "Predicting bootstrapped TPCs 34.7 %"
#> [1] "Predicting bootstrapped TPCs 34.8 %"
#> [1] "Predicting bootstrapped TPCs 34.8 %"
#> [1] "Predicting bootstrapped TPCs 34.9 %"
#> [1] "Predicting bootstrapped TPCs 35 %"
#> [1] "Predicting bootstrapped TPCs 35.1 %"
#> [1] "Predicting bootstrapped TPCs 35.1 %"
#> [1] "Predicting bootstrapped TPCs 35.2 %"
#> [1] "Predicting bootstrapped TPCs 35.3 %"
#> [1] "Predicting bootstrapped TPCs 35.4 %"
#> [1] "Predicting bootstrapped TPCs 35.4 %"
#> [1] "Predicting bootstrapped TPCs 35.5 %"
#> [1] "Predicting bootstrapped TPCs 35.6 %"
#> [1] "Predicting bootstrapped TPCs 35.7 %"
#> [1] "Predicting bootstrapped TPCs 35.8 %"
#> [1] "Predicting bootstrapped TPCs 35.8 %"
#> [1] "Predicting bootstrapped TPCs 35.9 %"
#> [1] "Predicting bootstrapped TPCs 36 %"
#> [1] "Predicting bootstrapped TPCs 36.1 %"
#> [1] "Predicting bootstrapped TPCs 36.1 %"
#> [1] "Predicting bootstrapped TPCs 36.2 %"
#> [1] "Predicting bootstrapped TPCs 36.3 %"
#> [1] "Predicting bootstrapped TPCs 36.4 %"
#> [1] "Predicting bootstrapped TPCs 36.4 %"
#> [1] "Predicting bootstrapped TPCs 36.5 %"
#> [1] "Predicting bootstrapped TPCs 36.6 %"
#> [1] "Predicting bootstrapped TPCs 36.7 %"
#> [1] "Predicting bootstrapped TPCs 36.7 %"
#> [1] "Predicting bootstrapped TPCs 36.8 %"
#> [1] "Predicting bootstrapped TPCs 36.9 %"
#> [1] "Predicting bootstrapped TPCs 37 %"
#> [1] "Predicting bootstrapped TPCs 37 %"
#> [1] "Predicting bootstrapped TPCs 37.1 %"
#> [1] "Predicting bootstrapped TPCs 37.2 %"
#> [1] "Predicting bootstrapped TPCs 37.3 %"
#> [1] "Predicting bootstrapped TPCs 37.3 %"
#> [1] "Predicting bootstrapped TPCs 37.4 %"
#> [1] "Predicting bootstrapped TPCs 37.5 %"
#> [1] "Predicting bootstrapped TPCs 37.6 %"
#> [1] "Predicting bootstrapped TPCs 37.6 %"
#> [1] "Predicting bootstrapped TPCs 37.7 %"
#> [1] "Predicting bootstrapped TPCs 37.8 %"
#> [1] "Predicting bootstrapped TPCs 37.9 %"
#> [1] "Predicting bootstrapped TPCs 37.9 %"
#> [1] "Predicting bootstrapped TPCs 38 %"
#> [1] "Predicting bootstrapped TPCs 38.1 %"
#> [1] "Predicting bootstrapped TPCs 38.2 %"
#> [1] "Predicting bootstrapped TPCs 38.2 %"
#> [1] "Predicting bootstrapped TPCs 38.3 %"
#> [1] "Predicting bootstrapped TPCs 38.4 %"
#> [1] "Predicting bootstrapped TPCs 38.5 %"
#> [1] "Predicting bootstrapped TPCs 38.5 %"
#> [1] "Predicting bootstrapped TPCs 38.6 %"
#> [1] "Predicting bootstrapped TPCs 38.7 %"
#> [1] "Predicting bootstrapped TPCs 38.8 %"
#> [1] "Predicting bootstrapped TPCs 38.9 %"
#> [1] "Predicting bootstrapped TPCs 38.9 %"
#> [1] "Predicting bootstrapped TPCs 39 %"
#> [1] "Predicting bootstrapped TPCs 39.1 %"
#> [1] "Predicting bootstrapped TPCs 39.2 %"
#> [1] "Predicting bootstrapped TPCs 39.2 %"
#> [1] "Predicting bootstrapped TPCs 39.3 %"
#> [1] "Predicting bootstrapped TPCs 39.4 %"
#> [1] "Predicting bootstrapped TPCs 39.5 %"
#> [1] "Predicting bootstrapped TPCs 39.5 %"
#> [1] "Predicting bootstrapped TPCs 39.6 %"
#> [1] "Predicting bootstrapped TPCs 39.7 %"
#> [1] "Predicting bootstrapped TPCs 39.8 %"
#> [1] "Predicting bootstrapped TPCs 39.8 %"
#> [1] "Predicting bootstrapped TPCs 39.9 %"
#> [1] "Predicting bootstrapped TPCs 40 %"
#> [1] "Predicting bootstrapped TPCs 40.1 %"
#> [1] "Predicting bootstrapped TPCs 40.1 %"
#> [1] "Predicting bootstrapped TPCs 40.2 %"
#> [1] "Predicting bootstrapped TPCs 40.3 %"
#> [1] "Predicting bootstrapped TPCs 40.4 %"
#> [1] "Predicting bootstrapped TPCs 40.4 %"
#> [1] "Predicting bootstrapped TPCs 40.5 %"
#> [1] "Predicting bootstrapped TPCs 40.6 %"
#> [1] "Predicting bootstrapped TPCs 40.7 %"
#> [1] "Predicting bootstrapped TPCs 40.7 %"
#> [1] "Predicting bootstrapped TPCs 40.8 %"
#> [1] "Predicting bootstrapped TPCs 40.9 %"
#> [1] "Predicting bootstrapped TPCs 41 %"
#> [1] "Predicting bootstrapped TPCs 41 %"
#> [1] "Predicting bootstrapped TPCs 41.1 %"
#> [1] "Predicting bootstrapped TPCs 41.2 %"
#> [1] "Predicting bootstrapped TPCs 41.3 %"
#> [1] "Predicting bootstrapped TPCs 41.3 %"
#> [1] "Predicting bootstrapped TPCs 41.4 %"
#> [1] "Predicting bootstrapped TPCs 41.5 %"
#> [1] "Predicting bootstrapped TPCs 41.6 %"
#> [1] "Predicting bootstrapped TPCs 41.6 %"
#> [1] "Predicting bootstrapped TPCs 41.7 %"
#> [1] "Predicting bootstrapped TPCs 41.8 %"
#> [1] "Predicting bootstrapped TPCs 41.9 %"
#> [1] "Predicting bootstrapped TPCs 42 %"
#> [1] "Predicting bootstrapped TPCs 42 %"
#> [1] "Predicting bootstrapped TPCs 42.1 %"
#> [1] "Predicting bootstrapped TPCs 42.2 %"
#> [1] "Predicting bootstrapped TPCs 42.3 %"
#> [1] "Predicting bootstrapped TPCs 42.3 %"
#> [1] "Predicting bootstrapped TPCs 42.4 %"
#> [1] "Predicting bootstrapped TPCs 42.5 %"
#> [1] "Predicting bootstrapped TPCs 42.6 %"
#> [1] "Predicting bootstrapped TPCs 42.6 %"
#> [1] "Predicting bootstrapped TPCs 42.7 %"
#> [1] "Predicting bootstrapped TPCs 42.8 %"
#> [1] "Predicting bootstrapped TPCs 42.9 %"
#> [1] "Predicting bootstrapped TPCs 42.9 %"
#> [1] "Predicting bootstrapped TPCs 43 %"
#> [1] "Predicting bootstrapped TPCs 43.1 %"
#> [1] "Predicting bootstrapped TPCs 43.2 %"
#> [1] "Predicting bootstrapped TPCs 43.2 %"
#> [1] "Predicting bootstrapped TPCs 43.3 %"
#> [1] "Predicting bootstrapped TPCs 43.4 %"
#> [1] "Predicting bootstrapped TPCs 43.5 %"
#> [1] "Predicting bootstrapped TPCs 43.5 %"
#> [1] "Predicting bootstrapped TPCs 43.6 %"
#> [1] "Predicting bootstrapped TPCs 43.7 %"
#> [1] "Predicting bootstrapped TPCs 43.8 %"
#> [1] "Predicting bootstrapped TPCs 43.8 %"
#> [1] "Predicting bootstrapped TPCs 43.9 %"
#> [1] "Predicting bootstrapped TPCs 44 %"
#> [1] "Predicting bootstrapped TPCs 44.1 %"
#> [1] "Predicting bootstrapped TPCs 44.1 %"
#> [1] "Predicting bootstrapped TPCs 44.2 %"
#> [1] "Predicting bootstrapped TPCs 44.3 %"
#> [1] "Predicting bootstrapped TPCs 44.4 %"
#> [1] "Predicting bootstrapped TPCs 44.4 %"
#> [1] "Predicting bootstrapped TPCs 44.5 %"
#> [1] "Predicting bootstrapped TPCs 44.6 %"
#> [1] "Predicting bootstrapped TPCs 44.7 %"
#> [1] "Predicting bootstrapped TPCs 44.7 %"
#> [1] "Predicting bootstrapped TPCs 44.8 %"
#> [1] "Predicting bootstrapped TPCs 44.9 %"
#> [1] "Predicting bootstrapped TPCs 45 %"
#> [1] "Predicting bootstrapped TPCs 45 %"
#> [1] "Predicting bootstrapped TPCs 45.1 %"
#> [1] "Predicting bootstrapped TPCs 45.2 %"
#> [1] "Predicting bootstrapped TPCs 45.3 %"
#> [1] "Predicting bootstrapped TPCs 45.4 %"
#> [1] "Predicting bootstrapped TPCs 45.4 %"
#> [1] "Predicting bootstrapped TPCs 45.5 %"
#> [1] "Predicting bootstrapped TPCs 45.6 %"
#> [1] "Predicting bootstrapped TPCs 45.7 %"
#> [1] "Predicting bootstrapped TPCs 45.7 %"
#> [1] "Predicting bootstrapped TPCs 45.8 %"
#> [1] "Predicting bootstrapped TPCs 45.9 %"
#> [1] "Predicting bootstrapped TPCs 46 %"
#> [1] "Predicting bootstrapped TPCs 46 %"
#> [1] "Predicting bootstrapped TPCs 46.1 %"
#> [1] "Predicting bootstrapped TPCs 46.2 %"
#> [1] "Predicting bootstrapped TPCs 46.3 %"
#> [1] "Predicting bootstrapped TPCs 46.3 %"
#> [1] "Predicting bootstrapped TPCs 46.4 %"
#> [1] "Predicting bootstrapped TPCs 46.5 %"
#> [1] "Predicting bootstrapped TPCs 46.6 %"
#> [1] "Predicting bootstrapped TPCs 46.6 %"
#> [1] "Predicting bootstrapped TPCs 46.7 %"
#> [1] "Predicting bootstrapped TPCs 46.8 %"
#> [1] "Predicting bootstrapped TPCs 46.9 %"
#> [1] "Predicting bootstrapped TPCs 46.9 %"
#> [1] "Predicting bootstrapped TPCs 47 %"
#> [1] "Predicting bootstrapped TPCs 47.1 %"
#> [1] "Predicting bootstrapped TPCs 47.2 %"
#> [1] "Predicting bootstrapped TPCs 47.2 %"
#> [1] "Predicting bootstrapped TPCs 47.3 %"
#> [1] "Predicting bootstrapped TPCs 47.4 %"
#> [1] "Predicting bootstrapped TPCs 47.5 %"
#> [1] "Predicting bootstrapped TPCs 47.5 %"
#> [1] "Predicting bootstrapped TPCs 47.6 %"
#> [1] "Predicting bootstrapped TPCs 47.7 %"
#> [1] "Predicting bootstrapped TPCs 47.8 %"
#> [1] "Predicting bootstrapped TPCs 47.8 %"
#> [1] "Predicting bootstrapped TPCs 47.9 %"
#> [1] "Predicting bootstrapped TPCs 48 %"
#> [1] "Predicting bootstrapped TPCs 48.1 %"
#> [1] "Predicting bootstrapped TPCs 48.1 %"
#> [1] "Predicting bootstrapped TPCs 48.2 %"
#> [1] "Predicting bootstrapped TPCs 48.3 %"
#> [1] "Predicting bootstrapped TPCs 48.4 %"
#> [1] "Predicting bootstrapped TPCs 48.5 %"
#> [1] "Predicting bootstrapped TPCs 48.5 %"
#> [1] "Predicting bootstrapped TPCs 48.6 %"
#> [1] "Predicting bootstrapped TPCs 48.7 %"
#> [1] "Predicting bootstrapped TPCs 48.8 %"
#> [1] "Predicting bootstrapped TPCs 48.8 %"
#> [1] "Predicting bootstrapped TPCs 48.9 %"
#> [1] "Predicting bootstrapped TPCs 49 %"
#> [1] "Predicting bootstrapped TPCs 49.1 %"
#> [1] "Predicting bootstrapped TPCs 49.1 %"
#> [1] "Predicting bootstrapped TPCs 49.2 %"
#> [1] "Predicting bootstrapped TPCs 49.3 %"
#> [1] "Predicting bootstrapped TPCs 49.4 %"
#> [1] "Predicting bootstrapped TPCs 49.4 %"
#> [1] "Predicting bootstrapped TPCs 49.5 %"
#> [1] "Predicting bootstrapped TPCs 49.6 %"
#> [1] "Predicting bootstrapped TPCs 49.7 %"
#> [1] "Predicting bootstrapped TPCs 49.7 %"
#> [1] "Predicting bootstrapped TPCs 49.8 %"
#> [1] "Predicting bootstrapped TPCs 49.9 %"
#> [1] "Predicting bootstrapped TPCs 50 %"
#> [1] "Predicting bootstrapped TPCs 50 %"
#> [1] "Predicting bootstrapped TPCs 50.1 %"
#> [1] "Predicting bootstrapped TPCs 50.2 %"
#> [1] "Predicting bootstrapped TPCs 50.3 %"
#> [1] "Predicting bootstrapped TPCs 50.3 %"
#> [1] "Predicting bootstrapped TPCs 50.4 %"
#> [1] "Predicting bootstrapped TPCs 50.5 %"
#> [1] "Predicting bootstrapped TPCs 50.6 %"
#> [1] "Predicting bootstrapped TPCs 50.6 %"
#> [1] "Predicting bootstrapped TPCs 50.7 %"
#> [1] "Predicting bootstrapped TPCs 50.8 %"
#> [1] "Predicting bootstrapped TPCs 50.9 %"
#> [1] "Predicting bootstrapped TPCs 50.9 %"
#> [1] "Predicting bootstrapped TPCs 51 %"
#> [1] "Predicting bootstrapped TPCs 51.1 %"
#> [1] "Predicting bootstrapped TPCs 51.2 %"
#> [1] "Predicting bootstrapped TPCs 51.2 %"
#> [1] "Predicting bootstrapped TPCs 51.3 %"
#> [1] "Predicting bootstrapped TPCs 51.4 %"
#> [1] "Predicting bootstrapped TPCs 51.5 %"
#> [1] "Predicting bootstrapped TPCs 51.5 %"
#> [1] "Predicting bootstrapped TPCs 51.6 %"
#> [1] "Predicting bootstrapped TPCs 51.7 %"
#> [1] "Predicting bootstrapped TPCs 51.8 %"
#> [1] "Predicting bootstrapped TPCs 51.9 %"
#> [1] "Predicting bootstrapped TPCs 51.9 %"
#> [1] "Predicting bootstrapped TPCs 52 %"
#> [1] "Predicting bootstrapped TPCs 52.1 %"
#> [1] "Predicting bootstrapped TPCs 52.2 %"
#> [1] "Predicting bootstrapped TPCs 52.2 %"
#> [1] "Predicting bootstrapped TPCs 52.3 %"
#> [1] "Predicting bootstrapped TPCs 52.4 %"
#> [1] "Predicting bootstrapped TPCs 52.5 %"
#> [1] "Predicting bootstrapped TPCs 52.5 %"
#> [1] "Predicting bootstrapped TPCs 52.6 %"
#> [1] "Predicting bootstrapped TPCs 52.7 %"
#> [1] "Predicting bootstrapped TPCs 52.8 %"
#> [1] "Predicting bootstrapped TPCs 52.8 %"
#> [1] "Predicting bootstrapped TPCs 52.9 %"
#> [1] "Predicting bootstrapped TPCs 53 %"
#> [1] "Predicting bootstrapped TPCs 53.1 %"
#> [1] "Predicting bootstrapped TPCs 53.1 %"
#> [1] "Predicting bootstrapped TPCs 53.2 %"
#> [1] "Predicting bootstrapped TPCs 53.3 %"
#> [1] "Predicting bootstrapped TPCs 53.4 %"
#> [1] "Predicting bootstrapped TPCs 53.4 %"
#> [1] "Predicting bootstrapped TPCs 53.5 %"
#> [1] "Predicting bootstrapped TPCs 53.6 %"
#> [1] "Predicting bootstrapped TPCs 53.7 %"
#> [1] "Predicting bootstrapped TPCs 53.7 %"
#> [1] "Predicting bootstrapped TPCs 53.8 %"
#> [1] "Predicting bootstrapped TPCs 53.9 %"
#> [1] "Predicting bootstrapped TPCs 54 %"
#> [1] "Predicting bootstrapped TPCs 54 %"
#> [1] "Predicting bootstrapped TPCs 54.1 %"
#> [1] "Predicting bootstrapped TPCs 54.2 %"
#> [1] "Predicting bootstrapped TPCs 54.3 %"
#> [1] "Predicting bootstrapped TPCs 54.3 %"
#> [1] "Predicting bootstrapped TPCs 54.4 %"
#> [1] "Predicting bootstrapped TPCs 54.5 %"
#> [1] "Predicting bootstrapped TPCs 54.6 %"
#> [1] "Predicting bootstrapped TPCs 54.6 %"
#> [1] "Predicting bootstrapped TPCs 54.7 %"
#> [1] "Predicting bootstrapped TPCs 54.8 %"
#> [1] "Predicting bootstrapped TPCs 54.9 %"
#> [1] "Predicting bootstrapped TPCs 55 %"
#> [1] "Predicting bootstrapped TPCs 55 %"
#> [1] "Predicting bootstrapped TPCs 55.1 %"
#> [1] "Predicting bootstrapped TPCs 55.2 %"
#> [1] "Predicting bootstrapped TPCs 55.3 %"
#> [1] "Predicting bootstrapped TPCs 55.3 %"
#> [1] "Predicting bootstrapped TPCs 55.4 %"
#> [1] "Predicting bootstrapped TPCs 55.5 %"
#> [1] "Predicting bootstrapped TPCs 55.6 %"
#> [1] "Predicting bootstrapped TPCs 55.6 %"
#> [1] "Predicting bootstrapped TPCs 55.7 %"
#> [1] "Predicting bootstrapped TPCs 55.8 %"
#> [1] "Predicting bootstrapped TPCs 55.9 %"
#> [1] "Predicting bootstrapped TPCs 55.9 %"
#> [1] "Predicting bootstrapped TPCs 56 %"
#> [1] "Predicting bootstrapped TPCs 56.1 %"
#> [1] "Predicting bootstrapped TPCs 56.2 %"
#> [1] "Predicting bootstrapped TPCs 56.2 %"
#> [1] "Predicting bootstrapped TPCs 56.3 %"
#> [1] "Predicting bootstrapped TPCs 56.4 %"
#> [1] "Predicting bootstrapped TPCs 56.5 %"
#> [1] "Predicting bootstrapped TPCs 56.5 %"
#> [1] "Predicting bootstrapped TPCs 56.6 %"
#> [1] "Predicting bootstrapped TPCs 56.7 %"
#> [1] "Predicting bootstrapped TPCs 56.8 %"
#> [1] "Predicting bootstrapped TPCs 56.8 %"
#> [1] "Predicting bootstrapped TPCs 56.9 %"
#> [1] "Predicting bootstrapped TPCs 57 %"
#> [1] "Predicting bootstrapped TPCs 57.1 %"
#> [1] "Predicting bootstrapped TPCs 57.1 %"
#> [1] "Predicting bootstrapped TPCs 57.2 %"
#> [1] "Predicting bootstrapped TPCs 57.3 %"
#> [1] "Predicting bootstrapped TPCs 57.4 %"
#> [1] "Predicting bootstrapped TPCs 57.4 %"
#> [1] "Predicting bootstrapped TPCs 57.5 %"
#> [1] "Predicting bootstrapped TPCs 57.6 %"
#> [1] "Predicting bootstrapped TPCs 57.7 %"
#> [1] "Predicting bootstrapped TPCs 57.7 %"
#> [1] "Predicting bootstrapped TPCs 57.8 %"
#> [1] "Predicting bootstrapped TPCs 57.9 %"
#> [1] "Predicting bootstrapped TPCs 58 %"
#> [1] "Predicting bootstrapped TPCs 58 %"
#> [1] "Predicting bootstrapped TPCs 58.1 %"
#> [1] "Predicting bootstrapped TPCs 58.2 %"
#> [1] "Predicting bootstrapped TPCs 58.3 %"
#> [1] "Predicting bootstrapped TPCs 58.4 %"
#> [1] "Predicting bootstrapped TPCs 58.4 %"
#> [1] "Predicting bootstrapped TPCs 58.5 %"
#> [1] "Predicting bootstrapped TPCs 58.6 %"
#> [1] "Predicting bootstrapped TPCs 58.7 %"
#> [1] "Predicting bootstrapped TPCs 58.7 %"
#> [1] "Predicting bootstrapped TPCs 58.8 %"
#> [1] "Predicting bootstrapped TPCs 58.9 %"
#> [1] "Predicting bootstrapped TPCs 59 %"
#> [1] "Predicting bootstrapped TPCs 59 %"
#> [1] "Predicting bootstrapped TPCs 59.1 %"
#> [1] "Predicting bootstrapped TPCs 59.2 %"
#> [1] "Predicting bootstrapped TPCs 59.3 %"
#> [1] "Predicting bootstrapped TPCs 59.3 %"
#> [1] "Predicting bootstrapped TPCs 59.4 %"
#> [1] "Predicting bootstrapped TPCs 59.5 %"
#> [1] "Predicting bootstrapped TPCs 59.6 %"
#> [1] "Predicting bootstrapped TPCs 59.6 %"
#> [1] "Predicting bootstrapped TPCs 59.7 %"
#> [1] "Predicting bootstrapped TPCs 59.8 %"
#> [1] "Predicting bootstrapped TPCs 59.9 %"
#> [1] "Predicting bootstrapped TPCs 59.9 %"
#> [1] "Predicting bootstrapped TPCs 60 %"
#> [1] "Predicting bootstrapped TPCs 60.1 %"
#> [1] "Predicting bootstrapped TPCs 60.2 %"
#> [1] "Predicting bootstrapped TPCs 60.2 %"
#> [1] "Predicting bootstrapped TPCs 60.3 %"
#> [1] "Predicting bootstrapped TPCs 60.4 %"
#> [1] "Predicting bootstrapped TPCs 60.5 %"
#> [1] "Predicting bootstrapped TPCs 60.5 %"
#> [1] "Predicting bootstrapped TPCs 60.6 %"
#> [1] "Predicting bootstrapped TPCs 60.7 %"
#> [1] "Predicting bootstrapped TPCs 60.8 %"
#> [1] "Predicting bootstrapped TPCs 60.8 %"
#> [1] "Predicting bootstrapped TPCs 60.9 %"
#> [1] "Predicting bootstrapped TPCs 61 %"
#> [1] "Predicting bootstrapped TPCs 61.1 %"
#> [1] "Predicting bootstrapped TPCs 61.1 %"
#> [1] "Predicting bootstrapped TPCs 61.2 %"
#> [1] "Predicting bootstrapped TPCs 61.3 %"
#> [1] "Predicting bootstrapped TPCs 61.4 %"
#> [1] "Predicting bootstrapped TPCs 61.5 %"
#> [1] "Predicting bootstrapped TPCs 61.5 %"
#> [1] "Predicting bootstrapped TPCs 61.6 %"
#> [1] "Predicting bootstrapped TPCs 61.7 %"
#> [1] "Predicting bootstrapped TPCs 61.8 %"
#> [1] "Predicting bootstrapped TPCs 61.8 %"
#> [1] "Predicting bootstrapped TPCs 61.9 %"
#> [1] "Predicting bootstrapped TPCs 62 %"
#> [1] "Predicting bootstrapped TPCs 62.1 %"
#> [1] "Predicting bootstrapped TPCs 62.1 %"
#> [1] "Predicting bootstrapped TPCs 62.2 %"
#> [1] "Predicting bootstrapped TPCs 62.3 %"
#> [1] "Predicting bootstrapped TPCs 62.4 %"
#> [1] "Predicting bootstrapped TPCs 62.4 %"
#> [1] "Predicting bootstrapped TPCs 62.5 %"
#> [1] "Predicting bootstrapped TPCs 62.6 %"
#> [1] "Predicting bootstrapped TPCs 62.7 %"
#> [1] "Predicting bootstrapped TPCs 62.7 %"
#> [1] "Predicting bootstrapped TPCs 62.8 %"
#> [1] "Predicting bootstrapped TPCs 62.9 %"
#> [1] "Predicting bootstrapped TPCs 63 %"
#> [1] "Predicting bootstrapped TPCs 63 %"
#> [1] "Predicting bootstrapped TPCs 63.1 %"
#> [1] "Predicting bootstrapped TPCs 63.2 %"
#> [1] "Predicting bootstrapped TPCs 63.3 %"
#> [1] "Predicting bootstrapped TPCs 63.3 %"
#> [1] "Predicting bootstrapped TPCs 63.4 %"
#> [1] "Predicting bootstrapped TPCs 63.5 %"
#> [1] "Predicting bootstrapped TPCs 63.6 %"
#> [1] "Predicting bootstrapped TPCs 63.6 %"
#> [1] "Predicting bootstrapped TPCs 63.7 %"
#> [1] "Predicting bootstrapped TPCs 63.8 %"
#> [1] "Predicting bootstrapped TPCs 63.9 %"
#> [1] "Predicting bootstrapped TPCs 63.9 %"
#> [1] "Predicting bootstrapped TPCs 64 %"
#> [1] "Predicting bootstrapped TPCs 64.1 %"
#> [1] "Predicting bootstrapped TPCs 64.2 %"
#> [1] "Predicting bootstrapped TPCs 64.2 %"
#> [1] "Predicting bootstrapped TPCs 64.3 %"
#> [1] "Predicting bootstrapped TPCs 64.4 %"
#> [1] "Predicting bootstrapped TPCs 64.5 %"
#> [1] "Predicting bootstrapped TPCs 64.6 %"
#> [1] "Predicting bootstrapped TPCs 64.6 %"
#> [1] "Predicting bootstrapped TPCs 64.7 %"
#> [1] "Predicting bootstrapped TPCs 64.8 %"
#> [1] "Predicting bootstrapped TPCs 64.9 %"
#> [1] "Predicting bootstrapped TPCs 64.9 %"
#> [1] "Predicting bootstrapped TPCs 65 %"
#> [1] "Predicting bootstrapped TPCs 65.1 %"
#> [1] "Predicting bootstrapped TPCs 65.2 %"
#> [1] "Predicting bootstrapped TPCs 65.2 %"
#> [1] "Predicting bootstrapped TPCs 65.3 %"
#> [1] "Predicting bootstrapped TPCs 65.4 %"
#> [1] "Predicting bootstrapped TPCs 65.5 %"
#> [1] "Predicting bootstrapped TPCs 65.5 %"
#> [1] "Predicting bootstrapped TPCs 65.6 %"
#> [1] "Predicting bootstrapped TPCs 65.7 %"
#> [1] "Predicting bootstrapped TPCs 65.8 %"
#> [1] "Predicting bootstrapped TPCs 65.8 %"
#> [1] "Predicting bootstrapped TPCs 65.9 %"
#> [1] "Predicting bootstrapped TPCs 66 %"
#> [1] "Predicting bootstrapped TPCs 66.1 %"
#> [1] "Predicting bootstrapped TPCs 66.1 %"
#> [1] "Predicting bootstrapped TPCs 66.2 %"
#> [1] "Predicting bootstrapped TPCs 66.3 %"
#> [1] "Predicting bootstrapped TPCs 66.4 %"
#> [1] "Predicting bootstrapped TPCs 66.4 %"
#> [1] "Predicting bootstrapped TPCs 66.5 %"
#> [1] "Predicting bootstrapped TPCs 66.6 %"
#> [1] "Predicting bootstrapped TPCs 66.7 %"
#> [1] "Predicting bootstrapped TPCs 66.7 %"
#> [1] "Predicting bootstrapped TPCs 66.8 %"
#> [1] "Predicting bootstrapped TPCs 66.9 %"
#> [1] "Predicting bootstrapped TPCs 67 %"
#> [1] "Predicting bootstrapped TPCs 67 %"
#> [1] "Predicting bootstrapped TPCs 67.1 %"
#> [1] "Predicting bootstrapped TPCs 67.2 %"
#> [1] "Predicting bootstrapped TPCs 67.3 %"
#> [1] "Predicting bootstrapped TPCs 67.3 %"
#> [1] "Predicting bootstrapped TPCs 67.4 %"
#> [1] "Predicting bootstrapped TPCs 67.5 %"
#> [1] "Predicting bootstrapped TPCs 67.6 %"
#> [1] "Predicting bootstrapped TPCs 67.6 %"
#> [1] "Predicting bootstrapped TPCs 67.7 %"
#> [1] "Predicting bootstrapped TPCs 67.8 %"
#> [1] "Predicting bootstrapped TPCs 67.9 %"
#> [1] "Predicting bootstrapped TPCs 68 %"
#> [1] "Predicting bootstrapped TPCs 68 %"
#> [1] "Predicting bootstrapped TPCs 68.1 %"
#> [1] "Predicting bootstrapped TPCs 68.2 %"
#> [1] "Predicting bootstrapped TPCs 68.3 %"
#> [1] "Predicting bootstrapped TPCs 68.3 %"
#> [1] "Predicting bootstrapped TPCs 68.4 %"
#> [1] "Predicting bootstrapped TPCs 68.5 %"
#> [1] "Predicting bootstrapped TPCs 68.6 %"
#> [1] "Predicting bootstrapped TPCs 68.6 %"
#> [1] "Predicting bootstrapped TPCs 68.7 %"
#> [1] "Predicting bootstrapped TPCs 68.8 %"
#> [1] "Predicting bootstrapped TPCs 68.9 %"
#> [1] "Predicting bootstrapped TPCs 68.9 %"
#> [1] "Predicting bootstrapped TPCs 69 %"
#> [1] "Predicting bootstrapped TPCs 69.1 %"
#> [1] "Predicting bootstrapped TPCs 69.2 %"
#> [1] "Predicting bootstrapped TPCs 69.2 %"
#> [1] "Predicting bootstrapped TPCs 69.3 %"
#> [1] "Predicting bootstrapped TPCs 69.4 %"
#> [1] "Predicting bootstrapped TPCs 69.5 %"
#> [1] "Predicting bootstrapped TPCs 69.5 %"
#> [1] "Predicting bootstrapped TPCs 69.6 %"
#> [1] "Predicting bootstrapped TPCs 69.7 %"
#> [1] "Predicting bootstrapped TPCs 69.8 %"
#> [1] "Predicting bootstrapped TPCs 69.8 %"
#> [1] "Predicting bootstrapped TPCs 69.9 %"
#> [1] "Predicting bootstrapped TPCs 70 %"
#> [1] "Predicting bootstrapped TPCs 70.1 %"
#> [1] "Predicting bootstrapped TPCs 70.1 %"
#> [1] "Predicting bootstrapped TPCs 70.2 %"
#> [1] "Predicting bootstrapped TPCs 70.3 %"
#> [1] "Predicting bootstrapped TPCs 70.4 %"
#> [1] "Predicting bootstrapped TPCs 70.4 %"
#> [1] "Predicting bootstrapped TPCs 70.5 %"
#> [1] "Predicting bootstrapped TPCs 70.6 %"
#> [1] "Predicting bootstrapped TPCs 70.7 %"
#> [1] "Predicting bootstrapped TPCs 70.7 %"
#> [1] "Predicting bootstrapped TPCs 70.8 %"
#> [1] "Predicting bootstrapped TPCs 70.9 %"
#> [1] "Predicting bootstrapped TPCs 71 %"
#> [1] "Predicting bootstrapped TPCs 71.1 %"
#> [1] "Predicting bootstrapped TPCs 71.1 %"
#> [1] "Predicting bootstrapped TPCs 71.2 %"
#> [1] "Predicting bootstrapped TPCs 71.3 %"
#> [1] "Predicting bootstrapped TPCs 71.4 %"
#> [1] "Predicting bootstrapped TPCs 71.4 %"
#> [1] "Predicting bootstrapped TPCs 71.5 %"
#> [1] "Predicting bootstrapped TPCs 71.6 %"
#> [1] "Predicting bootstrapped TPCs 71.7 %"
#> [1] "Predicting bootstrapped TPCs 71.7 %"
#> [1] "Predicting bootstrapped TPCs 71.8 %"
#> [1] "Predicting bootstrapped TPCs 71.9 %"
#> [1] "Predicting bootstrapped TPCs 72 %"
#> [1] "Predicting bootstrapped TPCs 72 %"
#> [1] "Predicting bootstrapped TPCs 72.1 %"
#> [1] "Predicting bootstrapped TPCs 72.2 %"
#> [1] "Predicting bootstrapped TPCs 72.3 %"
#> [1] "Predicting bootstrapped TPCs 72.3 %"
#> [1] "Predicting bootstrapped TPCs 72.4 %"
#> [1] "Predicting bootstrapped TPCs 72.5 %"
#> [1] "Predicting bootstrapped TPCs 72.6 %"
#> [1] "Predicting bootstrapped TPCs 72.6 %"
#> [1] "Predicting bootstrapped TPCs 72.7 %"
#> [1] "Predicting bootstrapped TPCs 72.8 %"
#> [1] "Predicting bootstrapped TPCs 72.9 %"
#> [1] "Predicting bootstrapped TPCs 72.9 %"
#> [1] "Predicting bootstrapped TPCs 73 %"
#> [1] "Predicting bootstrapped TPCs 73.1 %"
#> [1] "Predicting bootstrapped TPCs 73.2 %"
#> [1] "Predicting bootstrapped TPCs 73.2 %"
#> [1] "Predicting bootstrapped TPCs 73.3 %"
#> [1] "Predicting bootstrapped TPCs 73.4 %"
#> [1] "Predicting bootstrapped TPCs 73.5 %"
#> [1] "Predicting bootstrapped TPCs 73.5 %"
#> [1] "Predicting bootstrapped TPCs 73.6 %"
#> [1] "Predicting bootstrapped TPCs 73.7 %"
#> [1] "Predicting bootstrapped TPCs 73.8 %"
#> [1] "Predicting bootstrapped TPCs 73.8 %"
#> [1] "Predicting bootstrapped TPCs 73.9 %"
#> [1] "Predicting bootstrapped TPCs 74 %"
#> [1] "Predicting bootstrapped TPCs 74.1 %"
#> [1] "Predicting bootstrapped TPCs 74.1 %"
#> [1] "Predicting bootstrapped TPCs 74.2 %"
#> [1] "Predicting bootstrapped TPCs 74.3 %"
#> [1] "Predicting bootstrapped TPCs 74.4 %"
#> [1] "Predicting bootstrapped TPCs 74.5 %"
#> [1] "Predicting bootstrapped TPCs 74.5 %"
#> [1] "Predicting bootstrapped TPCs 74.6 %"
#> [1] "Predicting bootstrapped TPCs 74.7 %"
#> [1] "Predicting bootstrapped TPCs 74.8 %"
#> [1] "Predicting bootstrapped TPCs 74.8 %"
#> [1] "Predicting bootstrapped TPCs 74.9 %"
#> [1] "Predicting bootstrapped TPCs 75 %"
#> [1] "Predicting bootstrapped TPCs 75.1 %"
#> [1] "Predicting bootstrapped TPCs 75.1 %"
#> [1] "Predicting bootstrapped TPCs 75.2 %"
#> [1] "Predicting bootstrapped TPCs 75.3 %"
#> [1] "Predicting bootstrapped TPCs 75.4 %"
#> [1] "Predicting bootstrapped TPCs 75.4 %"
#> [1] "Predicting bootstrapped TPCs 75.5 %"
#> [1] "Predicting bootstrapped TPCs 75.6 %"
#> [1] "Predicting bootstrapped TPCs 75.7 %"
#> [1] "Predicting bootstrapped TPCs 75.7 %"
#> [1] "Predicting bootstrapped TPCs 75.8 %"
#> [1] "Predicting bootstrapped TPCs 75.9 %"
#> [1] "Predicting bootstrapped TPCs 76 %"
#> [1] "Predicting bootstrapped TPCs 76 %"
#> [1] "Predicting bootstrapped TPCs 76.1 %"
#> [1] "Predicting bootstrapped TPCs 76.2 %"
#> [1] "Predicting bootstrapped TPCs 76.3 %"
#> [1] "Predicting bootstrapped TPCs 76.3 %"
#> [1] "Predicting bootstrapped TPCs 76.4 %"
#> [1] "Predicting bootstrapped TPCs 76.5 %"
#> [1] "Predicting bootstrapped TPCs 76.6 %"
#> [1] "Predicting bootstrapped TPCs 76.6 %"
#> [1] "Predicting bootstrapped TPCs 76.7 %"
#> [1] "Predicting bootstrapped TPCs 76.8 %"
#> [1] "Predicting bootstrapped TPCs 76.9 %"
#> [1] "Predicting bootstrapped TPCs 76.9 %"
#> [1] "Predicting bootstrapped TPCs 77 %"
#> [1] "Predicting bootstrapped TPCs 77.1 %"
#> [1] "Predicting bootstrapped TPCs 77.2 %"
#> [1] "Predicting bootstrapped TPCs 77.2 %"
#> [1] "Predicting bootstrapped TPCs 77.3 %"
#> [1] "Predicting bootstrapped TPCs 77.4 %"
#> [1] "Predicting bootstrapped TPCs 77.5 %"
#> [1] "Predicting bootstrapped TPCs 77.6 %"
#> [1] "Predicting bootstrapped TPCs 77.6 %"
#> [1] "Predicting bootstrapped TPCs 77.7 %"
#> [1] "Predicting bootstrapped TPCs 77.8 %"
#> [1] "Predicting bootstrapped TPCs 77.9 %"
#> [1] "Predicting bootstrapped TPCs 77.9 %"
#> [1] "Predicting bootstrapped TPCs 78 %"
#> [1] "Predicting bootstrapped TPCs 78.1 %"
#> [1] "Predicting bootstrapped TPCs 78.2 %"
#> [1] "Predicting bootstrapped TPCs 78.2 %"
#> [1] "Predicting bootstrapped TPCs 78.3 %"
#> [1] "Predicting bootstrapped TPCs 78.4 %"
#> [1] "Predicting bootstrapped TPCs 78.5 %"
#> [1] "Predicting bootstrapped TPCs 78.5 %"
#> [1] "Predicting bootstrapped TPCs 78.6 %"
#> [1] "Predicting bootstrapped TPCs 78.7 %"
#> [1] "Predicting bootstrapped TPCs 78.8 %"
#> [1] "Predicting bootstrapped TPCs 78.8 %"
#> [1] "Predicting bootstrapped TPCs 78.9 %"
#> [1] "Predicting bootstrapped TPCs 79 %"
#> [1] "Predicting bootstrapped TPCs 79.1 %"
#> [1] "Predicting bootstrapped TPCs 79.1 %"
#> [1] "Predicting bootstrapped TPCs 79.2 %"
#> [1] "Predicting bootstrapped TPCs 79.3 %"
#> [1] "Predicting bootstrapped TPCs 79.4 %"
#> [1] "Predicting bootstrapped TPCs 79.4 %"
#> [1] "Predicting bootstrapped TPCs 79.5 %"
#> [1] "Predicting bootstrapped TPCs 79.6 %"
#> [1] "Predicting bootstrapped TPCs 79.7 %"
#> [1] "Predicting bootstrapped TPCs 79.7 %"
#> [1] "Predicting bootstrapped TPCs 79.8 %"
#> [1] "Predicting bootstrapped TPCs 79.9 %"
#> [1] "Predicting bootstrapped TPCs 80 %"
#> [1] "Predicting bootstrapped TPCs 80 %"
#> [1] "Predicting bootstrapped TPCs 80.1 %"
#> [1] "Predicting bootstrapped TPCs 80.2 %"
#> [1] "Predicting bootstrapped TPCs 80.3 %"
#> [1] "Predicting bootstrapped TPCs 80.3 %"
#> [1] "Predicting bootstrapped TPCs 80.4 %"
#> [1] "Predicting bootstrapped TPCs 80.5 %"
#> [1] "Predicting bootstrapped TPCs 80.6 %"
#> [1] "Predicting bootstrapped TPCs 80.7 %"
#> [1] "Predicting bootstrapped TPCs 80.7 %"
#> [1] "Predicting bootstrapped TPCs 80.8 %"
#> [1] "Predicting bootstrapped TPCs 80.9 %"
#> [1] "Predicting bootstrapped TPCs 81 %"
#> [1] "Predicting bootstrapped TPCs 81 %"
#> [1] "Predicting bootstrapped TPCs 81.1 %"
#> [1] "Predicting bootstrapped TPCs 81.2 %"
#> [1] "Predicting bootstrapped TPCs 81.3 %"
#> [1] "Predicting bootstrapped TPCs 81.3 %"
#> [1] "Predicting bootstrapped TPCs 81.4 %"
#> [1] "Predicting bootstrapped TPCs 81.5 %"
#> [1] "Predicting bootstrapped TPCs 81.6 %"
#> [1] "Predicting bootstrapped TPCs 81.6 %"
#> [1] "Predicting bootstrapped TPCs 81.7 %"
#> [1] "Predicting bootstrapped TPCs 81.8 %"
#> [1] "Predicting bootstrapped TPCs 81.9 %"
#> [1] "Predicting bootstrapped TPCs 81.9 %"
#> [1] "Predicting bootstrapped TPCs 82 %"
#> [1] "Predicting bootstrapped TPCs 82.1 %"
#> [1] "Predicting bootstrapped TPCs 82.2 %"
#> [1] "Predicting bootstrapped TPCs 82.2 %"
#> [1] "Predicting bootstrapped TPCs 82.3 %"
#> [1] "Predicting bootstrapped TPCs 82.4 %"
#> [1] "Predicting bootstrapped TPCs 82.5 %"
#> [1] "Predicting bootstrapped TPCs 82.5 %"
#> [1] "Predicting bootstrapped TPCs 82.6 %"
#> [1] "Predicting bootstrapped TPCs 82.7 %"
#> [1] "Predicting bootstrapped TPCs 82.8 %"
#> [1] "Predicting bootstrapped TPCs 82.8 %"
#> [1] "Predicting bootstrapped TPCs 82.9 %"
#> [1] "Predicting bootstrapped TPCs 83 %"
#> [1] "Predicting bootstrapped TPCs 83.1 %"
#> [1] "Predicting bootstrapped TPCs 83.1 %"
#> [1] "Predicting bootstrapped TPCs 83.2 %"
#> [1] "Predicting bootstrapped TPCs 83.3 %"
#> [1] "Predicting bootstrapped TPCs 83.4 %"
#> [1] "Predicting bootstrapped TPCs 83.4 %"
#> [1] "Predicting bootstrapped TPCs 83.5 %"
#> [1] "Predicting bootstrapped TPCs 83.6 %"
#> [1] "Predicting bootstrapped TPCs 83.7 %"
#> [1] "Predicting bootstrapped TPCs 83.7 %"
#> [1] "Predicting bootstrapped TPCs 83.8 %"
#> [1] "Predicting bootstrapped TPCs 83.9 %"
#> [1] "Predicting bootstrapped TPCs 84 %"
#> [1] "Predicting bootstrapped TPCs 84.1 %"
#> [1] "Predicting bootstrapped TPCs 84.1 %"
#> [1] "Predicting bootstrapped TPCs 84.2 %"
#> [1] "Predicting bootstrapped TPCs 84.3 %"
#> [1] "Predicting bootstrapped TPCs 84.4 %"
#> [1] "Predicting bootstrapped TPCs 84.4 %"
#> [1] "Predicting bootstrapped TPCs 84.5 %"
#> [1] "Predicting bootstrapped TPCs 84.6 %"
#> [1] "Predicting bootstrapped TPCs 84.7 %"
#> [1] "Predicting bootstrapped TPCs 84.7 %"
#> [1] "Predicting bootstrapped TPCs 84.8 %"
#> [1] "Predicting bootstrapped TPCs 84.9 %"
#> [1] "Predicting bootstrapped TPCs 85 %"
#> [1] "Predicting bootstrapped TPCs 85 %"
#> [1] "Predicting bootstrapped TPCs 85.1 %"
#> [1] "Predicting bootstrapped TPCs 85.2 %"
#> [1] "Predicting bootstrapped TPCs 85.3 %"
#> [1] "Predicting bootstrapped TPCs 85.3 %"
#> [1] "Predicting bootstrapped TPCs 85.4 %"
#> [1] "Predicting bootstrapped TPCs 85.5 %"
#> [1] "Predicting bootstrapped TPCs 85.6 %"
#> [1] "Predicting bootstrapped TPCs 85.6 %"
#> [1] "Predicting bootstrapped TPCs 85.7 %"
#> [1] "Predicting bootstrapped TPCs 85.8 %"
#> [1] "Predicting bootstrapped TPCs 85.9 %"
#> [1] "Predicting bootstrapped TPCs 85.9 %"
#> [1] "Predicting bootstrapped TPCs 86 %"
#> [1] "Predicting bootstrapped TPCs 86.1 %"
#> [1] "Predicting bootstrapped TPCs 86.2 %"
#> [1] "Predicting bootstrapped TPCs 86.2 %"
#> [1] "Predicting bootstrapped TPCs 86.3 %"
#> [1] "Predicting bootstrapped TPCs 86.4 %"
#> [1] "Predicting bootstrapped TPCs 86.5 %"
#> [1] "Predicting bootstrapped TPCs 86.5 %"
#> [1] "Predicting bootstrapped TPCs 86.6 %"
#> [1] "Predicting bootstrapped TPCs 86.7 %"
#> [1] "Predicting bootstrapped TPCs 86.8 %"
#> [1] "Predicting bootstrapped TPCs 86.8 %"
#> [1] "Predicting bootstrapped TPCs 86.9 %"
#> [1] "Predicting bootstrapped TPCs 87 %"
#> [1] "Predicting bootstrapped TPCs 87.1 %"
#> [1] "Predicting bootstrapped TPCs 87.2 %"
#> [1] "Predicting bootstrapped TPCs 87.2 %"
#> [1] "Predicting bootstrapped TPCs 87.3 %"
#> [1] "Predicting bootstrapped TPCs 87.4 %"
#> [1] "Predicting bootstrapped TPCs 87.5 %"
#> [1] "Predicting bootstrapped TPCs 87.5 %"
#> [1] "Predicting bootstrapped TPCs 87.6 %"
#> [1] "Predicting bootstrapped TPCs 87.7 %"
#> [1] "Predicting bootstrapped TPCs 87.8 %"
#> [1] "Predicting bootstrapped TPCs 87.8 %"
#> [1] "Predicting bootstrapped TPCs 87.9 %"
#> [1] "Predicting bootstrapped TPCs 88 %"
#> [1] "Predicting bootstrapped TPCs 88.1 %"
#> [1] "Predicting bootstrapped TPCs 88.1 %"
#> [1] "Predicting bootstrapped TPCs 88.2 %"
#> [1] "Predicting bootstrapped TPCs 88.3 %"
#> [1] "Predicting bootstrapped TPCs 88.4 %"
#> [1] "Predicting bootstrapped TPCs 88.4 %"
#> [1] "Predicting bootstrapped TPCs 88.5 %"
#> [1] "Predicting bootstrapped TPCs 88.6 %"
#> [1] "Predicting bootstrapped TPCs 88.7 %"
#> [1] "Predicting bootstrapped TPCs 88.7 %"
#> [1] "Predicting bootstrapped TPCs 88.8 %"
#> [1] "Predicting bootstrapped TPCs 88.9 %"
#> [1] "Predicting bootstrapped TPCs 89 %"
#> [1] "Predicting bootstrapped TPCs 89 %"
#> [1] "Predicting bootstrapped TPCs 89.1 %"
#> [1] "Predicting bootstrapped TPCs 89.2 %"
#> [1] "Predicting bootstrapped TPCs 89.3 %"
#> [1] "Predicting bootstrapped TPCs 89.3 %"
#> [1] "Predicting bootstrapped TPCs 89.4 %"
#> [1] "Predicting bootstrapped TPCs 89.5 %"
#> [1] "Predicting bootstrapped TPCs 89.6 %"
#> [1] "Predicting bootstrapped TPCs 89.6 %"
#> [1] "Predicting bootstrapped TPCs 89.7 %"
#> [1] "Predicting bootstrapped TPCs 89.8 %"
#> [1] "Predicting bootstrapped TPCs 89.9 %"
#> [1] "Predicting bootstrapped TPCs 89.9 %"
#> [1] "Predicting bootstrapped TPCs 90 %"
#> [1] "Predicting bootstrapped TPCs 90.1 %"
#> [1] "Predicting bootstrapped TPCs 90.2 %"
#> [1] "Predicting bootstrapped TPCs 90.2 %"
#> [1] "Predicting bootstrapped TPCs 90.3 %"
#> [1] "Predicting bootstrapped TPCs 90.4 %"
#> [1] "Predicting bootstrapped TPCs 90.5 %"
#> [1] "Predicting bootstrapped TPCs 90.6 %"
#> [1] "Predicting bootstrapped TPCs 90.6 %"
#> [1] "Predicting bootstrapped TPCs 90.7 %"
#> [1] "Predicting bootstrapped TPCs 90.8 %"
#> [1] "Predicting bootstrapped TPCs 90.9 %"
#> [1] "Predicting bootstrapped TPCs 90.9 %"
#> [1] "Predicting bootstrapped TPCs 91 %"
#> [1] "Predicting bootstrapped TPCs 91.1 %"
#> [1] "Predicting bootstrapped TPCs 91.2 %"
#> [1] "Predicting bootstrapped TPCs 91.2 %"
#> [1] "Predicting bootstrapped TPCs 91.3 %"
#> [1] "Predicting bootstrapped TPCs 91.4 %"
#> [1] "Predicting bootstrapped TPCs 91.5 %"
#> [1] "Predicting bootstrapped TPCs 91.5 %"
#> [1] "Predicting bootstrapped TPCs 91.6 %"
#> [1] "Predicting bootstrapped TPCs 91.7 %"
#> [1] "Predicting bootstrapped TPCs 91.8 %"
#> [1] "Predicting bootstrapped TPCs 91.8 %"
#> [1] "Predicting bootstrapped TPCs 91.9 %"
#> [1] "Predicting bootstrapped TPCs 92 %"
#> [1] "Predicting bootstrapped TPCs 92.1 %"
#> [1] "Predicting bootstrapped TPCs 92.1 %"
#> [1] "Predicting bootstrapped TPCs 92.2 %"
#> [1] "Predicting bootstrapped TPCs 92.3 %"
#> [1] "Predicting bootstrapped TPCs 92.4 %"
#> [1] "Predicting bootstrapped TPCs 92.4 %"
#> [1] "Predicting bootstrapped TPCs 92.5 %"
#> [1] "Predicting bootstrapped TPCs 92.6 %"
#> [1] "Predicting bootstrapped TPCs 92.7 %"
#> [1] "Predicting bootstrapped TPCs 92.7 %"
#> [1] "Predicting bootstrapped TPCs 92.8 %"
#> [1] "Predicting bootstrapped TPCs 92.9 %"
#> [1] "Predicting bootstrapped TPCs 93 %"
#> [1] "Predicting bootstrapped TPCs 93 %"
#> [1] "Predicting bootstrapped TPCs 93.1 %"
#> [1] "Predicting bootstrapped TPCs 93.2 %"
#> [1] "Predicting bootstrapped TPCs 93.3 %"
#> [1] "Predicting bootstrapped TPCs 93.3 %"
#> [1] "Predicting bootstrapped TPCs 93.4 %"
#> [1] "Predicting bootstrapped TPCs 93.5 %"
#> [1] "Predicting bootstrapped TPCs 93.6 %"
#> [1] "Predicting bootstrapped TPCs 93.7 %"
#> [1] "Predicting bootstrapped TPCs 93.7 %"
#> [1] "Predicting bootstrapped TPCs 93.8 %"
#> [1] "Predicting bootstrapped TPCs 93.9 %"
#> [1] "Predicting bootstrapped TPCs 94 %"
#> [1] "Predicting bootstrapped TPCs 94 %"
#> [1] "Predicting bootstrapped TPCs 94.1 %"
#> [1] "Predicting bootstrapped TPCs 94.2 %"
#> [1] "Predicting bootstrapped TPCs 94.3 %"
#> [1] "Predicting bootstrapped TPCs 94.3 %"
#> [1] "Predicting bootstrapped TPCs 94.4 %"
#> [1] "Predicting bootstrapped TPCs 94.5 %"
#> [1] "Predicting bootstrapped TPCs 94.6 %"
#> [1] "Predicting bootstrapped TPCs 94.6 %"
#> [1] "Predicting bootstrapped TPCs 94.7 %"
#> [1] "Predicting bootstrapped TPCs 94.8 %"
#> [1] "Predicting bootstrapped TPCs 94.9 %"
#> [1] "Predicting bootstrapped TPCs 94.9 %"
#> [1] "Predicting bootstrapped TPCs 95 %"
#> [1] "Predicting bootstrapped TPCs 95.1 %"
#> [1] "Predicting bootstrapped TPCs 95.2 %"
#> [1] "Predicting bootstrapped TPCs 95.2 %"
#> [1] "Predicting bootstrapped TPCs 95.3 %"
#> [1] "Predicting bootstrapped TPCs 95.4 %"
#> [1] "Predicting bootstrapped TPCs 95.5 %"
#> [1] "Predicting bootstrapped TPCs 95.5 %"
#> [1] "Predicting bootstrapped TPCs 95.6 %"
#> [1] "Predicting bootstrapped TPCs 95.7 %"
#> [1] "Predicting bootstrapped TPCs 95.8 %"
#> [1] "Predicting bootstrapped TPCs 95.8 %"
#> [1] "Predicting bootstrapped TPCs 95.9 %"
#> [1] "Predicting bootstrapped TPCs 96 %"
#> [1] "Predicting bootstrapped TPCs 96.1 %"
#> [1] "Predicting bootstrapped TPCs 96.1 %"
#> [1] "Predicting bootstrapped TPCs 96.2 %"
#> [1] "Predicting bootstrapped TPCs 96.3 %"
#> [1] "Predicting bootstrapped TPCs 96.4 %"
#> [1] "Predicting bootstrapped TPCs 96.4 %"
#> [1] "Predicting bootstrapped TPCs 96.5 %"
#> [1] "Predicting bootstrapped TPCs 96.6 %"
#> [1] "Predicting bootstrapped TPCs 96.7 %"
#> [1] "Predicting bootstrapped TPCs 96.7 %"
#> [1] "Predicting bootstrapped TPCs 96.8 %"
#> [1] "Predicting bootstrapped TPCs 96.9 %"
#> [1] "Predicting bootstrapped TPCs 97 %"
#> [1] "Predicting bootstrapped TPCs 97.1 %"
#> [1] "Predicting bootstrapped TPCs 97.1 %"
#> [1] "Predicting bootstrapped TPCs 97.2 %"
#> [1] "Predicting bootstrapped TPCs 97.3 %"
#> [1] "Predicting bootstrapped TPCs 97.4 %"
#> [1] "Predicting bootstrapped TPCs 97.4 %"
#> [1] "Predicting bootstrapped TPCs 97.5 %"
#> [1] "Predicting bootstrapped TPCs 97.6 %"
#> [1] "Predicting bootstrapped TPCs 97.7 %"
#> [1] "Predicting bootstrapped TPCs 97.7 %"
#> [1] "Predicting bootstrapped TPCs 97.8 %"
#> [1] "Predicting bootstrapped TPCs 97.9 %"
#> [1] "Predicting bootstrapped TPCs 98 %"
#> [1] "Predicting bootstrapped TPCs 98 %"
#> [1] "Predicting bootstrapped TPCs 98.1 %"
#> [1] "Predicting bootstrapped TPCs 98.2 %"
#> [1] "Predicting bootstrapped TPCs 98.3 %"
#> [1] "Predicting bootstrapped TPCs 98.3 %"
#> [1] "Predicting bootstrapped TPCs 98.4 %"
#> [1] "Predicting bootstrapped TPCs 98.5 %"
#> [1] "Predicting bootstrapped TPCs 98.6 %"
#> [1] "Predicting bootstrapped TPCs 98.6 %"
#> [1] "Predicting bootstrapped TPCs 98.7 %"
#> [1] "Predicting bootstrapped TPCs 98.8 %"
#> [1] "Predicting bootstrapped TPCs 98.9 %"
#> [1] "Predicting bootstrapped TPCs 98.9 %"
#> [1] "Predicting bootstrapped TPCs 99 %"
#> [1] "Predicting bootstrapped TPCs 99.1 %"
#> [1] "Predicting bootstrapped TPCs 99.2 %"
#> [1] "Predicting bootstrapped TPCs 99.2 %"
#> [1] "Predicting bootstrapped TPCs 99.3 %"
#> [1] "Predicting bootstrapped TPCs 99.4 %"
#> [1] "Predicting bootstrapped TPCs 99.5 %"
#> [1] "Predicting bootstrapped TPCs 99.5 %"
#> [1] "Predicting bootstrapped TPCs 99.6 %"
#> [1] "Predicting bootstrapped TPCs 99.7 %"
#> [1] "Predicting bootstrapped TPCs 99.8 %"
#> [1] "Predicting bootstrapped TPCs 99.8 %"
#> [1] "Predicting bootstrapped TPCs 99.9 %"
#> [1] "Predicting bootstrapped TPCs 100 %"
```

### 4. Plot uncertainty TPCs:

The bootstrapped uncertainty curves can be plotted easily with the
`plot_uncertainties()` function.

``` r
plot_uncertainties(bootstrap_uncertainties_tpcs = preds_boots_aphid,
                   temp = b.schwartzi_satar2002$temperature,
                   dev_rate = b.schwartzi_satar2002$rate_value,
                   species = "Brachycaudus schwartzi",
                   life_stage = "Nymphs")
#> Warning: Removed 89 rows containing missing values (`geom_line()`).
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

### 5. Calculate thermal suitability bounds:

Once a model have been selected under both ecological realism and
statistical criteria, the user can estimate the thermal boundaries
defining the suitable range of temperatures for the studied population.
The `thermal_suitability_bounds()` function calculate these values given
the `tibble` output from `predict_curves()` function and the selected
model name (note that only one model is allowed each time for this
function). Additionally, a value of suitability defining the
quantile-upper part of the curve can be provided by the user
($\text{Q}_{75}$ by default). If the user has propagated uncertainty in
`predict_curves()` function, this function inherits the bootstrapped
TPCs to calculate thermal suitability boundaries for each bootstrapped
curved in addition to the estimated curve.

``` r
boundaries_aphid <- therm_suit_bounds(preds_tbl = preds_boots_aphid,
                                      model_name = "lactin2",
                                      suitability_threshold = 80)
print(boundaries_aphid)
#> # A tibble: 102 × 6
#>    model_name suitability tval_left tval_right pred_suit  iter
#>    <chr>      <chr>           <dbl>      <dbl>     <dbl> <int>
#>  1 lactin2    80 %             NA         NA      NA        NA
#>  2 lactin2    80 %             21.8       37.2     0.111     1
#>  3 lactin2    80 %             NA         NA       0.132     2
#>  4 lactin2    80 %             21.6       35.8     0.115     3
#>  5 lactin2    80 %             20.7       37.3     0.112     4
#>  6 lactin2    80 %             21.3       36.6     0.110     5
#>  7 lactin2    80 %             21.4       36.1     0.118     6
#>  8 lactin2    80 %             22.4       35.2     0.117     7
#>  9 lactin2    80 %             21.1       37.7     0.110     8
#> 10 lactin2    80 %             21.6       35.7     0.117     9
#> # ℹ 92 more rows
```

### 4. Climatic data extraction and projection

Using the thermal boundaries provided by the previous function and a set
of raster maps of monthly temperatures for a given region (which can be
provided by the user or downloaded by the function), a map can be
produced showing where (and for how many months a year) thermal
conditions are suitable for the development of the pest.

``` r
# downloading data from geodata::wordlclim_global. It will take several minutes the first time you use the function on the same 'path'.
#   risk_rast <- map_risk(t_vals = c(thermal_boundaries_sharpshooter$tval_left,
#                                    thermal_boundaries_sharpshooter$tval_right),
#                         path = "~/downloaded_maps", # directory to download data
#                         region = "Réunion", 
#                         verbose = TRUE)
# terra::plot(risk_rast[[13]]) # the last layer represents the sum of suitable months within a year; the first 12 layers, the monthly binary value (1, if suitable; 0, if not suitable).

#we can also save the raster with:
# terra::writeRaster(risk_rast, filename = "~/output_maps/risk_rast.tif")

# Alternatively, if you already have a raster of monthly average temperatures for your region of interest, you can use it as an input of `map_risk()`
## load it (here Luxembourg data)
#tavg_file <- system.file("extdata/tavg_lux.tif", package = "mappestRisk")
### convert it into a raster-compatible file with `terra`
#tavg_rast <- terra::rast(tavg_file)
### and apply the function
#risk_rast_binary <- map_risk(t_vals = c(thermal_boundaries_sharpshooter$tval_left,
#                                        thermal_boundaries_sharpshooter$tval_right#), 
#                             t_rast = tavg_rast)
# terra::plot(risk_rast_binary[[13]]) # the last layer represents the sum of suitable months within a year; the 12-th previous ones, the monthly binary value (1, if suitable; 0, if not suitable).
```

### 5. Interactive map with `leaflet`

``` r
#example <- interactive_map(x = risk_rast_binary, map_type = "high",
 #               path_out = paste0(tempdir(), "test_map.html"))
#example

#htmlwidgets::saveWidget(example,file = "index.html")
```

A provisional example of the kind of output from `interactive_map()` is
shown in the following animated picture:

![](images/mappestRisk_interactive.gif)

## Citation

If using this package, please cite it:

``` r
citation("mappestRisk")

To cite mappestRisk in publications use:

  San Segundo Molina, D., Barbosa, A.M., Pérez-Luque, A.J. &
  Rodríguez-Sánchez, F. 2023. mappestRisk: Create Maps Forecasting Risk
  of Pest Occurrence
  https://ecologyr.github.io/templateRpackage/mappestRisk

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {mappestRisk},
    author = {Darío {San-Segundo  Molina} and A. Márcia Barbosa and Antonio Jesús Pérez-Luque and Francisco Rodríguez-Sánchez},
    year = {2024},
    url = {https://ecologyr.github.io/templateRpackage/mappestRisk},
  }
```

## Funding

The development of this software has been funded by Fondo Europeo de
Desarrollo Regional (FEDER) and Consejería de Transformación Económica,
Industria, Conocimiento y Universidades of Junta de Andalucía (proyecto
US-1381388 led by Francisco Rodríguez Sánchez, Universidad de Sevilla).

![](https://ecologyr.github.io/workshop/images/logos.png)

## References:

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-leaflet" class="csl-entry">

Cheng, Joe, Bhaskar Karambelkar, and Yihui Xie. 2022. “Leaflet: Create
Interactive Web Maps with the JavaScript ’Leaflet’ Library.”
<https://CRAN.R-project.org/package=leaflet>.

</div>

<div id="ref-nls.multstart" class="csl-entry">

Padfield, Daniel, and Granville Matheson. 2020. “Nls.multstart: Robust
Non-Linear Regression Using AIC Scores.”
<https://CRAN.R-project.org/package=nls.multstart>.

</div>

<div id="ref-padfield2021" class="csl-entry">

Padfield, Daniel, Hannah O’Sullivan, and Samraat Pawar. 2021. “rTPC and
Nls.multstart: A New Pipeline to Fit Thermal Performance Curves in r.”
*Methods in Ecology and Evolution* 12 (6): 1138–43.
<https://doi.org/10.1111/2041-210X.13585>.

</div>

<div id="ref-nlme" class="csl-entry">

Pinheiro, José, Douglas Bates, and R Core Team. 2022. “Nlme: Linear and
Nonlinear Mixed Effects Models.”
<https://CRAN.R-project.org/package=nlme>.

</div>

<div id="ref-rebaudo2018" class="csl-entry">

Rebaudo, François, Quentin Struelens, and Olivier Dangles. 2018.
“Modelling Temperature-Dependent Development Rate and Phenology in
Arthropods: The devRate Package for r.” *Methods in Ecology and
Evolution* 9 (4): 1144–50.
https://doi.org/<https://doi.org/10.1111/2041-210X.12935>.

</div>

<div id="ref-taylor2019" class="csl-entry">

Taylor, Rachel A., Sadie J. Ryan, Catherine A. Lippi, David G. Hall,
Hossein A. Narouei-Khandan, Jason R. Rohr, and Leah R. Johnson. 2019.
“Predicting the Fundamental Thermal Niche of Crop Pests and Diseases in
a Changing World: A Case Study on Citrus Greening.” *Journal of Applied
Ecology* 56 (8): 2057–68. <https://doi.org/10.1111/1365-2664.13455>.

</div>

</div>

[^1]: At least 4 unique temperatures are required. Fore more details,
    see documentation and vignettes.
