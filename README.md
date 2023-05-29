
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
understandable forecasts assessing risk of pest occurrence in a certain
region.

For that purpose, mappestRisk is built upon previous efforts such as
`devRate` (Rebaudo, Struelens, and Dangles 2018), `rTPC` and
`nls.multstart` packages (Padfield, O’Sullivan, and Pawar 2021) and a
methodology for predicting climatic suitability based on fundamental
thermal niche as estimated by mechanistic, process-based approaches
suggested in Taylor et al. (2019) . Therefore, mappestRisk has three
different modules: *(1) model fitting & selection* using a set of the
most used equations describing developmental responses to temperature
under `nls.multstart` (Padfield and Matheson 2020) and `nlme` (Pinheiro,
Bates 2022) frameworks with visualization of model fitting to help model
selection by the user; (2) *thermal traits extraction:* including
selection of the suitability threshold guiding the forecast
(i.e. obtaining the temperatures at which estimated performance lies
upon a performance higher threshold percentage); and (3) *climatic data
extraction & visualization* with either exportable rasters or
interactive maps with `leaflet` (Cheng, Karambelkar, and Xie 2022).

## Installation

``` r
# install.packages("devtools")
devtools::install_github("EcologyR/mappestRisk")

#or alternatively
remotes::install_github("EcologyR/mappestRisk")

#and load the package
library(mappestRisk)
```

If you want to clone or fork the repository or open and read some
issues, you can find the code
[here](https://github.com/EcologyR/mappestRisk).

## Example: mappestRisk workflow

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

``` r
plot(pressure)
```

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

## Citation

If using this package, please cite it:

``` r
# citation("mappestRisk")
```

## Funding

The development of this software has been funded by Fondo Europeo de
Desarrollo Regional (FEDER) and Consejería de Transformación Económica,
Industria, Conocimiento y Universidades of Junta de Andalucía (proyecto
US-1381388 led by Francisco Rodríguez Sánchez, Universidad de Sevilla).

![](https://ecologyr.github.io/workshop/images/logos.png)

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
