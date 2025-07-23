---
title: 'mappestRisk: An R package for modelling and mapping risk of  pest development based on known thermal limits'
tags:
  - R
  - pest risk assessment
  - thermal performance curves
  - global warming
  - thermal ecology
authors:
  - name: Darío San-Segundo Molina
    orcid: 0000-0002-7831-9623
    equal-contrib: false
    affiliation: 1
    corresponding: true
  - name: A. Marcia Barbosa
    orcid: 0000-0001-8972-7713
    equal-contrib: false
    affiliation: 2
  - name: Antonio J. Pérez-Luque
    orcid: 0000-0002-1747-0469
    equal-contrib: false
    affiliation: 3
  - name: Francisco Rodríguez-Sánchez
    orcid: 0000-0002-7981-1599
    equal-contrib: false
    affiliation: 4
affiliations:
 - name: Departament of Life Sciences, GloCEE –Global Change Ecology & Evolution Research Group, University of Alcalá, Alcalá de Henares, Spain
   index: 1
 - name: CICGE – Centro de Investigação em Ciências Geo-Espaciais, Faculdade de Ciências, Universidade do Porto, Portugal
   index: 2
 - name: Institute of Forest Sciences (ICIFOR), National Institute of Agricultural and Food Research and Technology (INIA-CSIC), Ctra. de La Coruña km 7.5, 28040 Madrid, Spain
   index: 3
 - name: Departamento de Biología Vegetal y Ecología, Universidad de Sevilla, Sevilla, Spain
   index: 4
date: 23 July 2025
output: 
  md_document:
    variant: markdown
    preserve_yaml: true
bibliography: paper.bib
csl: ecology.csl
---

# Summary

`mappestRisk` is an R package designed to facilitate ecological
modelling for pest risk assessment purposes to scientists from
experimental pest research labs through an easy-to-use open software. It
provides seven functions for modelling development rates variation
across temperatures to elaborate climatic suitability maps.

# Statement of need

Under ongoing global warming and recent crop pest invasions, there is an
urgent need to fill the gap between experimental research on crop pests'
thermal biology and applied forecasts informing policies for pest
management.

Experiments on how crop pests' development rates vary across
temperatures are abundant in the literature[^1]. This has lead to
numerous risk maps based on linear models for this response predicting
the number of generations a year that a certain pest could potentially
complete[e.g., @efsa2017b]. Nonlinear modelling of these responses,
i.e., fitting Thermal Performance Curve (TPC) for development rates can
improve these models while using th same source --largely available--
data, while preventing data-demanding sophisticated demographic models
(e.g., @gutierrez2021). Facilitating easy-to-use tools to address
nonlinear modelling and spatial projections to experimental, lab
researchers with basic skills in R would promote elaboration of custom,
reproducible and understandable forecasts for pest risk assessment.

# State of the field

Several tools have already addressed this gap. The R packages `devRate`
[@rebaudo2018]and `rTPC` [@padfield2021] support nonlinear thermal
performance curve (TPC) modelling by providing equations, parameter's
start values, and fitting functions. `devRate` also includes functions
for spatial forecasting of generation numbers, while `rTPC` offers tools
for calculating thermal traits of ecological relevance. Besides, *Insect
Life Cycle Modelling* ([ILCyM]{.smallcaps}) [@sporleder2017] software
allows experimental researchers to fit TPCs and simulate phenology with
GIS extensions. Its R Shiny interface supports broader accessibility for
non-experts.

The `mappestRisk` package adds to this growing ecosystem by offering a
reproducible, open-source, and easy-to-use pipeline for modelling pest
risk under climate variability. It extends `devRate` with more TPC
models and uncertainty propagation, integrates `rTPC` and
`nls.multstart` workflows into a simplified interface, and yields custom
spatial projections.

# Main features

## Fitting Thermal Performance Curves

`mappestRisk` facilitates fitting several nonlinear equations based on
`rTPC` -- `nls.multstart` and `devRate` packages. This is accomplished
by the `fit_devmodels()` function, which only requires the user to
provide a data set with at least four different temperatures with their
corresponding development rate values and choose which of the the
available TPC model equations are to be fitted. It returns a table with
relevant statistics. Fitted TPC shapes can be graphically examined for
an ecologically guided model selection using the `plot_devmodels()`
function.

Additionally, `mappestRisk` adapts into a function the code pipeline
from `rTPC`'s
[vignettes](https://padpadpadpad.github.io/rTPC/articles/bootstrapping_models.html)
for bootstrapping TPCs in `predict_curves()`. Using bootstrap with
residual resampling, this function simulates new curves that can be
serve as parameter uncertainty ribbons for the fitted TPC. For
visualization purposes, `plot_uncertainties()` depicts these simulations
obtained with `predict_curves()`.

## Thermal Limits Calculation

Inspired by research on demographic models (e.g., @taylor2019),
`mappestRisk` uses a directly applicable risk index metric, namely the
*number of months a year with optimal temperatures for development*.
Since development rate has major contributions to shaping the
ectotherms' fitness TPCs [@pawar2024], obtaining the temperature range
optimizing development may approximate the maximum potential viability
of a population.

The function `therm_suit_bounds()` uses the previously bootstrapped
curves to calculate the temperatures delimiting the top region of the
curve using a quantile threshold (default to $Q^{80}$) provided by the
user.

## Spatial Projections

The `map_risk()` function projects these thermal limits into monthly
average temperatures at the target, user-defined regions to elaborate
the climatic suitability maps. It uses WorldCim's historical climatic
data set ([https://worldclim.org/data/worldclim21.html](#0){.uri}) by
default, enabling the user to provide its own. This function outputs a
simple map visualization accompanied by a spatial raster (*.tiff*) that
can be exported for GIS processing.

# Examples:

Detailed examples of pipelines of `mappestRisk` are available in the
[package website](https://ecologyr.github.io/mappestRisk/) articles.

# Collaboration:

Designed as a collaborative tool, `mappestRisk` welcomes contributions
to expand biological models, data sources, and forecasting methods.

[^1]: See e.g., datasheets available at EPPO (<https://gd.eppo.int/>)
    and at CABI
    (<https://www.cabidigitallibrary.org/journal/cabicompendium>)
    websites.
