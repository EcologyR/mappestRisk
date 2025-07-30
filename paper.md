---
title: 'mappestRisk: An R package for modelling and mapping risk of pest development based on known thermal limits'
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

`mappestRisk` is an R package for ecological modelling in pest 
risk assessment. It offers an easy-to-use, open-source workflow to model 
organism development rates across temperature gradients and to elaborate
thermal suitability maps which can be used to anticipate pest outbreaks.

# Statement of need

Under ongoing global warming and recent pest invasions in crops, there is an
urgent need to fill the gap between experimental research on crop pests’ thermal 
biology and applied forecasting to inform policies for pest management.

Experimental studies on how crop pest development rates vary with 
temperatures are abundant[^1]. This has enabled the  creation 
of many risk maps based on linear models predicting the potential 
number of pest generations  per year[e.g., @efsa2017b]. However, nonlinear 
modelling  of temperature-driven development rates, i.e., fitting Thermal 
Performance Curves (TPCs), can improve forecasts while preventing 
data-demanding sophisticated demographic models (e.g., @gutierrez2021). 
Developing user-friendly tools to address nonlinear modelling and spatial 
risk projections for experimental lab researchers with basic R skills 
could facilitate the production of custom, reproducible and 
understandable forecasts for pest risk assessment.

# State of the field

Several tools have already addressed this gap. The R packages `devRate`
[@rebaudo2018] and `rTPC` [@padfield2021] support nonlinear thermal
performance curve modelling by providing equations, parameters
start values, and fitting functions. `devRate` also includes functions
for spatial forecasting of generation numbers, while `rTPC` offers tools
for calculating thermally relevant ecological traits. In addition, the *Insect
Life Cycle Modelling* ([ILCyM]{.smallcaps}) [@sporleder2017] software
enables experimental researchers to fit TPCs and simulate phenology with
GIS extensions. Its R Shiny interface supports broader accessibility for
non-experts.

The `mappestRisk` package contributes to this growing ecosystem by 
offering a reproducible, open-source, and user-friendly pipeline for 
modelling pest risk under climate variability. It extends `devRate` 
by including additional TPC models and uncertainty propagation, 
integrates `rTPC` and `nls.multstart` workflows through a simplified 
interface, and produces custom spatial projections.

# Main features

## Fitting Thermal Performance Curves

`mappestRisk` facilitates fitting several nonlinear equations based on
`rTPC`, `nls.multstart` and `devRate` packages. This is accomplished
by the `fit_devmodels()` function, which requires only a dataset with the
organism development rate at different temperatures the choice of a 
development model equation from those available in the package. 
The function returns a table with adjusted parameters and relevant 
model information. Fitted TPC shapes can be visualized using the
`plot_devmodels()` function to support ecologically guided model selection.

Additionally, `mappestRisk` adapts into a function the code pipeline
from `rTPC`'s
[vignettes](https://padpadpadpad.github.io/rTPC/articles/bootstrapping_models.html)
for bootstrapping TPCs in `predict_curves()`. Using bootstrap with
residual resampling, this function simulates new curves that can serve 
as parameter uncertainty ribbons for the fitted TPC. For
visualization purposes, `plot_uncertainties()` depicts these simulations
obtained with `predict_curves()`.

## Thermal Limits Calculation

Inspired by demographic modelling approaches (e.g., @taylor2019),
`mappestRisk` uses a directly applicable risk index metric, namely the
*number of months per year with optimal temperatures for development*.
Since development rate has major contributions to shaping the
ectotherms' fitness TPCs [@pawar2024], identifying the temperature range
that optimizes development offers a useful proxy for estimating the 
the maximum potential viability of a population.

The function `therm_suit_bounds()` uses the previously bootstrapped
curves to calculate the temperatures delimiting the top region of the
curve using a quantile threshold (default to $Q^{80}$) provided by the
user.

## Spatial Projections

The `map_risk()` function projects thermal suitability limits into monthly
average temperatures across user-defined regions to elaborate
the thermal suitability maps. By default, it uses historical climatic data from 
WorldClim ([https://worldclim.org/data/worldclim21.html](#0){.uri}), 
but users can provide custom monthly climate datasets. This function 
outputs a simple visual map and a spatial raster file (*.tif*) that
can be exported for furhter GIS-based mapping and processing.

# Examples

A summarized pipeline of `mappestRisk` is shown in @fig:workflow.
![Figure 1: A workflow with simplified output examples from `mappestRisk`.](figures/workflow_mappestRisk.png){#fig:workflow width=100% }
Detailed examples are available in the 
[package website](https://ecologyr.github.io/mappestRisk/) articles.

# Collaboration

Designed as a collaborative tool, `mappestRisk` welcomes contributions
to expand its biological models, data sources, and forecasting methods.

# Acknowledgements

The development of this software has been funded by Fondo Europeo de
Desarrollo Regional (FEDER) and Consejería de Transformación Económica,
Industria, Conocimiento y Universidades of Junta de Andalucía (proyecto
US-1381388, Universidad de Sevilla). AJPL acknowledges support from 
MCIN/AEI/10.13039/501100011033 and the 
European Union NextGenerationEU/PRTR through the
 “Juan de la Cierva” fellowship programme (JDC2022-050056-I). DSSM
acknowledges funding from the Spanish Ministry of Universities 
(grant no. FPU20/05528 to D.S.-S.M.).

[^1]: See e.g., datasheets available at EPPO (<https://gd.eppo.int/>)
    and at CABI
    (<https://www.cabidigitallibrary.org/journal/cabicompendium>)
    websites.
