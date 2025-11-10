# Get fitted model object

Get fitted model object

## Usage

``` r
get_fitted_model(fitted_df = NULL, model_name = NULL)
```

## Arguments

- fitted_df:

  A table with fitted models, as produced by
  [`fit_devmodels()`](https://ecologyr.github.io/mappestRisk/reference/fit_devmodels.md).

- model_name:

  Character. Name of a fitted model, see
  [available_models](https://ecologyr.github.io/mappestRisk/reference/available_models.md).

## Value

A model object

## Examples

``` r
data("aphid")

fitted_tpcs_aphid <- fit_devmodels(temp = aphid$temperature,
                                   dev_rate = aphid$rate_value,
                                   model_name = c("lactin2", "briere2", "ratkowsky")
                                   )
#> fitting model lactin2
#> fitting model briere2
#> fitting model ratkowsky
get_fitted_model(fitted_tpcs_aphid, "briere2")
#> Nonlinear regression model
#>   model: dev_rate ~ rTPC::briere2_1999(temp, tmin, tmax, a, b)
#>    data: data
#>      tmin      tmax         a         b 
#> 1.003e+01 3.651e+01 4.162e-05 1.125e+00 
#>  residual sum-of-squares: 0.0002223
#> 
#> Number of iterations to convergence: 18 
#> Achieved convergence tolerance: 1.49e-08
```
