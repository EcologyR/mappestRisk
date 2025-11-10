# Fit Thermal Performance Curves

Fit nonlinear regression models to data representing how development
rate changes with temperature (known as Thermal Performance Curves),
based on
[`nls.multstart::nls_multstart()`](https://rdrr.io/pkg/nls.multstart/man/nls_multstart.html)
approach to development rate data across temperatures. The fitting
procedure is built upon previous packages for starting values
estimation, namely `rTPC` and `devRate`.

## Usage

``` r
fit_devmodels(temp = NULL, dev_rate = NULL, model_name = NULL)
```

## Source

The dataset used in the example was originally published in Satar &
Yokomi (2022) under the CC-BY-NC license. The start values and equations
for the 'briere1', 'lactin1', 'mod_polynomial' and 'wang' models have
been obtained from the
[devRate](https://cran.r-project.org/package=devRate) package.

## Arguments

- temp:

  a vector of temperatures used in the experiment. It should have at
  least four different temperatures and must contain only numbers
  without any missing values.

- dev_rate:

  a vector of estimated development rates corresponding to each
  temperature. These rates are calculated as the inverse of the number
  of days to complete the transition from the beginning of a certain
  life stage to the beginning of the following at each temperature. It
  must be numeric and of the same length as `temp`.

- model_name:

  a string or a vector that specifies the model(s) to use for fitting
  the Thermal Performance Curves. Options include "all" or specific
  models listed in
  [available_models](https://ecologyr.github.io/mappestRisk/reference/available_models.md).
  These models typically exhibit a common unimodal, left-skewed shape.

## Value

A table in `tibble` format with estimates and standard errors for each
parameter of the models specified by the user that have adequately
converged. Models are sorted based on their Akaike Information Criterion
(AIC) values, with the best fitting models shown first. Fitted models
are also provided in list format in the `model_list` column and can be
accessed using
[`get_fitted_model()`](https://ecologyr.github.io/mappestRisk/reference/get_fitted_model.md)
for for further inspection. It is important to consider ecological
criteria alongside statistical information. For additional help in model
selection, we recommend using
[`plot_devmodels()`](https://ecologyr.github.io/mappestRisk/reference/plot_devmodels.md)
and consulting relevant literature.

## References

Angilletta, M.J., (2006). Estimating and comparing thermal performance
curves. *J. Therm. Biol.* 31: 541-545. (for model selection in TPC
framework)

Padfield, D., O'Sullivan, H. and Pawar, S. (2021). *rTPC* and
*nls.multstart*: A new pipeline to fit thermal performance curves in
`R`. *Methods Ecol Evol*. 12: 1138-1143.

Rebaudo, F., Struelens, Q. and Dangles, O. (2018). Modelling
temperature-dependent development rate and phenology in arthropods: The
`devRate` package for `R`. *Methods Ecol Evol*. 9: 1144-1150.

Satar, S. and Yokomi, R. (2002). Effect of temperature and host on
development of *Brachycaudus schwartzi* (Homoptera: Aphididae). *Ann.
Entomol. Soc. Am.* 95: 597-602.

## See also

[`nls.multstart::nls_multstart()`](https://rdrr.io/pkg/nls.multstart/man/nls_multstart.html)
for structure of model fitting approach

`browseVignettes("rTPC")` for model names, start values searching
workflows and bootstrapping procedures using both `rTPC` and
`nls.multstart` packages.

## Examples

``` r
data("aphid")

fitted_tpcs <- fit_devmodels(temp = aphid$temperature,
                             dev_rate = aphid$rate_value,
                             model_name = c("lactin2", "briere2",
                                            "mod_weibull")
                             )
#> fitting model lactin2
#> fitting model briere2
#> fitting model mod_weibull
head(fitted_tpcs)
#> # A tibble: 6 × 8
#>   model_name param_name start_vals param_est param_se model_AIC model_BIC
#>   <chr>      <chr>           <dbl>     <dbl>    <dbl>     <dbl>     <dbl>
#> 1 lactin2    a               0.119    0.0692   0.0463     -42.2     -42.5
#> 2 lactin2    b              -0.254   -0.283    0.402      -42.2     -42.5
#> 3 lactin2    tmax           32.5     41.3      7.75       -42.2     -42.5
#> 4 lactin2    delta_t         7.5     13.5      6.98       -42.2     -42.5
#> 5 briere2    tmin           15       10.0      2.33       -42.6     -42.9
#> 6 briere2    tmax           32.5     36.5      3.40       -42.6     -42.9
#> # ℹ 1 more variable: model_fit <list>
```
