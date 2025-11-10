# Propagate parameter uncertainty of TPC fits using bootstrap with residual resampling

Propagate parameter uncertainty of TPC fits using bootstrap with
residual resampling

## Usage

``` r
predict_curves(
  temp = NULL,
  dev_rate = NULL,
  fitted_parameters = NULL,
  model_name_2boot = NULL,
  propagate_uncertainty = TRUE,
  n_boots_samples = 100
)
```

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

- fitted_parameters:

  a `tibble` obtained with
  [`fit_devmodels()`](https://ecologyr.github.io/mappestRisk/reference/fit_devmodels.md)
  function, including parameter names, estimates, standard errors, AICs,
  and nls objects (fitted_models) using the
  [`nls.multstart::nls_multstart()`](https://rdrr.io/pkg/nls.multstart/man/nls_multstart.html)
  approach.

- model_name_2boot:

  A vector of strings including one or several TPC models fitted by
  [`fit_devmodels()`](https://ecologyr.github.io/mappestRisk/reference/fit_devmodels.md).
  Contrarily to that function, `model_name_2boot = "all"` is not allowed
  in this function due to the slow bootstrapping procedure. We recommend
  applying this function only to a small pre-selection of models (e.g.,
  one to four) based on statistical and ecological criteria with the
  help of
  [`plot_devmodels()`](https://ecologyr.github.io/mappestRisk/reference/plot_devmodels.md)
  function.

- propagate_uncertainty:

  A logical argument that specifies whether to propagate parameter
  uncertainty by bootstrap with residual resampling. If `FALSE`, the
  function returns predictions from the fitted TPCs for the selected
  model(s). If `TRUE`, bootstrap is applied using residual resampling to
  obtain multiple TPCs as detailed in vignettes of the `rTPC` package.
  Defaults to `TRUE`.

- n_boots_samples:

  Number of bootstrap resampling iterations (default is 100). A larger
  number of iterations makes the resampling procedure more robust, but
  typically 100 is sufficient for propagating parameter uncertainty, as
  increasing `n_boots_samples` will increase computation time for
  predicting resampled TPCs.

## Value

A tibble object with as many curves (TPCs) as the number of iterations
provided in the `n_boots_samples` argument if
`propagate_uncertainty = TRUE` minus the bootstrap samples that could
not be fitted (i.e., new nonlinear regression models did not converge
for them). Otherwise, it returns just one prediction TPC from model fit
estimates. Each resampled TPC consists of a collection of predictions
for a set of temperatures from `temp - 20` to `temp + 15` with a
resolution of 0.1°C and a unique identifier called `boots_iter`. In
addition to the uncertainty TPCs, the estimated TPC is also explicit in
the output tibble.

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

`browseVignettes("rTPC")` for model names, start values searching
workflows, and bootstrapping procedures using both
[`rTPC::get_start_vals()`](https://rdrr.io/pkg/rTPC/man/get_start_vals.html)
and
[`nls.multstart::nls_multstart()`](https://rdrr.io/pkg/nls.multstart/man/nls_multstart.html)

[`fit_devmodels()`](https://ecologyr.github.io/mappestRisk/reference/fit_devmodels.md)
for fitting Thermal Performance Curves to development rate data, which
is in turn based on
[`nls.multstart::nls_multstart()`](https://rdrr.io/pkg/nls.multstart/man/nls_multstart.html).

## Examples

``` r
if (FALSE) { # interactive()
data("aphid")

fitted_tpcs <- fit_devmodels(temp = aphid$temperature,
                             dev_rate = aphid$rate_value,
                             model_name = "all")

plot_devmodels(temp = aphid$temperature,
               dev_rate = aphid$rate_value,
               fitted_parameters = fitted_tpcs,
               species = "Brachycaudus schwartzi",
               life_stage = "Nymphs")

boot_tpcs <- predict_curves(temp = aphid$temperature,
                            dev_rate = aphid$rate_value,
                            fitted_parameters = fitted_tpcs,
                            model_name_2boot = c("lactin2", "briere2", "beta"),
                            propagate_uncertainty = TRUE,
                            n_boots_samples = 10)

head(boot_tpcs)
}
```
