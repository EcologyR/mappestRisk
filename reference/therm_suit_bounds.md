# Determine Thermal Boundaries for Optimal Performance Level

Calculate thermal boundaries that define the suitable region of a
Thermal Performance Curve (TPC) corresponding to a user-defined optimal
performance level.

## Usage

``` r
therm_suit_bounds(
  preds_tbl = NULL,
  model_name = NULL,
  suitability_threshold = NULL
)
```

## Arguments

- preds_tbl:

  a `tibble` object as produced by
  [`predict_curves()`](https://ecologyr.github.io/mappestRisk/reference/predict_curves.md).

- model_name:

  character. Name of one or several of the TPC models fitted first in
  [`fit_devmodels()`](https://ecologyr.github.io/mappestRisk/reference/fit_devmodels.md)
  and predicted next in
  [`predict_curves()`](https://ecologyr.github.io/mappestRisk/reference/predict_curves.md).
  If using `model_name = "all"` all models contained in `preds_tbl` will
  be used. Please, note that some models (most typically, `briere`,
  `mod_poly`, `wang` and `ratkowsky`) may calculate unrealistic thermal
  boundaries. We recommend to double-check it and compare among several
  models.

- suitability_threshold:

  A numeric value from 50 to 100 representing the quantile of the curve
  that provides the user-defined optimal performance. For instance,
  setting `suitability_threshold` to 80 identifies the top 20% (or
  quantile 80) of the maximum values of the development rate predicted
  by the chosen TPC model. If `suitability_threshold` equals 100, the
  function returns the optimum temperature for development rate.

## Value

A tibble with six columns:

- `model_name`: A string indicating the selected TPC model used for
  projections.

- `suitability`: A string indicating the suitability threshold in
  percentage (see `suitability_threshold`).

- `tval_left`: A number representing the lower thermal boundary
  delimiting the suitable region of the TPC.

- `tval_right`: A number representing the upper thermal boundary
  delimiting the suitable region of the TPC.

- `pred_suit`: A number corresponding to the predicted development rate
  value determining the chosen quantile threshold of the maximum rate
  (i.e., suitability percentage of maximum rate).

- `iter`: A string determining the TPC identity from the bootstrapping
  procedure in
  [`predict_curves()`](https://ecologyr.github.io/mappestRisk/reference/predict_curves.md)
  function, or `estimate` when it represents the estimated TPC fitted in
  [`fit_devmodels()`](https://ecologyr.github.io/mappestRisk/reference/fit_devmodels.md).

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
[`predict_curves()`](https://ecologyr.github.io/mappestRisk/reference/predict_curves.md)
for bootstrapping procedure based on the above-mentioned `rTPC`
vignettes.

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

print(boot_tpcs)


plot_uncertainties(temp = aphid$temperature,
                   dev_rate = aphid$rate_value,
                   bootstrap_tpcs = boot_tpcs,
                   species = "Brachycaudus schwartzi",
                   life_stage = "Nymphs")


boundaries <- therm_suit_bounds(preds_tbl = boot_tpcs,
                                model_name = "lactin2",
                                suitability_threshold = 80)
head(boundaries)
}
```
