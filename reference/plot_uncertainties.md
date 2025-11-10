# Draw bootstrapped Thermal Performance Curves (TPCs) to visualize uncertainty in parameter estimation of TPC fitting

Draw bootstrapped Thermal Performance Curves (TPCs) to visualize
uncertainty in parameter estimation of TPC fitting

## Usage

``` r
plot_uncertainties(
  temp = NULL,
  dev_rate = NULL,
  bootstrap_tpcs = NULL,
  species = NULL,
  life_stage = NULL,
  alpha = 0.2
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

- bootstrap_tpcs:

  a `tibble` A tibble object as produced by
  [`predict_curves()`](https://ecologyr.github.io/mappestRisk/reference/predict_curves.md),
  containing bootstrapped TPCs to propagate uncertainty.

- species:

  optional a string of the target species that will constitute the plot
  title. Must be of type "character".

- life_stage:

  optional a string of the target life stage that will constitute the
  plot subtitle. Must be of type "character".

- alpha:

  a number between 0 and 1 to choose transparency of the bootstrapped
  curves (0 = complete transparency, 1 = solid line).

## Value

A ggplot object containing the visual representation of the estimate TPC
and the bootstrapped uncertainty curves as a ribbon. Each model is
represented in a facet, and data points are also explicit.

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
               species = "Brachycaudus swartzi",
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
}
```
