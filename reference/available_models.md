# Available Models Table

Table containing the available models to be fit using
[`fit_devmodels()`](https://ecologyr.github.io/mappestRisk/reference/fit_devmodels.md).
These models come from two other packages:
[`devRate`](https://cran.r-project.org/package=devRate) and
[`rTPC`](https://padpadpadpad.github.io/rTPC/) .

## Usage

``` r
data("available_models")
```

## Format

### `available_models`

A data.frame/tibble with 13 rows and 6 columns:

- model_name:

  Model name to be used within
  [`fit_devmodels()`](https://ecologyr.github.io/mappestRisk/reference/fit_devmodels.md).

- package:

  names of the packages used by
  [`fit_devmodels()`](https://ecologyr.github.io/mappestRisk/reference/fit_devmodels.md)
  to obtain appropriate start values for the user-provided data. When
  the package is [rTPC package](https://padpadpadpad.github.io/rTPC/),
  start values are automatically computed with
  [`rTPC::get_start_vals()`](https://rdrr.io/pkg/rTPC/man/get_start_vals.html),
  which in turn relies on
  [`nls.multstart::nls_multstart()`](https://rdrr.io/pkg/nls.multstart/man/nls_multstart.html).
  When the package is [devRate
  package](https://github.com/frareb/devRate), iterative starting values
  are computed using
  [`nls.multstart::nls_multstart()`](https://rdrr.io/pkg/nls.multstart/man/nls_multstart.html),
  using the parameters published in `devRate::devRateEqStartVal()` as
  first attempts to iterate. As an exception, if
  `model_name == "briere1"`, generic starting values are provided and
  advised to the user due to the unrealistic value of some parameters in
  the `devRate` data set.

- source_model_name:

  name of the function in the source packages `rTPC` and `devRate`.

- formula, working_formula, n_params:

  formulas used for model fitting.

## Source

Rebaudo, F., Struelens, Q., and Dangles, O. (2018). Modelling
temperature-dependent development rate and phenology in arthropods: The
`devRate` package for R. Methods Ecol Evol. 9: 1144-1150.
[doi:10.1111/2041-210X.12935](https://doi.org/10.1111/2041-210X.12935) .

Padfield, D., O´Sullivan, H., and Pawar, S., (2021). `rTPC` and
`nls.multstart`: a new pipeline to fit thermal performance curves in R.
Methods Ecol Evol. 12: 1138-1143.
[doi:10.1111/2041-210X.13585](https://doi.org/10.1111/2041-210X.13585) .
