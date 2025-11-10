# Map pest risk

This function produces a raster map where each pixel shows the number of
months per year in which temperature is within a given set of bounds. If
the input has several pairs of minimum and maximum temperatures (as
produced by
[`therm_suit_bounds()`](https://ecologyr.github.io/mappestRisk/reference/therm_suit_bounds.md)),
the output raster has two layers: mean and standard deviation.

## Usage

``` r
map_risk(
  t_vals = NULL,
  t_rast = NULL,
  region = NULL,
  res = 2.5,
  path = NULL,
  mask = TRUE,
  verbose = FALSE,
  plot = TRUE,
  interactive = FALSE
)
```

## Arguments

- t_vals:

  a `data.frame` or
  [`dplyr::tibble()`](https://dplyr.tidyverse.org/reference/reexports.html)
  as produced by
  [`therm_suit_bounds()`](https://ecologyr.github.io/mappestRisk/reference/therm_suit_bounds.md).
  `t_vals` must contain results derived from a single model. It must
  contain at least one row of numeric values. Additionally, the minimum
  ("left") thermal boundary or `tval_left` must be lower than the
  maximum ("right") one, or `tval_right` for all rows. Nominative
  columns must be present in the input (i.e., `model_name`,
  `suitability`, `pred_suit` and `iter`).

- t_rast:

  Optional 12-layer
  [`terra::SpatRaster()`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with monthly mean temperatures for the region of interest. If not
  provided, global WorldClim raster layers will be automatically
  (down)loaded using
  [`geodata::worldclim_global()`](https://rdrr.io/pkg/geodata/man/worldclim.html),
  and cropped to `region` (if provided). Note that the download can be
  slow the first time you use the function in a new `path`. If you get a
  download error, consider running e.g `options(timeout = 500)` (or
  more).

- region:

  Optional object specifying the region to map. Must overlap the extent
  of `t_rast` if both are provided. Can be a
  [`terra::SpatVector()`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
  polygon (obtained with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html));
  or an `sf` polygon, in which case it will be coerced with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html))
  to a
  [`terra::SpatVector()`](https://rspatial.github.io/terra/reference/SpatVector-class.html);
  or a character vector of country name(s) in English (see
  [country_names](https://ecologyr.github.io/mappestRisk/reference/country_names.md)),
  in which case climate maps will be downloaded for those countries; or
  a
  [`terra::SpatExtent()`](https://rspatial.github.io/terra/reference/SpatExtent-class.html)
  object (obtained with
  [`terra::ext()`](https://rspatial.github.io/terra/reference/ext.html));
  or a numeric vector of length 4 specifying the region coordinates as
  follows: `c(xmin, xmax, ymin, ymax)`. The latter two must be in the
  same CRS as`t_rast` if `t_rast` is provided, or in unprojected lon-lat
  coordinates (WGS84, EPSG:4326) otherwise. If NULL, the output maps
  will cover the entire `t_rast` if provided, or the entire world
  otherwise.

- res:

  Argument to pass to
  [`geodata::worldclim_global()`](https://rdrr.io/pkg/geodata/man/worldclim.html)
  specifying the spatial resolution for the raster maps to download, if
  `t_rast` is not provided. The default is 2.5 arc-minutes. Beware that
  lower values (e.g., 0.5) may lead to extremely heavy data sets and
  large computation times.

- path:

  Argument to pass to
  [`geodata::worldclim_global()`](https://rdrr.io/pkg/geodata/man/worldclim.html)
  (if `t_rast` is not provided) and/or to
  [`geodata::world()`](https://rdrr.io/pkg/geodata/man/world.html) (if
  `region` is a vector of country names) specifying the folder path for
  the downloaded maps.

- mask:

  Logical value to pass to
  [`terra::mask()`](https://rspatial.github.io/terra/reference/mask.html)
  specifying whether the output raster maps should be masked with the
  borders of the target 'region', if this is a polygon map or a vector
  of country names. The default is TRUE. If FALSE, the entire
  rectangular extent of 'region' will be used.

- verbose:

  Logical value specifying whether to display messages about what the
  function is doing at possibly slow steps. The default is FALSE.
  Setting it to TRUE can be useful for checking progress when maps are
  large.

- plot:

  Logical value specifying whether to plot the results in a map.
  Defaults to TRUE. Note that the function will always return a
  [`terra::SpatRaster()`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object even if `plot = TRUE`.

- interactive:

  Logical value specifying whether the plotted map should be interactive
  (if plot=TRUE). The default is TRUE if the 'leaflet' package is
  installed.

## Value

This function returns a
[`terra::SpatRaster()`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with up to 2 layers: the ([`mean()`](https://rdrr.io/r/base/mean.html))
number of months with temperature within the species' thermal bounds;
and (if `t_vals` has \>1 rows) the standard deviation
([`stats::sd()`](https://rdrr.io/r/stats/sd.html)) around that mean.

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

risk_map_reunion <- map_risk(t_vals = boundaries,
                             path = tempdir(),
                             region = "Réunion",
                             mask = TRUE,
                             plot = TRUE,
                             interactive = FALSE,
                             verbose = TRUE)
}
```
