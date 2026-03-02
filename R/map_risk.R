#' Map pest risk
#'
#' This function produces a raster map where each pixel shows the number of
#' months per year in which temperature is within a given set of bounds. If
#' the input has several pairs of minimum and maximum temperatures (as
#' produced by [therm_suit_bounds()]), the output raster has two layers: mean
#' and standard deviation.
#'
#' @param t_vals a `data.frame` or [dplyr::tibble()] as produced by [therm_suit_bounds()].
#' `t_vals` must contain results derived from a single model.
#' It must contain at least one row of numeric values.
#' Additionally, the minimum ("left") thermal boundary or `tval_left`
#' must be lower than the maximum ("right") one, or `tval_right` for all rows.
#' Nominative columns must be present in the input (i.e., `model_name`,
#' `suitability`, `pred_suit` and `iter`).
#'
#' @param t_rast Optional 12-layer [terra::SpatRaster()] with monthly mean
#' temperatures for the region of interest. If not provided, global
#' WorldClim raster layers will be automatically (down)loaded
#' using [geodata::worldclim_global()], and cropped to `region` (if provided).
#' Note that the download can be slow the first time you use the function in a new
#' `path`. If you get a download error, consider running e.g `options(timeout = 500)` (or more).
#'
#' @param region Optional object specifying the region to map. Must overlap the
#' extent of `t_rast` if both are provided. Can be a [terra::SpatVector()]
#' polygon (obtained with [terra::vect()]); or an `sf` polygon,
#' in which case it will be coerced with [terra::vect()]) to a [terra::SpatVector()];
#' or a character vector of country
#' name(s) in English (see [country_names]), in which case climate maps will be
#' downloaded for those countries; or a [terra::SpatExtent()] object (obtained with
#' [terra::ext()]); or a numeric vector of length 4 specifying the region
#' coordinates as follows: `c(xmin, xmax, ymin, ymax)`. The latter two must be in
#' the same CRS as`t_rast` if `t_rast` is provided, or in unprojected lon-lat
#' coordinates (WGS84, EPSG:4326) otherwise. If NULL, the output maps will
#' cover the entire `t_rast` if provided, or the entire world otherwise.
#'
#' @param res Argument to pass to [geodata::worldclim_global()] specifying
#' the spatial resolution for the raster maps to download, if `t_rast` is not
#' provided. The default is 2.5 arc-minutes. Beware that lower values (e.g., 0.5)
#' may lead to extremely heavy data sets and large computation times.
#'
#' @param path Argument to pass to [geodata::worldclim_global()] (if `t_rast`
#' is not provided) and/or to [geodata::world()] (if `region` is a vector of
#' country names) specifying the folder path for the downloaded maps.
#'
#' @param mask Logical value to pass to [terra::mask()] specifying whether the
#' output raster maps should be masked with the borders of the target 'region',
#' if this is a polygon map or a vector of country names. The default is TRUE.
#' If FALSE, the entire rectangular extent of 'region' will be used.
#'
#' @param verbose Logical value specifying whether to display messages about
#' what the function is doing at possibly slow steps. The default is FALSE.
#' Setting it to TRUE can be useful for checking progress when maps are large.
#'
#' @param plot Logical value specifying whether to plot the results in a map.
#' Defaults to TRUE. Note that the function will always return a [terra::SpatRaster()]
#' object even if `plot = TRUE`.
#'
#' @param interactive Logical value specifying whether the plotted map should be
#'  interactive (if plot=TRUE). The default is TRUE if the 'leaflet' package is installed.
#'
#' @return This function returns a [terra::SpatRaster()] with up to 2 layers:
#' the ([mean()]) number of months with temperature within the species' thermal
#' bounds; and (if `t_vals` has >1 rows) the standard deviation ([stats::sd()]) around
#' that mean.
#'
#'
#' @export
#'
#' @examplesIf interactive()
#' data("aphid")
#'
#' fitted_tpcs <- fit_devmodels(temp = aphid$temperature,
#'                              dev_rate = aphid$rate_value,
#'                              model_name = "all")
#'
#' plot_devmodels(temp = aphid$temperature,
#'                dev_rate = aphid$rate_value,
#'                fitted_parameters = fitted_tpcs,
#'                species = "Brachycaudus schwartzi",
#'                life_stage = "Nymphs")
#'
#' boot_tpcs <- predict_curves(temp = aphid$temperature,
#'                             dev_rate = aphid$rate_value,
#'                             fitted_parameters = fitted_tpcs,
#'                             model_name_2boot = c("lactin2", "briere2", "beta"),
#'                             propagate_uncertainty = TRUE,
#'                             n_boots_samples = 10)
#'
#' print(boot_tpcs)
#'
#' plot_uncertainties(temp = aphid$temperature,
#'                    dev_rate = aphid$rate_value,
#'                    bootstrap_tpcs = boot_tpcs,
#'                    species = "Brachycaudus schwartzi",
#'                    life_stage = "Nymphs")
#'
#'
#' boundaries <- therm_suit_bounds(preds_tbl = boot_tpcs,
#'                                 model_name = "lactin2",
#'                                 suitability_threshold = 80)
#'
#' risk_map_reunion <- map_risk(t_vals = boundaries,
#'                              path = tempdir(),
#'                              region = "Réunion",
#'                              mask = TRUE,
#'                              plot = TRUE,
#'                              interactive = FALSE,
#'                              verbose = TRUE)

map_risk <- function(t_vals = NULL,
                     t_rast = NULL,
                     region = NULL,
                     res = 2.5,
                     path = NULL,
                     mask = TRUE,
                     verbose = FALSE,
                     plot = TRUE,
                     interactive = FALSE) {

  ## Checks

  if (!is.logical(mask)) {
    stop("`mask` must be logical (`TRUE` or `FALSE`). Defaults to `TRUE`.")
  }
  if (!is.logical(verbose)) {
    stop("`verbose` must be logical (`TRUE` or `FALSE`). Defaults to `FALSE`.")
  }
  if (!is.logical(plot)) {
    stop("`plot` must be logical (`TRUE` or `FALSE`). Defaults to `TRUE`.")
  }
  if (!is.logical(interactive)) {
    stop("`interactive` must be logical (`TRUE` or `FALSE`). Defaults to `FALSE`.")
  }

  stopifnot(is.numeric(res) & length(res) == 1)
  stopifnot(res %in% c(10, 5, 2.5, 0.5))
  stopifnot(is.null(path) | is.character(path))

  if (!inherits(t_vals, "data.frame") |
      !all(c("model_name", "tval_left", "tval_right", "pred_suit") %in% names(t_vals))) {
    stop("The argument `t_vals` must be a tibble or data.frame as produced by
    `mappestRisk::therm_suit_bounds()`. Column modifications are not allowed
    to ensure a continuous workflow of the package functions")
  }

  if (length(unique(t_vals$model_name)) > 1) {
    stop("`t_vals` must contain results from only one model. Please filter the dataset to choose a single model.")
  }

  if (any(t_vals$tval_left > t_vals$tval_right, na.rm = TRUE)) {
    stop("All 'tval_left' values must be less than or equal to 'tval_right'.")
  }

  if (is.null(region) && is.null(t_rast)) {
    message("Mapping the entire world as neither `region` nor `t_rast` are provided.")
    # stop("`region` must be defined by the user. You can use a name from
    #`country_names` or
    #      input your own spatial feature object or your own extent")
  }

  if (is.null(t_rast) && is.null(path)) {
    stop("Please provide an existing 'path' to save the downloaded temperature maps.")
  }

  if (!is.null(t_rast)) {
    stopifnot(inherits(t_rast, "SpatRaster"))
    stopifnot(length(t_rast) == 1)
    stopifnot(terra::nlyr(t_rast) == 12)
  }

  ## Check that country names are correct
  if (is.character(region)) {
    regions_not_found <- setdiff(region, country_names)
    if (length(regions_not_found) > 0) {
      stop("Input region(s) ", paste(regions_not_found, collapse = ", "), " not found. For available region names, run 'data(country_names); country_names'.")
    }
  }

  if (is.numeric(region) && length(region) != 4) {
    stop("When provided as a numeric vector, 'region' must have length 4 (xmin, xmax, ymin, ymax).")
  }

  ## End checks ##


  t_vals <- t_vals |>
    dplyr::select(tval_left, tval_right) |>
    tidyr::drop_na()


  ## Make region always a SpatVector

  # If region is a vector of country names
  if (is.character(region)) {
    if (verbose) message("\n(Down)loading countries map...\n")
    wrld <- geodata::world(path = path)
    country <- region  # store country names here
    region <- wrld[wrld$NAME_0 %in% region, ]  # a SpatVector
  }

  if (is.numeric(region)) {
    region <- terra::vect(terra::ext(region))
    mask <- FALSE
  }

  if (inherits(region, "sf")) {
    region <- terra::vect(region)
  }


  ## Get t_rast if not provided

  if (is.null(t_rast)) {

    if (verbose) message("\n(Down)loading temperature rasters...\n")

    # If a single country, use geodata::worldclim_country, else download
    # entire world using geodata::worldclim_global and crop afterwards
    if (exists("country") && length(country) == 1) {
      t_rast <- geodata::worldclim_country(country = country,
                                           var = "tavg",
                                           path = path)
    } else {
      t_rast <- geodata::worldclim_global(var = "tavg",
                                          res = res,
                                          path = path)
    }
  }

  # if data download from geodata has failed (e.g. no connection), return error
  if (is.null(t_rast)) {
    stop("The download of temperature rasters has failed. Please try later or provide a raster in `t_rast`")
  }


  ## Crop and mask t_rast with region

  # If no region provided, use entire t_rast extent
  if (is.null(region)) {
    region <- terra::vect(terra::ext(t_rast), crs = terra::crs(t_rast))
    mask <- FALSE
  }

  # If region does not have a CRS, assume same CRS as t_rast
  # SpatExtent do not have CRS
  if (terra::crs(region) == "" && !inherits(region, "SpatExtent")) {
    if (verbose) message("Assuming that 'region' has same CRS as 't_rast'")
    terra::crs(region) <- terra::crs(t_rast)
  }

  # If region and t_rast do not have same CRS, project region to t_rast CRS
  if (!inherits(region, "SpatExtent") && !terra::same.crs(t_rast, region)) {
    if (verbose) message("\nProjecting 'region' to 't_rast'...\n")
    region <- terra::project(region, t_rast)
  }

  if (is.null(terra::intersect(terra::ext(region), terra::ext(t_rast)))) {
    stop("There's no overlap between 'region' and 't_rast'.")
  }

  if (terra::ext(region) < terra::ext(t_rast)) {
    if (verbose) message("\nCropping temperature rasters to region...\n")
    t_rast <- terra::crop(t_rast, region)
  }

  if (isTRUE(mask)) {
    if (verbose) message("\nMasking temperature rasters with region...\n")
    t_rast <- terra::mask(t_rast, region)
  }


  ## Calculating rasters

  if (verbose) message("\nComputing summary layers...\n")

  t_rast_sum <- terra::rast(t_rast, nlyrs = nrow(t_vals))

  for (r in seq_len(nrow(t_vals))) {
    tvals <- unlist(t_vals[r, ])
    if (!all(is.finite(tvals))) next
    # Produce rasters with TRUE values if temp within bounds, FALSE otherwise
    inside <- t_rast >= tvals[1] & t_rast <= tvals[2]
    # sum layers to get number of months
    t_rast_sum[[r]] <- terra::app(inside, "sum", na.rm = TRUE)
  }

  # nulls <- which(sapply(t_rast_sum, is.null))
  # if (length(nulls) > 0)  t_rast_sum <- t_rast_sum[-nulls]

  # If t_vals contains bootstrap iterations, calculate mean and sd
  if (nrow(t_vals) > 1) {  # same as if(terra::nlyr(t_rast_sum) > 1)
    out <- c(terra::app(t_rast_sum, "mean", na.rm = TRUE),
             terra::app(t_rast_sum, "sd", na.rm = TRUE))
  } else {
    out <- t_rast_sum
    names(out) <- "mean"
  }



  ## Plot

  if (isTRUE(plot)) {

    if (verbose) message("\nPlotting map...\n")
    palette_bilbao <- khroma::color(palette = "bilbao",reverse = TRUE)(100)
    palette_acton <- khroma::color(palette = "acton",reverse = TRUE)(100)

    if (isTRUE(interactive)) {

      outmap <- terra::plet(out, y = names(out), collapse = FALSE,
                            legend = "bottomleft",
                            col = c(palette_bilbao),
                            tiles = "Stadia.AlidadeSmoothDark",
                            alpha = .95)
      print(outmap)

    } else {

      out_mean <- out["mean"]

      op <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(op), add = TRUE)

      if (nrow(t_vals) > 1) {

        sd_mean <- out["sd"]

        graphics::par(mfrow = c(1, 2))
        terra::plot(out_mean,
                    col = palette_bilbao[5:100],
                    main = "Risk Map",
                    colNA = "white")

        terra::plot(sd_mean,
                    col = palette_acton,
                    main = "Uncertainty Map",
                    colNA = "white")

      } else {
        graphics::par(mfrow = c(1, 1))
        terra::plot(out_mean,
                    col = palette_bilbao[5:100],
                    main = "Risk Map",
                    colNA = "white")
      }
    }
  }

  if (verbose) message("\nFinished!\n\n")

  return(out)  # return SpatRaster always, even if plotting leaflet map

}
