#' Map pest risk
#'
#' This function produces a raster map where each pixel shows the number of
#' months per year in which temperature is within a given set of bounds. If
#' the input has several pairs of minimum and maximum temperatures (e.g. as
#' produced by [therm_suit_bounds]), the output raster has two layers: mean
#' and standard deviation.
#'
#' @param t_vals a `data.frame` or [dplyr::tibble] inherited from `therm_suit_bounds()`
#' function without any further modification. It must contain at least one row
#' of numeric values. Additionally, the minimum ("left") thermal boundary or `tval_left`
#' must be lower than the maximum ("right") one, or `tval_right` for all rows.
#' Nominative columns must be present in the input (i.e., `model_name`,
#' `suitability`, `pred_suit` and `iter`).
#'
#' @param t_rast Optional 12-layer [terra::SpatRaster] with monthly mean
#' temperatures for (at least) the target 'region'. If not provided, global
#' WorldClim raster layers will be automatically (down)loaded and cropped to
#' 'region' using [geodata::worldclim_global()].
#' Note that the download can be slow the first time you use the function in a new
#' `path`. If you get a download error, consider running e.g [options(timeout = 500)] (or more).
#'
#' @param region Mandatory object specifying the region to map. Must overlap the
#' extent of `t_rast` if both are provided. Can be a [terra::SpatVector]
#' polygon map (obtained with [terra::vect()]); or an [sf::sf] polygon map
#' (obtained with [sf::st_as_sf()]), in which case it will be coerced with
#' [terra::vect()]) to a [terra::SpatVector]; or a character vector of country
#' name(s) in English, in which case a countries map will be downloaded and
#' subset to those countries; or a [terra::SpatExtent] object (obtained with
#' [terra::ext()]); or a numeric vector of length 4 specifying the region
#' coordinates as folows: `c(xmin, xmax, ymin, ymax)`. The latter two must be in
#' the same CRS as`t_rast` if `t_rast` is provided, or in unprojected lon-lat
#' coordinates (WGS84, EPSG:4326) otherwise. If NULL, the output maps will
#' cover the entire `t_rast` if provided, or the entire world otherwise.
#'
#' @param res Argument to pass to [geodata::worldclim_global()] specifying
#' the spatial resolution for the raster maps to download, if 't_rast' is not
#' provided. The default is 2.5 arc-minutes. Beware that lower values (e.g., 0.5)
#' may lead to extremely heavy data sets and large computation times.
#'
#' @param path Argument to pass to [geodata::worldclim_global()] (if `t_rast`
#' is not provided) and/or to [geodata::world()] (if 'region' is a vector of
#' country names) specifying the folder path for the downloaded maps.
#' @param mask Logical value to pass to [terra::crop()] specifying whether the
#' output raster maps should be masked with the borders of the target 'region',
#' if this is a polygon map or a vector of country names. The default is TRUE.
#' If FALSE, the entire rectangular extent of 'region' will be used.
#'
#' @param verbose Logical value specifying whether to display messages about
#' what the function is doing at possibly slow steps. The default is FALSE.
#' Setting it to TRUE can be useful for checking progress when maps are large.
#'
#' @param plot Logical value specifying whether to plot the results in a map.
#' Defaults to TRUE. Note that the function will always return a [terra::SpatRaster]
#' object even if `plot = TRUE`.
#'
#' @param interactive Logical value specifying whether the plotted map should be
#'  interactive (if plot=TRUE). The default is TRUE if the 'leaflet' package is installed.
#'
#' @return This function returns a [terra::SpatRaster] with up to 2 layers:
#' the ([mean]) number of months with temperature within the species' thermal
#' bounds; and (if `t_vals` has >1 rows) the standard deviation ([sd]) around
#' that mean.
#'
#' @import terra
#'
#' @export
#'
#' @examples
#' # if you have temperature rasters for your region:
#'
#' tavg_file <- system.file("extdata/tavg_lux.tif", package = "mappestRisk")
#' tavg_rast <- terra::rast(tavg_file)
#' terra:::plot(tavg_rast)
#'
#' therm_bounds <- data.frame(tval_left = c(12.5, 12.1, 13.2),
#'                            tval_right = c(21.4, 21.2,  22.0))
#' therm_bounds  # normally you would use the output of `therm_suit_bounds()`
#'
#' risk_rast <- map_risk(t_vals = therm_bounds, t_rast = tavg_rast)
#'
#' risk_rast <- map_risk(t_vals = therm_bounds, t_rast = tavg_rast, interactive = FALSE)
#'
#'
#'
#' \dontrun{
#' # if you don't have temperature rasters for your region:
#'
#' risk_rast <- map_risk(t_vals = therm_bounds, path = "downloaded_maps",
#' region = c("Portugal", "Spain"), verbose = TRUE)
#'
#'
#' # to save output map(s) to disk:
#'
#' terra::writeRaster(risk_rast, "risk_map.tif")  # exported maps can
#' be seen with GIS software
#' }
#'

map_risk <- function(t_vals = NULL,
                     t_rast = NULL,
                     region = NULL,
                     res = 2.5,
                     path = NULL,
                     mask = TRUE,
                     verbose = FALSE,
                     plot = TRUE,
                     interactive = FALSE
                     ) {

  if (class(mask) != "logical" ) {
    stop("`mask` must be logical (`TRUE` or `FALSE`). Defaults to `TRUE`.")
  }
  if (class(verbose) != "logical" ) {
    stop("`verbose` must be logical (`TRUE` or `FALSE`). Defaults to `TRUE`.")
  }
  if (class(plot) != "logical" ) {
    stop("`plot` must be logical (`TRUE` or `FALSE`). Defaults to `TRUE`.")
  }
  if (class(interactive) != "logical" ) {
    stop("`interactive` must be logical (`TRUE` or `FALSE`). Defaults to `TRUE`.")
  }
  if (!any(class(t_vals) == "data.frame")){
    stop("The argument `t_vals` must be a tibble or data.frame inherited
from the output of `mappestRisk::therm_suit_bounds()` function.
No modifications of columns of the `t_vals` data.frame are allowed in order
to ensure a continuous workflow of the package functions")
  }
  if (is.null(region) && is.null(t_rast)) {
    stop("`region` must be defined by the user. You can use a name from `country_names` or
         input your own spatial feature object or your own extent")
  }

  if (suppressWarnings(any(!c("model_name", "suitability", "tval_left",
                              "tval_right", "pred_suit", "iter") %in% names(t_vals)))){
    stop("The argument `t_vals` must be a tibble or data.frame inherited
         from the output of `mappestRisk::therm_suit_bounds()` function.
         No modifications of columns of the `t_vals` data.frame are allowed in order
         to ensure a continuous workflow of the package functions")
  }

  t_vals <- t_vals |>
    dplyr::select(tval_left, tval_right) |>
    tidyr::drop_na()

  stopifnot(all(na.omit(t_vals)[ , 2] >= na.omit(t_vals)[ , 1]))

  if (!is.null(t_rast)) {
    stopifnot(inherits(t_rast, "SpatRaster"))
    stopifnot(length(t_rast) == 1)
    stopifnot(terra::nlyr(t_rast) == 12)
  }

  if (is.null(path)) {
    if (is.null(t_rast)) {
      stop("Please provide an existing 'path' to save the downloaded temperature maps.")
    }
    if (is.character(region)) {
      stop("Please provide a 'path' to save the downloaded region maps.")
    }
  }

  if (is.character(region)) {
    regions_not_found <- setdiff(region, country_names)
    if (length(regions_not_found) > 0) {
      stop(paste("Input region(s)", paste(regions_not_found, collapse = ", "), "not found. For available region names, run 'data(country_names); country_names'."))
    }
    if (verbose) cat("\n(Down)loading countries map...\n")
    wrld <- geodata::world(path = path)
    region <- wrld[wrld$NAME_0 %in% region, ]
    if (isFALSE(mask)) {
      region <- terra::ext(region)
    }
  }

  if (is.numeric(region)) {
    if (length(region) != 4) {
      stop("When provided as a numeric vector, 'region' must have length 4 (xmin, xmax, ymin, ymax) in unprojected geographic coordinates (EPSG:4326).")
    }
    region <- terra::ext(region)
  }

  if (methods::is(region, "sf")) {
    region <- terra::vect(region)
  }

  if (is.null(t_rast)) {
    if (verbose) cat("\n(Down)loading temperature rasters...\n")
    t_rast <- geodata::worldclim_global(var = "tavg",
                                        res = res,
                                        path = path)
  }

  if (is.null(region)) {
    region <- terra::vect(terra::ext(t_rast),
                          crs = terra::crs(t_rast))
  }

  if (is(region, "SpatExtent")) {
    mask <- FALSE  # pointless otherwise
    region <- terra::vect(region, crs = "EPSG:4326")  # needed for checking CRS match with 't_rast' below; input extents are required to be in this EPSG
  }

  if (isFALSE(terra::same.crs(t_rast, region))) {
    if (verbose) cat("\nProjecting 'region' to 't_rast'...\n")
    region <- terra::project(region, t_rast)
  }

  if (is.null(terra::intersect(terra::ext(region), terra::ext(t_rast)))) {
    stop("There's no overlap between 'region' and 't_rast'.")
  }

  if (terra::ext(region) < terra::ext(t_rast)) {
    if (verbose) cat("\nCropping temperature rasters to region...\n")
    t_rast <- terra::crop(t_rast,
                          region,
                          mask = mask)
  }

  if (verbose) cat("\nComputing summary layers...\n")
  t_rast_sum <- terra::rast(t_rast, nlyrs = nrow(t_vals))
  for (r in 1:nrow(t_vals)) {
    tvals <- unlist(t_vals[r, ])
    if (!all(is.finite(tvals))) next
    outside <- t_rast < tvals[1] | t_rast > tvals[2]
    t_rast_binary <- t_rast
    t_rast_binary[outside] <- 0
    t_rast_binary[!outside] <- 1
    t_rast_sum[[r]] <- terra::app(t_rast_binary, "sum", na.rm = TRUE)
  }

  # nulls <- which(sapply(t_rast_sum, is.null))
  # if (length(nulls) > 0)  t_rast_sum <- t_rast_sum[-nulls]

  if (nrow(t_vals) > 1) {  # same as if(terra::nlyr(t_rast_sum) > 1)
    out <- c(terra::app(t_rast_sum, "mean", na.rm = TRUE),
             terra::app(t_rast_sum, "sd", na.rm = TRUE))
  } else {
    out <- t_rast_sum
    names(out) <- "mean"
  }

  if (plot) {
    if (verbose) cat("\nPlotting map...\n")
    palette_bilbao <- khroma::color(palette = "bilbao",reverse = F)(100)
    palette_acton <- khroma::color(palette = "acton",reverse = T)(100)

    if (interactive && !requireNamespace("leaflet", quietly = TRUE)) {
      if (verbose) message("'interactive' requires having the 'leaflet' package installed. Plotting a static map instead.")
      interactive <- FALSE
    }

    if (interactive) {
      outmap <- terra::plet(out, y = names(out), collapse = FALSE,
                            legend = "bottomleft",
                            col = c(palette_bilbao),
                            tiles = "Stadia.AlidadeSmoothDark",
                            alpha =.95)
      print(outmap)
      return(outmap)

    } else {
      out_mean <- out["mean"]
      if(nrow(t_vals) > 1) {
        sd_mean <- out["sd"]
        par(mfrow = c(1, 2))
        terra::plot(out_mean,
                    col = c(palette_bilbao)[5:100],
                    main = "Risk Map",
                    colNA = "white")

        terra::plot(sd_mean,
                    col = c(palette_acton),
                    main = "Uncertainty Map",
                    colNA = "white")
      } else {
        par(mfrow = c(1, 1))
        terra::plot(out_mean,
                    col = c(palette_bilbao)[5:100],
                    main = "Risk Map",
                    colNA = "white")
        }
     }
    return(out)
    }
  if (verbose) cat("\nFinished!\n\n")

}

