#' Map pest risk
#'
#' @param t_vals Numeric vector of length 2 specifying the left (minimum) and
#' right (maximum) temperature bounds for optimal performance of the target species.
#' @param t_rast Optional 12-layer SpatRaster with monthly mean temperatures
#' for (at least) the target 'region'. If not provided, global WorldClim raster
#'  layers will be (down)loaded and cropped to 'region'.
#' @param region Optional object specifying the region to map.
#' Can be a 'SpatVector' polygon map (obtained with [terra::vect()]);
#' or a 'SpatExtent' object (obtained with [terra::ext()]),
#' or a numeric vector of length 4 specifying the region coordinates
#' in the order xmin, xmax, ymin, ymax;
#' or a character vector of country name(s) in English.
#' If NULL, the output maps will cover the entire `t_rast` if provided,
#' or the entire world otherwise.
#' @param res Argument to pass to [geodata::worldclim_global()] specifying
#' the spatial resolution for the raster maps to download, if 't_rast' is not provided.
#' The default is 2.5 arc-minutes.
#' @param path Argument to pass to [geodata::worldclim_global()] (if `t_rast` is not provided)
#' and/or to [geodata::world()] (if 'region' is a vector of country names)
#' specifying the folder path for the downloaded maps.
#' @param mask Logical value to pass to [terra::crop()] specifying whether
#' the output raster maps should be masked with the borders of the target 'region',
#' if this is a polygon map or a vector of country names. The default is TRUE.
#' If FALSE, the entire rectangular extent of 'region' will be used.
#'
#' @return This function returns a SpatRaster object with 13 raster layers
#' of temperature (one for each month, and a final one with the mean value across months),
#' with values only within the bounds of the input 't_vals'.
#' @export
#'
#' @examples
#' risk_rasters <- map_risk(t_vals = c(12.5, 21.4), region = "Spain")
#' terra::plot(risk_rasters)
#' terra::plot(risk_rasters[[13]])
#' # terra::writeRaster(risk_rasters[[13]], "risk_mean.tif")
#'
map_risk <- function(t_vals = NULL,
                     t_rast = NULL,
                     region = NULL,
                     res = 2.5,
                     path = NULL,
                     mask = TRUE
                     ) {

  stopifnot(is.numeric(t_vals), length(t_vals) == 2, t_vals[2] > t_vals[1])

  if (!is.null(t_rast)) {
    stopifnot(inherits(t_rast, "SpatRaster"))
    stopifnot(terra::nlyr(t_rast) == 12)
  }

  if (is.null(t_rast) & is.null(path)) {
    stop("Please provide a path to download the temperature rasters.")
  }

  if (is.null(region)) {
    region <- terra::ext()  # whole world
    mask <- FALSE  # pointless otherwise
  }

  if (is.numeric(region)) {
    if (length(region != 4)) {
      stop("When provided as a numeric vector, 'region' must have length 4 (xmin, xmax, ymin, ymax).")
    }
    region <- terra::ext(region)
    mask <- FALSE  # pointless otherwise
  }

  if (is.character(region)) {
    regions_not_found <- setdiff(region, country_names)
    if (length(regions_not_found) > 0) {
      stop(paste("Input region(s)", paste(regions_not_found, collapse = ", "), "not found. For available region names, run 'data(country_names); country_names'."))
    }
    wrld <- geodata::world(path = path)
    region <- subset(wrld, wrld$NAME_0 %in% region)
  }

  tavg <- t_rast     # Why not use t_rast directly? No need to duplicate object?

  if (is.null(tavg)) {
    tavg <- geodata::worldclim_global(var = "tavg",
                                      res = res,
                                      path = path)
  }

  tavg_crop <- terra::crop(tavg,
                           region,
                           mask = mask)

  # add raster layer with mean across months:
  tavg_crop <- c(tavg_crop, terra::app(tavg_crop, "mean", na.rm = TRUE))

  # delete values outside thermal optimum zones:
  tavg_crop[tavg_crop < min(t_vals) | tavg_crop > max(t_vals)] <- NA

  return(tavg_crop)
}
