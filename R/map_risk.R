#' Map pest risk
#'
#' @param t_vals Numeric vector of length 2 specifying the left (minimum) and
#' right (maximum) temperature bounds for optimal performance of the target species.
#' @param t_rast Optional 12-layer SpatRaster with monthly mean temperatures
#' for (at least) the target 'region'. If not provided, global WorldClim raster
#' layers will be (down)loaded and cropped to 'region'. Note that the download
#' can be slow the first time you use the function in a new working directory.
#' If you get an error, consider running e.g. [options(timeout = 500)].
#' @param region Optional object specifying the region to map.
#' Can be a 'SpatVector' polygon map (obtained with [terra::vect()]);
#' or a 'SpatExtent' object (obtained with [terra::ext()]),
#' or a numeric vector of length 4 specifying the region coordinates
#' in the order xmin, xmax, ymin, ymax;
#' or a character vector of country name(s) in English.
#' If NULL, the output maps will cover the entire `t_rast` if provided,
#' or the entire world otherwise.
#' @param res Argument to pass to [geodata::worldclim_global()] specifying
#' the spatial resolution for the raster maps to download, if 't_rast' is not
#' provided. The default is 2.5 arc-minutes.
#' @param path Argument to pass to [geodata::worldclim_global()] (if `t_rast`
#' is not provided) and/or to [geodata::world()] (if 'region' is a vector of
#'  country names) specifying the folder path for the downloaded maps.
#' @param mask Logical value to pass to [terra::crop()] specifying whether the
#' output raster maps should be masked with the borders of the target 'region',
#' if this is a polygon map or a vector of country names. The default is TRUE.
#' If FALSE, the entire rectangular extent of 'region' will be used.
#' @param output Character value specifying the type of values in the output
#' map pixels. Can be "binary" (the default), to identify pixels within (1)
#' and outside (0) the bounds of the input 't_vals'; or "value", to assign
#' to pixels the temperature values where these are within the bounds of the
#' input 't_vals', and NA where they are not.
#' @return This function returns a SpatRaster object with 13 layers: one for
#' each month, and a final summary layer with the sum (if output="binary") or
#' the mean (if output="value") across months.
#' @export
#' @examples
#' risk_rasters <- map_risk(t_vals = c(12.5, 21.4), path = "downloaded_maps",
#' region = c("Portugal", "Spain"))  # note that 't_vals' should be an output
#' of fit_devmodels()
#'
#' terra::plot(risk_rasters)
#'
#' terra::plot(risk_rasters[[13]])
#'
#' # if you want to save some map(s) to disk:
#' \dontrun{
#' terra::writeRaster(risk_rasters[[13]], "risk_map.tif")  # exported map can be seen with GIS software
#' }
#'
map_risk <- function(t_vals = NULL,
                     t_rast = NULL,
                     region = NULL,
                     res = 2.5,
                     path = NULL,
                     mask = TRUE,
                     output = "binary"
) {

  stopifnot(is.numeric(t_vals),
            length(t_vals) == 2  #,
            # t_vals[2] > t_vals[1]
  )

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

  if (is.null(t_rast)) {
    t_rast <- geodata::worldclim_global(var = "tavg",
                                        res = res,
                                        path = path)
  }

  t_rast_crop <- terra::crop(t_rast,
                             region,
                             mask = mask)

  # delete values outside thermal optimum zones:
  outside <- t_rast_crop < min(t_vals) | t_rast_crop > max(t_vals)
  t_rast_crop[outside] <- ifelse(output == "binary", 0, NA)

  if (output == "binary") {
    t_rast_crop[!outside] <- 1
  }

  # add 13th raster layer with sum or mean across months:
  fun <- ifelse(output == "binary", "sum", "mean")
  t_rast_crop <- c(t_rast_crop, terra::app(t_rast_crop, fun, na.rm = TRUE))

  return(t_rast_crop)
}
