#' Map pest risk
#'
#' @param t_vals Numeric vector of length 2 specifying the left (minimum) and
#' right (maximum) temperature bounds for optimal performance of the target
#' species.
#' @param t_rast Optional 12-layer [terra::SpatRaster] with monthly mean
#' temperatures for (at least) the target 'region'. If not provided, global
#' WorldClim raster layers will be (down)loaded and cropped to 'region'. Note
#' that the download can be slow the first time you use the function in a new
#' working directory. If you get an error, consider running e.g.
#' [options(timeout = 500)] (or more).
#' @param region Optional object specifying the region to map. Must overlap the
#' extent of 't_rast' if both are provided. Can be a [terra::SpatVector] polygon #' map (obtained with [terra::vect()]); or a character vector of country name(s)
#' in English, in which case a countries map will be downloaded and subsetted to #' those countries; or a [terra::SpatExtent] object (obtained with
#' [terra::ext()]); or a numeric vector of length 4 specifying the region
#' coordinates in the order xmin, xmax, ymin, ymax. The latter two must be in
#' unprojected coordinates (WGS84, EPSG:4326). If NULL, the output maps will
#' cover the entire `t_rast` if provided, or the entire world otherwise.
#' @param res Argument to pass to [geodata::worldclim_global()] specifying
#' the spatial resolution for the raster maps to download, if 't_rast' is not
#' provided. The default is 2.5 arc-minutes.
#' @param path Argument to pass to [geodata::worldclim_global()] (if `t_rast`
#' is not provided) and/or to [geodata::world()] (if 'region' is a vector of
#' country names) specifying the folder path for the downloaded maps.
#' @param mask Logical value to pass to [terra::crop()] specifying whether the
#' output raster maps should be masked with the borders of the target 'region',
#' if this is a polygon map or a vector of country names. The default is TRUE.
#' If FALSE, the entire rectangular extent of 'region' will be used.
#' @param output Character value specifying the type of values in the output
#' map pixels. Can be "binary" (the default), to identify pixels within (1)
#' and outside (0) the bounds of the input 't_vals'; or "value", to assign
#' to pixels the temperature values where these are within the bounds of the
#' input 't_vals', and NA where they are not.
#' @param verbose Logical value specifying whether to display messages about
#' what the function is doing at possibly slow steps. The default is FALSE.
#' Setting it to TRUE can be useful for checking progress when maps are large.
#' @return This function returns a [terra::SpatRaster] object with 13 layers:
#' one for each month, and a final summary layer with the sum (if output
#' ="binary") or the mean (if output="value") across months.
#' @export
#' @examples
#' tavg_file <- system.file("extdata/tavg_lux.tif", package = "mappestRisk")
#' tavg_rast <- terra::rast(tavg_file)
#' terra:::plot(tavg_rast)
#'
#' risk_rast_binary <- map_risk(t_vals = c(12.5, 21.4), t_rast = tavg_rast)
#' # note that 't_vals' should be an output of fit_devmodels()
#'
#' terra::plot(risk_rast_binary)
#'
#' terra::plot(risk_rast_binary[[13]])
#'
#'
#' risk_rast_value <- map_risk(t_vals = c(12.5, 21.4), t_rast = tavg_rast,
#' output = "value")
#'
#' terra::plot(risk_rast_value)
#'
#' terra::plot(risk_rast_value[[13]])
#'
#' \dontrun{
#' # if you don't have temperature rasters for your region:
#' risk_rast <- map_risk(t_vals = c(12.5, 21.4), path = "downloaded_maps",
#' region = c("Portugal", "Spain"), verbose = TRUE)

#' terra::plot(risk_rast[[13]])

#' # if you want to save some output map(s) to disk:
#' terra::writeRaster(risk_rast[[13]], "risk_map.tif")  # exported maps can
#' be seen with GIS software
#' }
#'
map_risk <- function(t_vals = NULL,
                     t_rast = NULL,
                     region = NULL,
                     res = 2.5,
                     path = NULL,
                     mask = TRUE,
                     output = "binary",
                     verbose = FALSE
) {

  stopifnot(is.numeric(t_vals),
            length(t_vals) == 2  #,
            # t_vals[2] > t_vals[1]  # not necessary
  )

  if (!is.null(t_rast)) {
    stopifnot(inherits(t_rast, "SpatRaster"))
    stopifnot(length(t_rast) == 1)
    stopifnot(terra::nlyr(t_rast) == 12)
  }

  if (is.null(path)) {
    if (is.null(t_rast)) {
      stop("Please provide a 'path' to save the downloaded temperature maps.")
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
    if (mask == FALSE) {
      region <- terra::ext(region)
    }
  }

  if (is.numeric(region)) {
    if (length(region) != 4) {
      stop("When provided as a numeric vector, 'region' must have length 4 (xmin, xmax, ymin, ymax).")
    }
    region <- terra::ext(region)
  }

  if (is.null(region)) {
    if (is.null(t_rast)) {
      region <- terra::ext()  # whole world
    } else {
      region <- terra::ext(t_rast)
    }
  }

  if (is(region, "SpatExtent")) {
    mask <- FALSE  # pointless otherwise
  }

  if (is.null(t_rast)) {
    if (verbose) cat("\n(Down)loading temperature rasters...\n")
    t_rast <- geodata::worldclim_global(var = "tavg",
                                        res = res,
                                        path = path)
  }

  if (!isTRUE(terra::compareGeom(t_rast, terra::rast(region), crs = TRUE,
                                 lyrs = FALSE, warncrs = FALSE, ext = FALSE,
                                 rowcol = FALSE, res = FALSE,
                                 stopOnError = FALSE, messages = FALSE))) {
    if (is(region, "SpatExtent")) {
      region <- terra::vect(region, crs = "epsg:4326")
    }  # otherwise terra::project fails
    if (verbose) cat("\nProjecting 'region' to 't_rast'...\n")
    region <- terra::project(region, t_rast)
  }

  if (!isTRUE(all.equal(terra::ext(region), terra::ext()))) {
    if (verbose) cat("\nCropping temperature rasters to region...\n")
    if (is.null(terra::intersect(terra::ext(tavg_rast), terra::ext(region)))) {
      stop("There's no overlap between 'region' and 't_rast'. Result is an empty raster.")
    }
    t_rast <- terra::crop(t_rast,
                          region,
                          mask = mask)
  }

  # delete values outside thermal optimum zones:
  outside <- t_rast < min(t_vals) | t_rast > max(t_vals)
  if (output == "binary") {
    t_rast[outside] <- 0
    t_rast[!outside] <- 1
  } else {
    t_rast[outside] <- NA_real_
  }

  # outside <- terra::app(t_rast, function(x) x < min(t_vals) | x > max(t_vals))
  # if (output == "binary") {
  #   t_rast <- !outside
  # } else {
  #   t_rast[outside] <- NA_real_
  # }

  # add 13th raster layer with summary stat across months:
  fun <- ifelse(output == "binary", "sum", "mean")
  if (verbose) cat("\nComputing final summary layer...\n")
  layer13 <- terra::app(t_rast, fun, na.rm = TRUE)
  t_rast <- c(t_rast, layer13)

  if (verbose) cat("\nFinished!\n")
  return(t_rast)
}
