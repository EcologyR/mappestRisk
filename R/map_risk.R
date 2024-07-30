#' Map pest risk
#'
#' @param t_vals Numeric vector of length 2 or data frame with 2 columns
#' specifying, respectively, the left (minimum) and right (maximum)
#' temperature bounds for the development of the target species.
#' @param t_rast Optional 12-layer [terra::SpatRaster] with monthly mean
#' temperatures for (at least) the target 'region'. If not provided, global
#' WorldClim raster layers will be (down)loaded and cropped to 'region'. Note
#' that the download can be slow the first time you use the function in a new
#' working directory. If you get a download error, consider running e.g.
#' [options(timeout = 500)] (or more).
#' @param region Optional object specifying the region to map. Must overlap the
#' extent of 't_rast' if both are provided. Can be a [terra::SpatVector] polygon #' map (obtained with [terra::vect()]); or a character vector of country name(s)
#' in English, in which case a countries map will be downloaded and subset to
#' those countries; or a [terra::SpatExtent] object (obtained with
#' [terra::ext()]); or a numeric vector of length 4 specifying the region
#' coordinates in the order xmin, xmax, ymin, ymax. The latter two must be in
#' in the same CRS as `t_rast` if `t_rast` is provided, or in unprojected lon-lat
#' coordinates (WGS84, EPSG:4326) otherwise. If NULL, the output maps will cover
#' the entire `t_rast` if provided, or the entire world otherwise.
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
#' @param verbose Logical value specifying whether to display messages about
#' what the function is doing at possibly slow steps. The default is FALSE.
#' Setting it to TRUE can be useful for checking progress when maps are large.
#' @return This function returns a [terra::SpatRaster] with up to 2 layers:
#' the ([mean]) number of months with temperature within the species' thermal
#' bounds; and (if `t_vals` has >1 rows) the standard deviation ([sd]) around
#' that mean.
#' @import terra
#' @export
#' @examples
#' # if you have temperature rasters for your region:
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
#' terra::plot(risk_rast)
#'
#' \dontrun{
#' # if you don't have temperature rasters for your region:
#' risk_rast <- map_risk(t_vals = therm_bounds, path = "downloaded_maps",
#' region = c("Portugal", "Spain"), verbose = TRUE)

#' terra::plot(risk_rast)

#' # if you want to save output map(s) to disk:
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
                     verbose = FALSE
) {

  if (is.atomic(t_vals) && length(t_vals) == 2) {
    t_vals <- t(data.frame(t_vals))
  }

  stopifnot(inherits(t_vals, "data.frame"),
            ncol(t_vals) == 2,
            all(na.omit(t_vals)[ , 2] >= na.omit(t_vals)[ , 1])
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

  if (is.null(terra::intersect(terra::ext(t_rast), terra::ext(region)))) {
    stop("There's no overlap between 'region' and 't_rast'.")
  }

  # if (isTRUE(terra::is.lonlat(region))) {
  #   global <- terra::vect(terra::ext(), crs = "epsg:4326")
  # } else {
  #   global <- terra::vect(terra::ext(), crs = terra::crs(region))
  # }

  # if (!isTRUE(all.equal(terra::ext(region), terra::ext(global)))) {
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
  }

  if (verbose) cat("\nFinished!\n")
  return(out)
}
