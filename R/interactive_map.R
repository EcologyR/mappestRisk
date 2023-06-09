#' Generate an interactive map from a risk map
#'
#' @param x a SpatRaster object
#' @param map_type the type of map. One of "high" or "medium" risk.
#' @param legend_title the title of the legend.
#' @param path_out (optional) the name and the path of the output file. Example: "data/map.html"
#' @param pal the palette used to plot the risk map. Default ...
#'
#' @return a dynamic map and, optionally, a html with the dynamic map
#' @export
#'
#' @examples
#' \dontrun{
#' r <- system.file("extdata", "trioza_rast.tif", package = "mappestRisk")
#' # r <- terra::rast("inst/extdata/trioza_rast.tif")
#' interactive_map(x = r, map_type = "high", path_out = paste0(tempdir(), "test_map.html"))
#' }
#'
interactive_map <- function(x,
                     legend_title = NULL,
                     map_type = c("high", "medium"),
                     path_out = NULL,
                     pal = NULL
                     ) {

  stopifnot(inherits(x, "SpatRaster"))

  # current leaflet version on CRAN (2.1.2) does not seem to accept SpatRaster yet
  # temporary fix:
   if (inherits(x, "SpatRaster")) {
     x <- raster::raster(x)
   }

  # current leafem version does not seem to accept SpatRaster yet.
  # tempory fix:
  # x <- raster::raster(x)

  # check epsg
  # if (!(terra::same.crs(x, "EPSG:3857"))) {
  #   x <- terra::project(x, "EPSG:3857", method = "near")}

  # argument legend
  if (is.null(legend_title)) {
    map_type <- match.arg(map_type)
    legend_title <- paste0("Number of ", map_type, " suitable months")
  }

  # palette
  if (is.null(pal)) {
    mycolors <- c("grey65",
                  grDevices::colorRampPalette(c("#0081a7","#00afb9","#fdfcdc","#fed9b7","#f07167"))(12)
                  )
    pal <- leaflet::colorFactor(mycolors, terra::values(x), na.color = "transparent")
  }

  outmap <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron,
                     group = "Basemap") |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                     group = "Satellite") |>
    leaflet::addRasterImage(x,
                            method = "ngb",
                            layerId = "Risk Map",
                            group = "Risk Map",
                            colors = mycolors) |>
    leaflet::addMiniMap(tiles = leaflet::providers$Jawg.Light,
               toggleDisplay = TRUE) |>
    leaflegend::addLegendFactor(values = raster::values(x),
                                pal = pal,
                                title = legend_title,
                                position = "bottomleft",
                                orientation = 'horizontal') |>
    leaflet.opacity::addOpacitySlider(layerId = "Risk Map") |>
    leafem::addImageQuery(x,
                          project = FALSE,
                          type = "mousemove",
                          layerId = "Risk Map",
                          digits = 2
    ) |>
    leaflet::addLayersControl(position = "bottomright",
                     baseGroups = c("Basemap", "Satellite"),
                     overlayGroups = c("Risk Map"),
                     options = leaflet::layersControlOptions(collapsed = TRUE))

  # export html file
  if (!is.null(path_out)) {
    htmlwidgets::saveWidget(widget = outmap, file = path_out,
                          selfcontained = TRUE)
  }

  return(outmap)

}
