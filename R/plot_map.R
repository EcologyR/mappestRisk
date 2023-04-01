#' Generate an interactive map from a risk map
#'
#' @param x a raster object
#' @param map_type the type of map. One of "high" or "medium".
#' @param legend_title the title of the legend.
#' @param path_out the name and the path of the output file. Example: "data/map.html"
#' @param pal the palette used to plot the risk map. Default ...
#'
#' @return a dynamic map and a html with the dynamic map
#' @export
#'
#' @example
#' \dontrun{
#' p <- tempdir()
#' r <- raster::raster("inst/trioza_rast.tif")
#' plot_map(x = r, map_type = "high", path_out = paste0(tempdir(), "test_map.html"))
#' }
#'
plot_map <- function(x, legend_title = NULL,
                     map_type = "high",
                     path_out = NULL,
                     pal = NULL, ...){

  #  if(!is(x, "RasterLayer"))
  # if(!any(x %in% c("SpatRaster", "RasterLayer", "stars", "terra")))
  #     stop("x has to be a raster object")

  # argument legend
  if (is.null(legend_title)) {
    map_type <- match.arg(map_type, c("high", "medium"))
    legend_title <- paste0("Number of ", map_type, " suitable months")
  }

  # palette
  if (is.null(pal)) {
    mycolors <- c("grey65",
                  colorRampPalette(c("#0081a7","#00afb9","#fdfcdc","#fed9b7","#f07167"))(12))
    pal <- colorFactor(mycolors, values(x), na.color = "transparent")
  }

  outmap <- leaflet() |>
    addProviderTiles(providers$CartoDB.Positron,
                     group = "Basemap") |>
    addProviderTiles(providers$Esri.WorldImagery,
                     group = "Satellite") |>
    addRasterImage(x,
                   layerId = "Risk Map",
                   group = "Risk Map",
                   colors = mycolors) |>
    addMiniMap(tiles = providers$Jawg.Light,
               toggleDisplay = TRUE) |>
    leaflegend::addLegendFactor(values = values(x),
                                pal = pal,
                                title = legend_title,
                                position = "bottomleft",
                                orientation = 'horizontal') |>
    leaflet.opacity::addOpacitySlider(layerId = "Risk Map") |>
    leafem::addImageQuery(x, project = FALSE,
                          type="mousemove",
                          layerId = "Risk Map",
                          digits = 2
    ) |>
    addLayersControl(position = "bottomright",
                     baseGroups = c("Basemap", "Satellite"),
                     overlayGroups = c("Risk Map"),
                     options = layersControlOptions(collapsed = TRUE))

  # export html file
  htmlwidgets::saveWidget(widget = outmap, file = path_out,
                          selfcontained = TRUE)

  return(outmap)

}
