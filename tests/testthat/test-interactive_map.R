library(testthat)
library(mappestRisk)
library(raster)
library(leaflet)

# test data
r <- raster::raster(system.file("extdata", "trioza_rast.tif", package = "mappestRisk"))

test_that("interactive_map throws an error if x is not a SpatRaster or RasterLayer object", {
  expect_error(interactive_map(x = "not a raster"))
})

test_that("interactive_map returns a leaflet object", {
  # Test that the function returns a leaflet map object
  m <- interactive_map(x = r)
  expect_s3_class(m, "leaflet")
  })
