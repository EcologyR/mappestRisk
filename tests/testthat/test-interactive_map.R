library(testthat)
library(mappestRisk)
library(terra)

# test data
tavg_file <- system.file("extdata/tavg_lux.tif", package = "mappestRisk")
tavg_rast <- terra::rast(tavg_file)
risk_rast_binary <- map_risk(t_vals = c(12.5, 21.4), t_rast = tavg_rast)
r <- risk_rast_binary[[13]]

test_that("interactive_map throws an error if x is not a SpatRaster", {
  expect_error(interactive_map(x = "not a raster"))
})
