library(testthat)
library(terra)

tavg_file <- system.file("extdata/tavg_lux.tif", package = "mappestRisk")
tavg_rast <- terra::rast(tavg_file)
t_vals <- c(12.5, 21.4)
ext_region <- terra::ext(-10, 10, 30, 40)

# Test the function with default arguments:
test_that("map_risk function works with default arguments", {
  skip_on_ci()
  skip_on_cran()
  result <- map_risk(t_vals = t_vals, path = "downloaded_maps", verbose = TRUE)
  expect_true(is(result, "SpatRaster"))
  expect_equal(terra::nlyr(result), 13)
  # unlink("downloaded_maps")
})

# Test the function with provided rasters:
test_that("map_risk function works with provided rasters", {
  result <- map_risk(t_vals = t_vals, t_rast = tavg_rast)
  expect_true(is(result, "SpatRaster"))
  expect_equal(terra::nlyr(result), 13)
})

# Test the function with a SpatVector region:
region <- terra::vect(system.file("ex/lux.shp", package="terra"))
test_that("map_risk function works with a SpatVector region", {
  result <- map_risk(t_vals = t_vals, region = region, t_rast = tavg_rast)
  expect_true(is(result, "SpatRaster"))
  expect_equal(terra::nlyr(result), 13)
})

# Test the function with a SpatExtent region:
extent <- ext(-10, 10, 30, 50)
test_that("map_risk function works with a SpatExtent region", {
  result <- map_risk(t_vals = t_vals, region = extent, t_rast = tavg_rast)
  expect_true(is(result, "SpatRaster"))
  expect_equal(terra::nlyr(result), 13)
})

# Test the function with a numeric vector region:
region <- c(-10, 10, 30, 50)
test_that("map_risk function works with a numeric vector region", {
  result <- map_risk(t_vals = t_vals, t_rast = tavg_rast, region = region)
  expect_true(is(result, "SpatRaster"))
  expect_equal(terra::nlyr(result), 13)
})

# Test the function with a character region:
region <- "Luxembourg"
test_that("map_risk function works with a character vector region", {
  skip_on_ci()
  skip_on_cran()
  result <- map_risk(t_vals = t_vals, region = region, path = "downloaded_maps")
  expect_true(is(result, "SpatRaster"))
  expect_equal(terra::nlyr(result), 13)
})

# Test the function with multiple character regions:
test_that("map_risk works for multiple character regions", {
  region <- c("Luxembourg", "Belgium")
  result <- map_risk(t_vals = t_vals, region = region, path = "downloaded_maps")
  expect_true(is(result, "SpatRaster"))
  expect_equal(terra::nlyr(result), 13)
})

# Test that the values outside the temperature bounds are NA
test_that("values outside temperature bounds are NA", {
  result <- map_risk(t_vals = t_vals, t_rast = tavg_rast)
  expect_true(all(result[[1]][result[[1]] < 12.5 | result[[1]] > 21.4] == 0))
})

# Test that the function returns the same result when using a SpatExtent or a SpatVector as input for region
ext_region <- terra::ext(tavg_rast)
vec_region <- terra::vect(ext_region, crs = "epsg:4326")
test_that("the function returns the same result with SpatExtent and SpatVector input for region", {
  result1 <- map_risk(t_vals = t_vals, region = ext_region, t_rast = tavg_rast)
  result2 <- map_risk(t_vals = t_vals, region = vec_region, t_rast = tavg_rast)
  expect_identical(all.equal(result1, result2), TRUE)
})

# Test that the function returns an error when providing an invalid numeric vector for t_vals
test_that("the function returns an error when providing an invalid numeric vector for region", {
  expect_error(map_risk(t_vals = t_vals[1], region = c(-10, 10, 30, 40), t_rast = tavg_rast))
})

# Test that the function returns an error when providing an invalid numeric vector for region
test_that("the function returns an error when providing an invalid numeric vector for region", {
  expect_error(map_risk(t_vals = t_vals, region = c(-10, 10, 30), t_rast = tavg_rast))
})

# Test that the function returns an error when providing an invalid character vector for region
test_that("the function returns an error when providing an invalid character vector for region", {
  expect_error(map_risk(t_vals = t_vals, region = "nowhere", t_rast = tavg_rast))
})

# Test that the output of the function is cropped to the extent of the input region
test_that("the output is cropped to the extent of the input region", {
  result <- map_risk(t_vals = t_vals, region = ext_region, t_rast = tavg_rast)
  expect_equal(all.equal(ext(result), ext_region), TRUE)
})

# Test that the function returns the same result when using a pre-existing SpatRaster for t_rast and downloading a new SpatRaster with worldclim_global()
# test_that("map_risk returns same result for t_rast and worldclim_global()", {
#   skip_on_ci()
#   skip_on_cran()
#
#   # Create SpatRaster with worldclim_global()
#   wclim <- geodata::worldclim_global(var = "tavg", res = 2.5, path = "downloaded_maps")
#
#   # Run function with t_rast
#   result_t_rast <- map_risk(t_vals = t_vals, t_rast = tavg_rast, verbose = TRUE)
#
#   # Run function with worldclim_global()
#   result_worldclim <- map_risk(t_vals = t_vals, region = region, res = res, t_rast = wclim, verbose = TRUE)
#
#   # Test output
#   expect_true(all.equal(result_t_rast, result_worldclim))
# })

# Binary output, with temperature range covering the entire range of the data, should produce only 0s and 1s
test_that("Binary output produces SpatRaster with pixel values of 0 or 1", {
  result <- map_risk(t_vals = t_vals, t_rast = tavg_rast, output = "binary")
  monthly_layers <- result[[-13]]
  out_values <- as.vector(terra::values(monthly_layers))
  out_values <- out_values[!is.nan(out_values)]
  expect_true(all(out_values %in% c(0, 1), na.rm = TRUE))
})

# Value output, with temperature range covering the entire range of the data, should produce pixel values between the min and max temperature
test_that("Value output produces SpatRaster with pixel values between min and max temperature", {
  result <- map_risk(t_vals = c(-50, 50), t_rast = tavg_rast, output = "value")
  expect_true(all(result[] >= -50 & result[] <= 50, na.rm = TRUE))
})

# Binary output, with temperature range outside the range of the data, should produce only 0s
test_that("Binary output with temperature range outside data range produces all 0s", {
  result <- map_risk(t_vals = c(-100, -50), t_rast = tavg_rast, output = "binary")
  expect_true(all(result[] == 0, na.rm = TRUE))
})

# Value output, with temperature range outside the range of the data, should produce only NAs
test_that("Value output with temperature range outside data range produces all NAs", {
  result <- map_risk(t_vals = c(-100, -50), output = "value", t_rast = tavg_rast)
  expect_true(all(is.na(result[])))
})
