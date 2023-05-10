library(testthat)

# Define example temperature bounds for a target species:
t_vals <- c(12.5, 21.4)
ext_region <- terra::ext(-10, 10, 30, 40)

# Test the function with default arguments:
test_that("map_risk function works with default arguments", {
  result <- map_risk(t_vals = t_vals, path = "downloaded_maps")
  expect_true(is(result, "SpatRaster"))
  expect_equal(length(result), 13)
})

# Test the function with a SpatVector region:
library(terra)
region <- system.file("ex/lux.shp", package="terra")
test_that("map_risk function works with a SpatVector region", {
  result <- map_risk(t_vals = t_vals, region = region, path = "downloaded_maps")
  expect_true(is(result, "SpatRaster"))
  expect_equal(length(result), 13)
})

# Test the function with a SpatExtent region:
extent <- ext(-10, 10, 30, 50)
test_that("map_risk function works with a SpatExtent region", {
  result <- map_risk(t_vals = t_vals, region = extent, path = "downloaded_maps")
  expect_true(is(result, "SpatRaster"))
  expect_equal(length(result), 13)
})

# Test the function with a numeric vector region:
region <- c(-10, 10, 30, 50)
test_that("map_risk function works with a numeric vector region", {
  result <- map_risk(t_vals = t_vals, region = region)
  expect_true(is(result, "SpatRaster"))
  expect_equal(length(result), 13)
})

# Test the function with a character region:
region <- "Luxembourg"
test_that("map_risk function works with a character vector region", {
  result <- map_risk(t_vals = t_vals, region = region, path = "downloaded_maps")
  expect_true(is(result, "SpatRaster"))
  expect_equal(length(result), 13)
})

# Test the function with multiple character regions:
test_that("map_risk works for multiple character regions", {
  region <- c("Luxembourg", "Belgium")
  result <- map_risk(t_vals = t_vals, region = region, path = "downloaded_maps")
  expect_equal(class(result), "SpatRaster")
  expect_equal(nlyr(result), 13)
})

# Test that the values outside the temperature bounds are NA
test_that("values outside temperature bounds are NA", {
  result <- map_risk(t_vals = t_vals, region = "Switzerland", path = "downloaded_maps")
  expect_true(all(is.na(result[[1]][result[[1]] < 12.5 | result[[1]] > 21.4])))
})

# Test that the function returns the same result when using a SpatExtent or a SpatVector as input for region
test_that("the function returns the same result with SpatExtent and SpatVector input for region", {
  vec_region <- terra::vect(ext_region)
  result1 <- map_risk(t_vals = t_vals, region = ext_region, path = "downloaded_maps")
  result2 <- map_risk(t_vals = t_vals, region = vec_region, path = "downloaded_maps")
  expect_identical(result1, result2)
})

# Test that the function returns an error when providing an invalid numeric vector for t_vals
test_that("the function returns an error when providing an invalid numeric vector for region", {
  expect_error(map_risk(t_vals = t_vals[1], region = c(-10, 10, 30, 40), path = "downloaded_maps"))
})

# Test that the function returns an error when providing an invalid numeric vector for region
test_that("the function returns an error when providing an invalid numeric vector for region", {
  expect_error(map_risk(t_vals = t_vals, region = c(-10, 10, 30), path = "downloaded_maps"))
})

# Test that the function returns an error when providing an invalid character vector for region
test_that("the function returns an error when providing an invalid character vector for region", {
  expect_error(map_risk(t_vals = t_vals, region = "nowhere", path = "downloaded_maps"))
})

# Test that the output of the function is cropped to the extent of the input region
test_that("the output is cropped to the extent of the input region", {
  result <- map_risk(t_vals = t_vals, region = ext_region, path = "downloaded_maps")
  expect_equal(ext(result), ext_region)
})

# Test that the function returns the same result when using a pre-existing SpatRaster for t_rast and downloading a new SpatRaster with worldclim_global()
test_that("map_risk returns same result for t_rast and worldclim_global()", {

  # Create SpatRaster with worldclim_global()
  wclim <- geodata::worldclim_global(var = "tavg", res = 10, path = "downloaded_maps")
  t_rast <- terra::crop(wclim, region)

  # Run function with t_rast
  result_t_rast <- map_risk(t_vals = t_vals, t_rast = t_rast, path = "downloaded_maps")

  # Run function with worldclim_global()
  result_worldclim <- map_risk(t_vals = t_vals, region = region, res = res, path = "downloaded_maps")

  # Test output
  expect_identical(result_t_rast, result_worldclim)

})

# Binary output, with temperature range covering the entire range of the data, should produce only 0s and 1s
test_that("Binary output produces SpatRaster with pixel values of 0 or 1", {
  r <- map_risk(t_vals = c(-50, 50), output = "binary")
  expect_true(all(r[] %in% c(0, 1), na.rm = TRUE))
})

# Value output, with temperature range covering the entire range of the data, should produce pixel values between the min and max temperature
test_that("Value output produces SpatRaster with pixel values between min and max temperature", {
  r <- map_risk(t_vals = c(-50, 50), output = "value")
  expect_true(all(r[] >= -50 & r[] <= 50, na.rm = TRUE))
})

# Binary output, with temperature range outside the range of the data, should produce only 0s
test_that("Binary output with temperature range outside data range produces all 0s", {
  r <- map_risk(t_vals = c(-100, -50), output = "binary")
  expect_true(all(r[] == 0, na.rm = TRUE))
})

# Value output, with temperature range outside the range of the data, should produce only NAs
test_that("Value output with temperature range outside data range produces all NAs", {
  r <- map_risk(t_vals = c(-100, -50), output = "value")
  expect_true(all(is.na(r[])))
})
