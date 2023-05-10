library(testthat)

# Define example temperature bounds for a target species:
t_vals <- c(12.5, 21.4)
ext_region <- terra::ext(-10, 10, 30, 40)

# Test the function with default arguments:
test_that("map_risk function works with default arguments", {
  result <- map_risk(t_vals = t_vals)
  expect_true(is(result, "SpatRaster"))
  expect_equal(length(result), 13)
})

# Test the function with a SpatVector region:
library(terra)
region <- system.file("ex/lux.shp", package="terra")
test_that("map_risk function works with a SpatVector region", {
  result <- map_risk(t_vals = t_vals, region = region)
  expect_true(is(result, "SpatRaster"))
  expect_equal(length(result), 13)
})

# Test the function with a SpatExtent region:
extent <- ext(-10, 10, 30, 50)
test_that("map_risk function works with a SpatExtent region", {
  result <- map_risk(t_vals = t_vals, region = extent)
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
  result <- map_risk(t_vals = t_vals, region = region)
  expect_true(is(result, "SpatRaster"))
  expect_equal(length(result), 13)
})

# Test the function with multiple character regions:
test_that("map_risk works for multiple character regions", {
  region <- c("Luxembourg", "Belgium")
  result <- map_risk(t_vals = t_vals, region = region)
  expect_equal(class(result), "SpatRaster")
  expect_equal(nlyr(result), 13)
})

# Test that the values outside the temperature bounds are NA
test_that("values outside temperature bounds are NA", {
  result <- map_risk(t_vals = t_vals, region = "Switzerland")
  expect_true(all(is.na(result[[1]][result[[1]] < 12.5 | result[[1]] > 21.4])))
})

# Test that the function returns the same result when using a SpatExtent or a SpatVector as input for region
test_that("the function returns the same result with SpatExtent and SpatVector input for region", {
  vec_region <- terra::vect(ext_region)
  result1 <- map_risk(t_vals = t_vals, region = ext_region)
  result2 <- map_risk(t_vals = t_vals, region = vec_region)
  expect_identical(result1, result2)
})

# Test that the function returns an error when providing an invalid numeric vector for t_vals
test_that("the function returns an error when providing an invalid numeric vector for region", {
  expect_error(map_risk(t_vals = t_vals[1], region = c(-10, 10, 30, 40)))
})

# Test that the function returns an error when providing an invalid numeric vector for region
test_that("the function returns an error when providing an invalid numeric vector for region", {
  expect_error(map_risk(t_vals = t_vals, region = c(-10, 10, 30)))
})

# Test that the function returns an error when providing an invalid character vector for region
test_that("the function returns an error when providing an invalid character vector for region", {
  expect_error(map_risk(t_vals = t_vals, region = "nowhere"))
})

# Test that the output of the function is cropped to the extent of the input region
test_that("the output is cropped to the extent of the input region", {
  result <- map_risk(t_vals = t_vals, region = ext_region)
  expect_equal(ext(result), ext_region)
})

# Test that the function returns the same result when using a pre-existing SpatRaster for t_rast and downloading a new SpatRaster with worldclim_global()
test_that("map_risk returns same result for t_rast and worldclim_global()", {

  # Create SpatRaster with worldclim_global()
  wclim <- geodata::worldclim_global(var = "tavg", res = 10)
  t_rast <- terra::crop(wclim, region)

  # Run function with t_rast
  result_t_rast <- map_risk(t_vals = t_vals, t_rast = t_rast)

  # Run function with worldclim_global()
  result_worldclim <- map_risk(t_vals = t_vals, region = region, res = res)

  # Test output
  expect_identical(result_t_rast, result_worldclim)

})
