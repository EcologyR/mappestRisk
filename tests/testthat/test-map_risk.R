tavg_file <- system.file("extdata/tavg_lux.tif", package = "mappestRisk")
tavg_rast <- terra::rast(tavg_file)

t_vals <- dplyr::tibble(model_name = "any model",
                     suitability = 75,
                     tval_left = rnorm(1, 15, 1),
                     tval_right = rnorm(1, 22, 1),
                     pred_suit = .1,
                     iter = sample(1:100, 1))

tvals_several <- dplyr::tibble(model_name = "any model",
                               suitability = 75,
                               tval_left = rnorm(10, 15, 1),
                               tval_right = rnorm(10, 22, 1),
                               pred_suit = .1,
                               iter = sample(1:100, 10))

ext_region <- terra::ext(-10, 10, 30, 40)

# Test the function with  character region in `country_names`:
test_that("map_risk function works with character region from `country_names", {
  result <- map_risk(t_vals = t_vals,
                     region =  "Réunion",
                     path = tempdir(),
                     verbose = TRUE)
  expect_true(inherits(result, "SpatRaster"))
  expect_equal(terra::nlyr(result), 1)
  expect_true(names(result) == "mean")
  })

# Test the function with provided rasters:
test_that("map_risk function works with provided rasters", {
  result <- map_risk(t_vals = t_vals,
                     path = tempdir(),
                     t_rast = tavg_rast)
  expect_true(inherits(result, "SpatRaster"))
  expect_equal(terra::nlyr(result), 1)
  expect_true(names(result) == "mean")
})

# Test the function with a SpatVector region:
region <- readRDS(file = test_path("testdata", "sobrarbe.rds"))

test_that("map_risk function works with a SpatVector region", {
  result <- map_risk(t_vals = t_vals,
                     region = region,
                     path = tempdir(),
                     verbose = T,
                     res = 10)
  expect_true(inherits(result, "SpatRaster"))
  expect_equal(terra::nlyr(result), 1)
  expect_true(names(result) == "mean")

})

# Test the function with a SpatExtent region:
extent <- ext(-10, 10, 30, 50)
test_that("map_risk function works with a SpatExtent region", {
  result <- map_risk(t_vals = t_vals,
                     region = extent,
                     t_rast = tavg_rast,
                     res = 10)
  expect_true(inherits(result, "SpatRaster"))
  expect_equal(terra::nlyr(result), 1)
  expect_true(names(result) == "mean")
  })

# Test the function with a numeric vector region:
region <- c(-10, 10, 30, 50)
test_that("map_risk function works with a numeric vector region", {
  result <- map_risk(t_vals = t_vals,
                     t_rast = tavg_rast,
                     region = region,
                     res = 10)
  expect_true(inherits(result, "SpatRaster"))
  expect_equal(terra::nlyr(result), 1)
  expect_true(names(result) == "mean")
})

# Test the function with multiple character regions:
test_that("map_risk works for multiple character regions", {
  region <- c("Guadeloupe", "Dominica", "Martinique")
  result <- map_risk(t_vals = t_vals,
                     region = region,
                     path = tempdir(),
                     res = 10)
  expect_true(inherits(result, "SpatRaster"))
  expect_equal(terra::nlyr(result), 1)
  expect_true(names(result) == "mean")

})


# Test that the function returns the same result when using a SpatExtent or a SpatVector as input for region

test_that("the function returns the same result with SpatExtent and SpatVector input for region", {
  ext_region <- terra::ext(tavg_rast)
  vec_region <- terra::vect(ext_region, crs = "epsg:4326")
  result1 <- map_risk(t_vals = t_vals,
                      region = ext_region,
                      t_rast = tavg_rast,
                      res = 10)
  result2 <- map_risk(t_vals = t_vals,
                      region = vec_region,
                      t_rast = tavg_rast,
                      res = 10)
  expect_identical(all.equal(result1, result2), TRUE)
})

# Test that the function returns an error when providing an invalid numeric vector for t_vals
test_that("the function returns an error when providing an invalid numeric vector for region", {
  expect_error(map_risk(t_vals = c(12.5, 20.3), region = c(-10, 10, 30, 40), t_rast = tavg_rast),
               "The argument `t_vals` must be a tibble or data.frame inherited
from the output of `mappestRisk::therm_suit_bounds()` function.
No modifications of columns of the `t_vals` data.frame are allowed in order
to ensure a continuous workflow of the package functions",
               fixed = TRUE)
  })

# Test that the function returns an error when providing an invalid numeric vector for region
test_that("the function returns an error when providing an invalid numeric vector for region", {
  expect_error(map_risk(t_vals = t_vals,
                        region = c(-10, 10, 30),
                        path = tempdir(),
                        t_rast = tavg_rast),
               "When provided as a numeric vector, 'region' must have length 4 (xmin, xmax, ymin, ymax) in unprojected geographic coordinates (EPSG:4326).",
               fixed = TRUE)
})

# Test that the function returns an error when no valid path to download data is given
test_that(" the function returns an error when no path to download data is given
            and raster of temperatures neither", {
  expect_error(map_risk(t_vals = t_vals,
                        region = "Réunion"),
               "Please provide an existing 'path' to save the downloaded temperature maps.",
               fixed = TRUE)
})


# Test that the function returns an error when providing an invalid character vector for region
test_that("the function returns an error when providing an invalid character vector for region", {
  expect_error(map_risk(t_vals = t_vals,
                        region = "nowhere",
                        path = tempdir(),
                        t_rast = tavg_rast),
               "Input region(s) nowhere not found. For available region names, run 'data(country_names); country_names'.",
               fixed = TRUE)
})

# # Test that `interactive = TRUE` yields a leaflet object rather than a `SpatRaster`
# test_that("interactive html map is plotted in the Viewer when `interactive = TRUE`", {
#   result <- map_risk(t_vals = t_vals,
#                      region = "Spain",
#                      path = tempdir(),
#                      interactive = TRUE)
#   expect_true(any(class(result) == "leaflet"))
#   })

# Test that `mask = FALSE` is adjusted with the extent
test_that("`mask = FALSE` gives more cells with NA values than mask = TRUE", {
  result <- map_risk(t_vals = t_vals,
                     region = "Réunion",
                     mask = FALSE,
                     path = tempdir(),
                     interactive = FALSE)
  result_masked <- map_risk(t_vals = t_vals,
                            region = "Réunion",
                            mask = TRUE,
                            path = tempdir(),
                            interactive = FALSE)
  how_many_nas_nomask <- nrow(result[which(!is.na(result[]))])
  how_many_nas_mask <- nrow(result_masked[which(!is.na(result_masked[]))])
  expect_true(how_many_nas_mask < how_many_nas_nomask)
})

# Test that different CRSs are re-projected to be the same
test_that("t_rast with EPSG: 3035 is converted to EPSG: 4326 for masking", {
  region <- readRDS(file = test_path("testdata", "sobrarbe.rds"))
  result <- map_risk(t_vals = t_vals,
                     region = region,
                     mask = FALSE,
                     path = tempdir(),
                     interactive = FALSE,
                     res = 10)
  expect_true(terra::same.crs(tavg_rast, result)
  )
})

# Test that mask should be logical
test_that("using a non-logical argument for `mask` drops an error", {
  expect_error(result <- map_risk(t_vals = t_vals,
                                  region = "Réunion",
                                  mask = "true",
                                  path = tempdir()),
               "`mask` must be logical (`TRUE` or `FALSE`). Defaults to `TRUE`.",
               fixed = TRUE)
               })

# Test that interactive should be logical
test_that("using a non-logical argument for `interactive` drops an error", {
  expect_error(result <- map_risk(t_vals = t_vals,
                                  region = "Réunion",
                                  interactive = "yes",
                                  path = tempdir()),
               "`interactive` must be logical (`TRUE` or `FALSE`). Defaults to `TRUE`.",
               fixed = TRUE)
})

# Test that plot should be logical
test_that("using a non-logical argument for `plot` drops an error", {
  expect_error(result <- map_risk(t_vals = t_vals,
                                  region = "Réunion",
                                  plot = "png",
                                  path = tempdir()),
               "`plot` must be logical (`TRUE` or `FALSE`). Defaults to `TRUE`.",
               fixed = TRUE)
})

# Test that verbose should be logical
test_that("using a non-logical argument for `verbose` drops an error", {
  expect_error(result <- map_risk(t_vals = t_vals,
                                  region = "Réunion",
                                  verbose = "no",
                                  path = tempdir()),
               "`verbose` must be logical (`TRUE` or `FALSE`). Defaults to `TRUE`.",
               fixed = TRUE)
})

# Test that t_rast should be a SpatRast
test_that("t_rast being a matrix drops an error", {
  expect_error(result <- map_risk(t_vals = t_vals,
                                  t_rast = as.matrix(tavg_rast),
                                  path = tempdir()),
               )
})

# Test that t_rast should be a SpatRast has 12 layers (one for each month)
test_that("t_rast with just one layer gives an error", {
  expect_error(result <- map_risk(t_vals = t_vals,
                                  t_rast = tavg_rast[[1]],
                                  path = tempdir()),
  )
  })



# Test that no overlapping yields an error for a extent
test_that("Luxembourg temperatures tavg_rast cannot be mapped for a region in the equator", {
  expect_error(
    result <- map_risk(t_vals = t_vals,
                       t_rast = tavg_rast,
                       region = c(0, 1, 0, 1),
                       path = tempdir()),
    "There's no overlap between 'region' and 't_rast'.")
})

# Test that the output of the function is cropped to the extent of the input region
test_that("Luxembourg temperatures tavg_rast cannot be mapped for a region in the equator", {
  result <- map_risk(t_vals = t_vals,
                     t_rast = tavg_rast,
                     region = c(6, 6.10, 49.7, 50),
                     path = tempdir())
  expect_true(all(terra::ext(result) == terra::ext(c(6, 6.10, 49.7, 50))))
    })

# Test that if more than one row are found, the output has two layers ("mean" and "sd)
test_that("resulting SpatRaster has two layers if t_vals has more than one row", {

  result <- map_risk(t_vals = tvals_several,
                     region = "Réunion",
                     path = tempdir())
  expect_true(terra::nlyr(result) == 2)
})

# Test that if more than one row are found, the output has two layers named "mean" and "sd"
test_that("resulting SpatRaster has two layers if t_vals has more than one row", {

  result <- map_risk(t_vals = tvals_several,
                     region = "Réunion",
                     path = tempdir())
  expect_true(all(names(result) == c("mean", "sd")))
})

