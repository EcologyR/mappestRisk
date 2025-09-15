

data("aphid")

set.seed(2025)

tpcs <- fit_devmodels(temp = aphid$temperature,
                      dev_rate = aphid$rate_value,
                      model_name = c("lactin2", "briere2")
)

curves <- suppressWarnings(
  predict_curves(temp = aphid$temperature,
                 dev_rate = aphid$rate_value,
                 fitted_parameters = tpcs,
                 model_name_2boot = c("lactin2", "briere2"),
                 propagate_uncertainty = TRUE,
                 n_boots_samples = 2))

bounds <- therm_suit_bounds(curves,
                            model_name = "lactin2", suitability_threshold = 80)
bounds2 <- therm_suit_bounds(curves,
                             model_name = c("lactin2", "briere2"),
                                            suitability_threshold = 80)
bound <- bounds[bounds$iter == "estimate", ]

tavg_file <- system.file("extdata/tavg_reunion.tif", package = "mappestRisk")
tavg <- terra::rast(tavg_file)

folder <- tempdir()

map <- map_risk(t_vals = bound, t_rast = tavg, plot = FALSE)


###

test_that("map_risk returns correct output with only one set of thermal bounds", {

  # map <- map_risk(t_vals = bound, t_rast = tavg, plot = FALSE)

  expect_true(inherits(map, "SpatRaster"))
  expect_equal(dim(map), c(62, 74, 1))
  expect_equal(terra::res(map), c(0.00833333333333334, 0.00833333333333332))
  expect_true(terra::is.lonlat(map))
  expect_true(names(map) == "mean")
  expect_equal(min(as.matrix(map), na.rm = TRUE), 0)
  expect_equal(max(as.matrix(map), na.rm = TRUE), 12)

})


test_that("map_risk returns correct output with >1 thermal bounds", {

  map2 <- map_risk(t_vals = bounds, t_rast = tavg, plot = FALSE)

  expect_true(inherits(map2, "SpatRaster"))
  expect_equal(dim(map2), c(62, 74, 2))
  expect_equal(names(map2), c("mean", "sd"))
  expect_equal(min(as.matrix(map2[["mean"]]), na.rm = TRUE), 0)
  expect_equal(max(as.matrix(map2[["mean"]]), na.rm = TRUE), 11.6666667)
  expect_equal(min(as.matrix(map2[["sd"]]), na.rm = TRUE), 0)
  expect_equal(max(as.matrix(map2[["sd"]]), na.rm = TRUE), 2.081666)

})


test_that("map_risk returns correct output with downloaded map", {

  skip_on_cran()
  skip_if_offline()
  skip_on_ci()

  map3 <- map_risk(t_vals = bound, region = "Réunion", path = folder, plot = FALSE)

  expect_true(inherits(map3, "SpatRaster"))
  expect_equal(dim(map3), c(62, 74, 1))
  expect_equal(terra::res(map3), c(0.00833333333333334, 0.00833333333333332))
  expect_true(terra::is.lonlat(map3))
  expect_true(names(map3) == "mean")
  expect_equal(min(as.matrix(map3), na.rm = TRUE), 0)
  expect_equal(max(as.matrix(map3), na.rm = TRUE), 12)

  # map.world <- map_risk(t_vals = bound, res = 10, path = folder, plot = FALSE)
  # too heavy download (36 MB)

})

test_that("masking with 'region' returns correct output", {

  ras <- tavg
  ras[1, ] <- 20 # add values to first row (mostly were NA)
  # plot(tavg, 1)
  # plot(ras, 1)

  # without region
  map4 <- map_risk(t_vals = bound, t_rast = ras, path = folder, plot = FALSE)
  # plot(map4)
  expect_equal(terra::values(map4, row = 1, nrows = 1, mat = FALSE),
               rep(0, times = 74))


  # masking with region

  skip_on_cran()
  skip_if_offline()
  skip_on_ci()

  map5 <- map_risk(t_vals = bound, t_rast = ras, region = "Réunion",
                   path = folder, plot = FALSE)
  # plot(map5)
  expect_equal(terra::values(map5, row = 1, nrows = 1, mat = FALSE),
               c(rep(NA, times = 24), rep(0, 12), rep(NA, 38)))

})

test_that("map_risk should throw an error if t_vals include more than one
          TPC model", {
  expect_error(map_risk(t_vals = bounds2, t_rast = ras, region = "Réunion"),
               "`t_vals` must contain results from only one model. Please filter the dataset to choose a single model.",
               fixed = TRUE)
})

test_that("map_risk should throw an error if any tval_left is higher or equal
          than tval_right ", {
  bounds_error <- bounds |>
    dplyr::mutate(tval_right = c(21.7, 32, 31.8))
  expect_error(map_risk(t_vals = bounds_error, t_rast = ras, region = "Réunion"),
  "All 'tval_left' values must be less than or equal to 'tval_right'.",
  fixed = TRUE)
          })

test_that("using different types of 'region' returns correctly cropped raster", {

  # not testing with character vector of country names as it implies heavy download
  # providing a single country is tested above

  # numeric vector
  reg.num <- c(xmin = 55.5, xmax = 55.6, ymin = -21, ymax = -20.9)
  map7 <- map_risk(t_vals = bound, t_rast = tavg, region = reg.num, plot = FALSE)
  expect_equal(as.vector(terra::ext(map7)), reg.num)

  # extent
  reg.ext <- terra::ext(reg.num)
  map8 <- map_risk(t_vals = bound, t_rast = tavg, region = reg.ext, plot = FALSE)
  expect_equal(as.vector(terra::ext(map8)), reg.num)

  # SpatVector
  reg.vect <- terra::vect(reg.ext)
  map9 <- map_risk(t_vals = bound, t_rast = tavg, region = reg.vect, plot = FALSE)
  expect_equal(as.vector(terra::ext(map9)), reg.num)

})


test_that("no failure when plot = TRUE, and output is a SpatRaster", {

  skip_on_cran()

  expect_no_failure(map10 <- map_risk(t_vals = bound, t_rast = tavg, plot = TRUE))
  expect_no_failure(map11 <- map_risk(t_vals = bound, t_rast = tavg, interactive = TRUE))
  expect_no_failure(map12 <- map_risk(t_vals = bounds, t_rast = tavg, plot = TRUE))
  file.remove("Rplots.pdf")

  expect_true(inherits(map10, "SpatRaster"))
  expect_true(inherits(map11, "SpatRaster"))
  expect_true(inherits(map12, "SpatRaster"))

})


test_that("error is produced if mask is not logical", {

  expect_error(map_risk(t_vals = bound, t_rast = tavg, mask = 1))

})

test_that("error is produced if verbose is not logical", {

  expect_error(map_risk(t_vals = bound, t_rast = tavg, verbose = 1))

})

test_that("error is produced if plot is not logical", {

  expect_error(map_risk(t_vals = bound, t_rast = tavg, plot = 1))

})

test_that("error is produced if interactive is not logical", {

  expect_error(map_risk(t_vals = bound, t_rast = tavg, interactive = 1))

})

test_that("error is produced if t_vals data frame is not correct", {

  expect_error(map_risk(t_vals = bound[, 2:3], t_rast = tavg, plot = FALSE))

})

test_that("error is produced if country names are not correct", {
  regions_error <- c("Spa", "Po")
  expect_error(map_risk(t_vals = bound,
                        region = regions_error,
                        path = folder,
                        plot = FALSE),
 "Input region(s) Spa, Po not found. For available region names, run 'data(country_names); country_names'",
 fixed = TRUE)
})

test_that("the function returns an error when providing an invalid numeric vector for region", {
  expect_error(map_risk(t_vals = bound, t_rast = tavg, region = c(-10, 10, 30)))
})


test_that(" the function returns an error when no path to download data is given
            and raster of temperatures neither", {
  expect_error(map_risk(t_vals = bound,
                        region = "Réunion"),
               "Please provide an existing 'path' to save the downloaded temperature maps.",
               fixed = TRUE)
})


test_that("t_rast should be a SpatRaster with 12 layers", {
  expect_error(map_risk(t_vals = bound, t_rast = as.matrix(tavg)))
  expect_error(map_risk(t_vals = bound, t_rast = subset(tavg, 1)))
})


test_that("no overlapping yields an error for a extent", {
  expect_error(map_risk(t_vals = bound, t_rast = tavg, region = c(0, 1, 0, 1)),
    "There's no overlap between 'region' and 't_rast'.")
})

test_that("t_rast should drop a messages", {
  expect_error(map_risk(t_vals = bound, t_rast = as.matrix(tavg)))
  expect_error(map_risk(t_vals = bound, t_rast = subset(tavg, 1)))
})



