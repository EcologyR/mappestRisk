data("aphid")

# test check_data()

test_that("check_data should throw an error if temperature data is not numeric", {

  expect_error(check_data(temp = as.character(aphid$temperature),
                          dev_rate = aphid$rate_value),
               "temperature data is not numeric. Please check it.")
})

## test that rate of development data have NAs
test_that("check_data should stop the function and advise the user about NAs in the data set", {

  expect_error(check_data(temp = c(aphid$temperature, 10),
                             dev_rate = c(aphid$rate_value, NA)),
               "development rate data have NAs; please consider removing them or fixing them")
})

## test that get error if temperature data have NAs
test_that("check_data should stop the function and advise the user about NAs in the data set", {

  expect_error(check_data(temp = c(aphid$temperature, NA),
                             dev_rate = aphid$rate_value),
               "temperature data have NAs; please consider removing them or fixing them")
})


test_that("check_data should throw an error if temperature data is a data.frame", {

  expect_error(check_data(temp = data.frame(temperature = seq(4, 40, 3),
                                               temp_error = runif(13, 0, 2)),
                             dev_rate = aphid$rate_value),
               "temperature data is not numeric. Please check it.")
})



test_that("check_data should throw an error if temperature data have just three values", {

  expect_error(check_data(temp = c(15, 20, 25),
                             dev_rate = aphid$rate_value),
               "At least four different temperature treatments in the data are required.",
               fixed = TRUE)
})

test_that("check_data should throw an error if development rate data is not numeric
          (e.g. incorrectly importing data from csv/xlsx, using commas as decimal markers, etc)", {

            expect_error(check_data(temp = aphid$temperature,
                                       dev_rate = as.character(aphid$rate_value)),
                         "development rate data is not numeric. Please check it.")
          })



test_that("check_data should throw an error if temperature and development rate inputs are not of same length", {

  expect_error(check_data(temp = seq(4, 40, 3),
                             dev_rate = seq(0, 1, by = 0.1)),
               "development rate and temperature inputs are not of same length. Please check it.")
})

test_that("check_data should throw an error if development rate is negative, which is biologically unrealistic", {
  expect_error(check_data(temp = aphid$temperature,
                             dev_rate = -1*aphid$rate_value),
               "Negative dev_rate development rate data found. Please check it.")
})


test_that("check_data should throw an error if development rate is too high", {
  expect_error(check_data(temp = aphid$temperature,
                          dev_rate = aphid$rate_value*100),
               "Extremely high values of dev_rate development rate data might contain a typo error. Please check it.")
})


test_that("check_data should throw an error if temperature data contains values outside of the range of active organisms", {

  expect_error(check_data(temp = c(seq(4, 39, 3), 4000),
                             dev_rate = seq(0, 1, length.out = 13)),
               "experienced temperatures by active organisms are usually between 0 and 50 degrees centigrades",
               fixed = TRUE)
})





