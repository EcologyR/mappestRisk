check_data <- function(temp = NULL, dev_rate = NULL) {

  if (any(is.na(dev_rate))) {
    stop("development rate data have NAs; please consider removing them or fixing them")
  }
  if (any(is.na(temp))) {
    stop("temperature data have NAs; please consider removing them or fixing them")
  }
  if (!is.numeric(temp)) {
    stop("temperature data is not numeric. Please check it.")
  }
  if (length(unique(temp)) < 4) {
    stop("At least four different temperature treatments in the data are required.")
  }
  if (!is.numeric(dev_rate)) {
    stop("development rate data is not numeric. Please check it.")
  }
  if (length(temp) != length(dev_rate)) {
    stop("development rate and temperature inputs are not of same length. Please check it.")
  }

  if (any(dev_rate < 0)) {
    stop("Negative dev_rate development rate data found. Please check it.")
  }
  if (any(dev_rate > 10)) {
    stop("Extremely high values of dev_rate development rate data might contain a typo error. Please check it.")
  }

  if (any(temp < -10) | any(temp > 56)) {
    stop("experienced temperatures by active organisms are usually between 0 and 50 degrees centigrades")
  }

}
