
wang <- function(temp, k, r, topt, tmin, tmax, a){
  if (all(sapply(list(temp, k, r, topt, tmin, tmax, a), is.numeric))
) {
    est <- (k/(1 + exp(-r * (temp - topt)))) * (1 - exp(-(temp - tmin)/a)) *
      (1 - exp(-(tmax - temp)/a))

    return(est)
  } else { stop("Non-numeric inputs for `wang` model. Model discarded.") }
}

mod_polynomial <- function(temp, a_0, a_1, a_2, a_3, a_4){
  if (all(sapply(list(temp, a_0, a_1, a_2, a_3, a_4), is.numeric))) {
      est <- a_0 + a_1 * temp + a_2 * temp^2 + a_3 * temp^3 + a_4 * temp^4
  return(est)
  } else { stop("Non-numeric inputs for `mod_polynomial` model. Model discarded.") }
}

briere1 <- function(temp, tmin, tmax, a) {
  est <- a * temp * (temp - tmin) * (tmax - temp)^(1/2)
  return(est)
}

lactin1 <- function(temp, a, tmax, delta_t) {
  est <- exp(a * temp) - exp(a * tmax - (tmax - temp)/delta_t)
  return(est)
}




