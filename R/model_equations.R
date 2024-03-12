
# functions developed based on devRate formulas
wang <- function(temp, k, r, topt, tmin, tmax, a){
  est <- (k/(1 + exp(-r * (temp - topt)))) * (1 - exp(-(temp - tmin)/a)) *
    (1 - exp(-(tmax - temp)/a))
  return(est)
}
mod_polynomial <- function(temp, a_0, a_1, a_2, a_3, a_4){
  est <- a_0 + a_1 * temp + a_2 * temp^2 + a_3 * temp^3 + a_4 * temp^4
  return(est)
}

janisch <- function(temp, topt, dmin, a, b){
  est <- ((dmin/2 * (exp(a * (temp - topt)) + exp(-b * (temp - topt))))^(-1))
  return(est)
}

briere1 <- function(temp, tmin, tmax, a) {
  est <- a * temp * (temp - tmin) * (tmax - temp)^(1/2)
  return(est)
}

lactin1 <- function(temp, a, tmax, delta_t) {
  est <- exp(a * temp) - exp(a * tmax - (tmax - temp)/delta_t)
  return(est)
}

regniere <- function(temp,tmin, tmax, phi, delta_b, delta_m, b) {
 est <- phi* (exp(b * (temp - tmin)) - ((tmax - temp)/(tmax - tmin)) * exp(-b *
   (temp - tmin)/delta_b) - ((temp - tmin)/(tmax - tmin)) * exp(b * (tmax - tmin) - (tmax - temp)/delta_m))
return(est)
 }


