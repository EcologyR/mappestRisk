# model fitting, selection and traits inference


# 1. development rate equations -------------------------------------------
library(rTPC)
# get parameters
extract_param_names <- function(nls_object){
  parameter_est <- coef(nls_object)
  param_names <- names(parameter_est)
  return(param_names)
}

## from rTPC package: equations and structures
briere2 <- function (temp, tmin, tmax, a, b) {
  est <- a * temp * (temp - tmin) * (tmax - temp)^(1/b)
  return(est)
}

mod_gaussian <- function (temp, rmax, topt, a) {
  est <- rmax * exp(-0.5 * (abs(temp - topt)/a)^2)
  return(est)
}

mod_weibull <- function (temp, a, topt, b, c) {
  return((a * (((c - 1)/c)^((1 - c)/c)) * ((((temp - topt)/b) +
                                              (((c - 1)/c)^(1/c)))^(c - 1)) * (exp(-((((temp - topt)/b) +
                                                                                        (((c - 1)/c)^(1/c)))^c) + ((c - 1)/c)))))
}

lactin2 <- function (temp, a, b, tmax, delta_t) {
  est <- exp(a * temp) - exp(a * tmax - ((tmax - temp)/delta_t)) +  b
  return(est)
}

rezende <- function (temp, q10, a, b, c) {
  est <- {
    ifelse(temp < b,
           (a * 10^(log10(q10)/(10/temp))),
           (a * 10^(log10(q10)/(10/temp))) * (1 - c * (b - temp)^2))
  }
  return(est)
}
ssi <- function (temp, r_tref, e, el, tl, eh, th, tref) {
  tref <- 273.15 + tref
  k <- 8.62e-05
  boltzmann.term <- r_tref * exp(e/k * (1/tref - 1/(temp +273.15)))
  inactivation.term <- 1/(1 + exp(-el/k * (1/(tl + 273.15) - 1/(temp + 273.15)))
                          + exp(eh/k * (1/(th + 273.15) -   1/(temp + 273.15))))
  return(boltzmann.term * inactivation.term)
}

ratkowsky <- function (temp, tmin, tmax, a, b)
{
  est <- ((a * (temp - tmin)) * (1 - exp(b * (temp - tmax))))^2
  return(est)
}

# from devRate equations
logan10 <- function (temp, tmax, b, phi, delta_t){
  est <- (phi * (exp(b * x) - exp(b * tmax - (tmax - temp)/delta_t)))
  return(est)
}

mod_polynomial <- function (temp, a_0, a_1, a_2, a_3, a_4){
  est <- a_0 + a_1 * temp + a_2 * temp^2 + a_3 * temp^3 + a_4 * temp^4
  return(est)
}

janisch <- function (temp, topt, dmin, a, b){
  est <- ((dmin/2 * (exp(a * (T - topt)) + exp(-b * (T - Topt))))^(-1))
  return(est)
}

briere1 <- function (temp, tmin, tmax, a) {
  est <- a * temp * (temp - tmin) * (tmax - temp)^(1/2)
  return(est)
}

linear_campbell <- function (temp, intercept, slope) {
  est <- slope*temp + intercept
  return(est)
}

lactin1 <- function (temp, a, tmax, delta_t) {
  est <- exp(a * temp) - exp(a * tmax - (tmax - temp)/delta_t)
  return(est)
}

taylor <- function(temp, r_m, tm)
# 2. Model table ----------------------------------------------------------

model_names <- c("briere1", "briere2", "mod_gaussian", "janisch",
                 "lactin1", "lactin2", "linear_campbell", "logan10",
                 "mod_polynomial", "ratkowsky", "rezende", "ssi", "mod_weibull")

pkg_model_names <- c("briere1_99", "briere2_1999", "gaussian_1987", "janisch_32",
                     "lactin1_95", "lactin2_1995", "campbell_74", "logan10_76",
                     "poly4", "ratkowsky_1983", "rezende_2019",
                     "sharpeschoolfull_1981", "weibull_1995")
dev_model_table <- tibble(model_name = model_names) |>
  mutate(package = if_else(model_name %in% c("briere1", "lactin1",
                                                    "logan10", "janisch",
                                                    "mod_polynomial", "linear_campbell"),
                                  "devRate",
                                  "rTPC"),
         source_model_name = pkg_model_names)
devRate::taylor_81

# 3. Start Values ---------------------------------------------------------
model_name_translator <- function(user_model_name){
  model_eq <- dev_model_table |>
    filter(model_name == user_model_name) |>
    select(source_model_name) |>
    pull()
  if(length(model_eq)==0) {
    stop("model name not available. See list of model names in curve_fitting vignette")
  }
  return(model_eq)

}

model_name_translator("mod_gaussian")

start_vals_rtpc <- function(model_name, dev_rate, temp){
  start_vals <- rTPC::get_start_vals(x = 20,
                                     y = 0.45,
                                     model_name = model_name)
}

