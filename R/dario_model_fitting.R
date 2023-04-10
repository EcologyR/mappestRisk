# model fitting, selection and traits inference


# 0. example data ---------------------------------------------------------
aphis_tsai1999_simexample <- readRDS(file = here::here("data/a.citricidus_tsai1999.rds"))

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
logan6 <- function (temp, tmax, b, phi, delta_t){
  est <- phi * (exp(b * temp) - exp(b * tmax - (tmax - temp)/delta_t))
  return(est)
}
mod_polynomial <- function (temp, a_0, a_1, a_2, a_3, a_4){
  est <- a_0 + a_1 * temp + a_2 * temp^2 + a_3 * temp^3 + a_4 * temp^4
  return(est)
}

janisch <- function (temp, topt, dmin, a, b){
  est <- ((dmin/2 * (exp(a * (temp - topt)) + exp(-b * (temp - topt))))^(-1))
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

# 2. Model table ----------------------------------------------------------

model_names <- c("briere1", "briere2", "mod_gaussian", "janisch",
                 "lactin1", "lactin2", "linear_campbell", "logan6",
                 "mod_polynomial", "ratkowsky", "rezende", "ssi", "mod_weibull")

pkg_model_names <- c("briere1_99", "briere2_1999", "gaussian_1987", "janisch_32",
                     "lactin1_95", "lactin2_1995", "campbell_74", "logan6_76",
                     "poly4", "ratkowsky_1983", "rezende_2019",
                     "sharpeschoolfull_1981", "weibull_1995")
dev_model_table <- tibble(model_name = model_names) |>
  mutate(package = if_else(model_name %in% c("briere1", "lactin1",
                                             "logan6", "janisch",
                                             "mod_polynomial", "linear_campbell"),
                           "devRate",
                           "rTPC"),
         source_model_name = pkg_model_names)

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

## start_vals rTPC function & example
start_vals_rtpc <- function(model_name, temperature, dev_rate){
  start_vals <- rTPC::get_start_vals(x = temperature,
                                     y = dev_rate,
                                     model_name = model_name_translator(model_name))
  return(start_vals)
}
dev_model_table
start_vals_rtpc(model_name = "ssi",
                temp = aphis_tsai1999_simexample$temperature,
                dev_rate = aphis_tsai1999_simexample$rate_development)

## start_vals devRate function & example
startvals_names_translate_devrate <- function(start_vals, model_name){
  start_vals_names <- if(model_name == "briere1"){
    c("a", "tmin", "tmax")
  } else if (model_name == "lactin1"){
    c("a", "tmax", "delta_t")
  } else if (model_name == "janisch"){
    c("dmin", "topt", "a", "b")
  } else if (model_name == "linear_campbell"){
    c("intercept", "slope")
  } else if (model_name == "logan6"){
    c("phi", "b", "tmax", "delta_t")
  } else if (model_name == "mod_polynomial") {
    c("a_0", "a_1", "a_2", "a_3", "a_4")
  } else (NA)

  return(start_vals_names)
}

start_vals_devRate <- function(model_name, temperature, dev_rate){
  start_vals_prev <- devRate::devRateEqStartVal[[model_name_translator(model_name)]]
  names(start_vals_prev) <- startvals_names_translate_devrate(start_vals = start_vals_prev_devrate,
                                                              model_name)
  grid_startvals <- map(.x = start_vals_prev,
                        .f = ~runif(10, min = .x - .x,
                                    max = .x + .x))
  exp_grid_startvals <- expand.grid(grid_startvals)
  devdata <- tibble(temp = temperature,
                    dev_rate = dev_rate,
                    model_name = model_name,
                    formula = case_when(model_name == "briere1" ~  "briere1(temp,a, tmin, tmax)",
                                        model_name == "lactin1" ~ "lactin1(temp, a, tmax, delta_t)",
                                        model_name == "janisch" ~ "janisch(temp, topt, dmin, a, b)",
                                        model_name == "linear_campbell" ~ "linear_campbell(temp,intercept, slope)",
                                        model_name == "logan6" ~ "logan6(temp, tmax, b, phi, delta_t)",
                                        model_name == "mod_polynomial" ~ "mod_polynomial(temp, a_0, a_1, a_2, a_3, a_4)"
                    )
  )
  start_vals_fit <- nls2::nls2(reformulate(response = "dev_rate", termlabels = unique(devdata$formula)) ,
                               data = devdata,
                               start = exp_grid_startvals,
                               algorithm = "brute-force",
                               trace = FALSE,
                               control = nls.control(tol = 1e-01))
  sum_start_vals_fit <- summary(start_vals_fit)
  if(is.null(start_vals_fit)){start_vals_explore <- tibble(param_name = names(start_vals_prev),
                                                           start_value = NA,
                                                           model_name = model_name)}
  else {start_vals_names <- extract_param_names(start_vals_fit)
  start_vals <- sum_start_vals_fit$parameters[1:length(start_vals_names),1]
  start_vals_explore <- tibble(param_name = start_vals_names,
                               start_value = start_vals,
                               model_name = model_name) |>
    pull(start_value)}
  return(start_vals_explore)
}
ex_lactin1 <- start_vals_devRate(model_name = "lactin1",
                   temp = aphis_tsai1999_simexample$temperature,
                   dev_rate = aphis_tsai1999_simexample$rate_development)

fit_vals <- function(temp, dev_rate){
  predict2fill <- tibble(temp = NULL,
                         dev_rate = NULL,
                         model_name = NULL)
  for(i in c("lactin1", "lactin2", "rezende", "mod_weibull", "logan6")){
    model_i <- dev_model_table |> filter(model_name == i)
    if(model_i$package == "devRate") {
      start_vals <- start_vals_devRate(model_name = i, temperature = temp, dev_rate = dev_rate)
      } else if (model_i$package == "rTPC") {
      start_vals <- start_vals_rtpc(model_name = i, temperature = a.citricidus_tsai1999$temperature,
                                         dev_rate = a.citricidus_tsai1999$rate_development)
      }
    explore_preds <- tibble(temp = seq(0,50,.001),
                              model_name = i,
                            start_vals = list(start_vals))
    fit_vals_tbl <- explore_preds |>
      select(temp, model_name) |>
   mutate(formula = case_when(model_name == "briere1" ~  "briere1(.x, start_vals[1], start_vals[2], start_vals[3])",
                              model_name == "lactin1" ~ "lactin1(.x, start_vals[1], start_vals[2], start_vals[3])",
                              model_name == "janisch" ~ "janisch(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                              model_name == "linear_campbell" ~ "linear_campbell(.x, start_vals[1], start_vals[2])",
                              model_name == "logan6" ~ "logan6(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                              model_name == "mod_polynomial" ~ "mod_polynomial(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                              model_name == "briere2" ~ "briere2(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                              model_name == "mod_gaussian" ~ "mod_gaussian(.x, start_vals[1], start_vals[2], start_vals[3])",
                              model_name == "lactin2" ~ "lactin2(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                              model_name == "ratkowsky" ~ "ratkowsky(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                              model_name == "rezende" ~ "rezende(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                              model_name == "ssi" ~ "ssi(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4], start_vals[5], start_vals[6], start_vals[7])",
                              model_name == "mod_weibull" ~ "mod_weibull(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])"
                              )) |>
  mutate(preds = map_dbl(.x = temp,
                         .f = reformulate(unique(formula)))) |>
  filter(preds >= 0) |>
    select(-formula)
  predict2fill <- predict2fill |> bind_rows(fit_vals_tbl)
  }
  return(predict2fill)
}

example <- fit_vals(temp = a.citricidus_tsai1999$temperature,
                    dev_rate = a.citricidus_tsai1999$rate_development)

view(example)




  ggplot(explore_preds, aes(x = temp, y = preds))+
    geom_point(data = a.citricidus_tsai1999,
               aes(x = temperature, y = rate_development),
               color = "gray68",
               alpha = .3)+
    geom_point(aes(color = model_name))+
    facet_wrap(.~model_name)+
    theme_minimal()+
    theme(legend.position = "none")


lactin2()
devdata <- tibble(temp = temperature,
                  dev_rate = dev_rate,
                  model_name = model_name,
                  formula = case_when(model_name == "briere1" ~  "briere1(temp,a, tmin, tmax)",
                                      model_name == "lactin1" ~ "lactin1(temp, a, tmax, delta_t)",
                                      model_name == "janisch" ~ "janisch(temp, topt, dmin, a, b)",
                                      model_name == "linear_campbell" ~ "linear_campbell(temp,intercept, slope)",
                                      model_name == "logan6" ~ "logan6(temp, tmax, b, phi, delta_t)",
                                      model_name == "mod_polynomial" ~ "mod_polynomial(temp, a_0, a_1, a_2, a_3, a_4)"
                  )
)
start_vals_fit <- nls2::nls2(reformulate(response = "dev_rate", termlabels = unique(devdata$formula))












exploratory_curve_fitting <- function (temp, dev_rate) {
  devdata <- data.frame(temperature = temp,
                        rate_development = dev_rate)
  predict2fill <- tibble(temp = NULL,
                         dev_rate = NULL,
                         model_name = NULL)
  for(i in model_names){
    model_i <- dev_model_table |> filter(model_name == i)
    if(model_i$package == "devRate") {
      start_vals_prev <- start_vals_devRate(model_name = i, temperature = temp, dev_rate = dev_rate)
      prev_fitting <- tibble(temp = seq(0,50,.001),
                             dev_rate = case_when(i == "briere1" ~ briere1(temp,
                                                                        a = start_vals_prev$aa,
                                                                        tmin = start_vals_prev$Tmin,
                                                                        tmax = start_vals_prev$Tmax),
                                               i == "lactin1" ~ lactin1(temp,
                                                                        a = start_vals_prev$aa,
                                                                        tmax = start_vals_prev$Tmax,
                                                                        delta_t = start_vals_prev$deltaT),
                                               i == "janisch" ~ janisch(temp,
                                                                        topt = start_vals_prev$Topt,
                                                                        dmin = start_vals_prev$Dmin,
                                                                        a = start_vals_prev$aa,
                                                                        b =start_vals_prev$bb),
                                               i == "linear_campbell" ~ linear_campbell(temp,
                                                                                        intercept = start_vals_prev$aa,
                                                                                        slope = start_vals_prev$bb),
                                               i == "logan6" ~ logan6(temp,
                                                                      tmax = start_vals_prev$Tmax,
                                                                      b = start_vals_prev$bb,
                                                                      phi = start_vals_prev$phi,
                                                                      delta_t = start_vals_prev$deltaT),
                                               i == "mod_polynomial" ~ mod_polynomial(temp,
                                                                                      a_0 = start_vals_prev$a0,
                                                                                      a_1 = start_vals_prev$a1,
                                                                                      a_2 = start_vals_prev$a2,
                                                                                      a_3 = start_vals_prev$a3,
                                                                                      a_4 = start_vals_prev$a4)
                                               ),
                             model_name == i,
                             )
      prev_fitting_positive <- prev_fitting |> filter(dev_rate >= 0)
      predict2fill <- predict2fill |> bind_rows(prev_fitting_positive)
    } else (model_i$package == "rTPC") {
      start_vals_prev <- start_vals_rtpc(model_name = i)
      }

  }

  return(start_vals)
}

list_parameters_startvals <- tibble(param_name = NULL,
                                    start_value = NULL,
                                    model_name = NULL)
devrate_mods <- dev_model_table |> filter(package == "devRate") |> distinct(model_name) |> pull()
for(i in devrate_mods){
  start_vals_model <- start_vals_devRate(model_name = i,
                                         temperature = trioza_aidoo22_simexample$temperature,
                                         rate_dev = trioza_aidoo22_simexample$rate_development)
  list_parameters_startvals <- list_parameters_startvals |>
    bind_rows(start_vals_model)
}


list_parameters_startvals


start_vals_rtpc(model_name = "briere2",
                temp = trioza_aidoo22_simexample$temperature,
                dev_rate = trioza_aidoo22_simexample$rate_development)
