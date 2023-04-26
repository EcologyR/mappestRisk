# model fitting, selection and traits inference


# 1. development rate equations -------------------------------------------
library(rTPC)
library(devRate)
library(tidyverse) # <- tal vez sea mejor cargar los paquetes que hagan falta en vez de todo tidyverse (dplyr, readr, tidyr, ggplot2, tibble, purrr)
library(nlme)
library(devRate)

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

lactin2 <- function (temp, a, tmax, delta_t, b) {
  est <- exp(a * temp) - exp(a * tmax - ((tmax - temp)/delta_t)) + b
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
wang <- function (temp, k, r, topt, tmin, tmax, a){
  est <- (k/(1 + exp(-r * (temp - topt)))) * (1 - exp(-(temp - tmin)/a)) *
    (1 - exp(-(tmax - temp)/a))
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
### build a table with user-friendly arguments for model_names and their
### equivalent names in source helping packates (devRate & rTPC)
model_names <- c("briere1", "briere2", "mod_gaussian", "janisch",
                 "lactin1", "lactin2", "linear_campbell", "wang",
                 "ratkowsky", "rezende", "ssi", "mod_weibull", "mod_polynomial")

pkg_model_names <- c("briere1_99", "briere2_1999", "gaussian_1987", "janisch_32",
                     "lactin1_95", "lactin2_95", "campbell_74", "wang_82",
                     "ratkowsky_1983", "rezende_2019",
                     "sharpeschoolfull_1981", "weibull_1995", "poly4")
dev_model_table <- tibble(model_name = model_names) |>
  mutate(package = if_else(model_name %in% c("briere1", "lactin1", "lactin2",
                                             "wang","janisch",
                                             "linear_campbell", "mod_polynomial"),
                           "devRate",
                           "rTPC"),
         source_model_name = pkg_model_names) #table for equivalencies
list_available_models <- dev_model_table |> pull(model_name)
rm(pkg_model_names)
rm(model_names)
save(list_available_models, file = here::here("data/list_available_models.rda"))

# 3. Auxiliary functions ---------------------------------------------------------
#### a) names functions ----

## translate user-friendly input names for models into auxiliary packages input names
model_name_translate<- function(user_model_name){
  model_eq <- dev_model_table |>
    filter(model_name == user_model_name) |>
    select(source_model_name) |>
    pull()
  if(length(model_eq)==0) {
    stop("model name not available. For available model names, run 'data(list_available_models); list_available_models'.")
  }
  return(model_eq)

}

# take names from a fitted model to assign them as names for start values later
extract_param_names <- function(nls_object){
  parameter_est <- coef(nls_object)
  param_names <- names(parameter_est)
  return(param_names)
}

## change names of parameters in devRate to easier argument names of parameters
startvals_names_translate_devrate <- function(start_vals, model_name){
  start_vals_names <- if(model_name == "briere1"){
    c("tmin", "tmax", "a")
  } else if (model_name == "lactin1"){
    c("a", "tmax", "delta_t")
  } else if (model_name == "lactin2"){
    c("a", "tmax", "delta_t", "b")
  } else if (model_name == "janisch"){
    c("dmin", "topt", "a", "b")
  } else if (model_name == "linear_campbell"){
    c("intercept", "slope")
  } else if (model_name == "wang"){
    c("k", "r", "topt", "tmin", "tmax", "a")
  } else if (model_name == "mod_polynomial") {
    c("a_0", "a_1", "a_2", "a_3", "a_4")
  } else (NA)

  return(start_vals_names)
}

#### b) obtain start values for model fitting ----

## start_vals rTPC function, defined by the package (rTPC::get_start_vals) to adapt starting values to the input data
## so that they are more informative

start_vals_rtpc <- function(model_name, temperature, dev_rate){
  start_vals <- rTPC::get_start_vals(x = temperature,
                                     y = dev_rate,
                                     model_name = model_name_translate(model_name))
  return(start_vals)
}

## start_vals devRate function & example. Since the devRate package does not provide a function for start values,
## we use nls2::nls2() function with literature starting values given in devRate packate to iteratively calculate parameters given the input data
## and use them later as starting values for nonlinear regression

start_vals_devRate <- function(model_name, temperature, dev_rate){
    if (model_name == "wang"){ ## it has 6 parameters, which is excessively time costly, we'll accept use the generic literature start values from devRate package
      start_vals <- devRate::devRateEqStartVal[[model_name_translate(model_name)]] # take literature start values from devRate
      names(start_vals) <- startvals_names_translate_devrate(start_vals = start_vals,
                                                             model_name)
      start_vals_explore <- tibble(param_name = c("k", "r", "topt", "tmin", "tmax", "a"),
                                   start_value = unlist(start_vals),
                                   model_name = model_name) |>
        pull(start_value)
      print("poorly informative starting values for Wang fitting")

    } else {
      if (model_name == "briere1") {
      start_vals_prev <- c(tmin = 6, tmax = 32, a = 1e-04) # devRate start values are not representative, especially the a parameter, we use them manually
      names(start_vals_prev) <- startvals_names_translate_devrate(start_vals = start_vals_prev,
                                                                  model_name)
    } else {
      start_vals_prev <- devRate::devRateEqStartVal[[model_name_translate(model_name)]] # take literature start values from devRate
      names(start_vals_prev) <- startvals_names_translate_devrate(start_vals = start_vals_prev,
                                                                  model_name)
    }
  set.seed(2023) #to ensure reproducibility
  grid_startvals <- map(.x = start_vals_prev,
                        .f = ~runif(10, min = .x - .x/2,
                                    max = .x + .x/2)) #grid with different combinations of possible starting values to expand next
  exp_grid_startvals <- expand.grid(grid_startvals) #expanded grid with combinatory to iteratively adjust models
  devdata <- tibble(temp = temperature,
                    dev_rate = dev_rate,
                    model_name = model_name,
                    formula = case_when(model_name == "briere1" ~  "briere1(temp, tmin, tmax, a)",
                                        model_name == "lactin1" ~ "lactin1(temp, a, tmax, delta_t)",
                                        model_name == "lactin2" ~ "lactin2(temp, a, tmax, delta_t, b)",
                                        model_name == "janisch" ~ "janisch(temp, dmin, topt, a, b)",
                                        model_name == "linear_campbell" ~ "linear_campbell(temp,intercept, slope)",
                                        model_name == "wang" ~ "wang(temp, k, r, topt, tmin, tmax, a)",
                                        model_name == "mod_polynomial" ~ "mod_polynomial(temp, a_0, a_1, a_2, a_3, a_4)"
                    )
  )
  capture.output(type = "message",
                 start_vals_fit <- try(nls2::nls2(reformulate(response = "dev_rate", termlabels = unique(devdata$formula)) ,
                                                  data = devdata,
                                                  start = exp_grid_startvals,
                                                  algorithm = "brute-force",
                                                  trace = FALSE,
                                                  control = nls.control(tol = 1)),
                                       silent=TRUE) #try(object, silent = TRUE) to avoid printing usual uninformative errors
                 )
  sum_start_vals_fit <- summary(start_vals_fit)
  if(is.null(start_vals_fit)){start_vals_explore <- tibble(param_name = names(start_vals_prev),
                                                           start_value = unlist(start_vals_prev),
                                                           model_name = model_name) |>
    pull(start_value)

  warning("generic starting values")
  }
  else {start_vals_names <- extract_param_names(start_vals_fit)
  start_vals <- sum_start_vals_fit$parameters[1:length(start_vals_names),1]
  start_vals_explore <- tibble(param_name = start_vals_names,
                               start_value = start_vals,
                               model_name = model_name) |>
    pull(start_value)}
  }
  return(start_vals_explore)
}


# 4. Core functions for model fitting & selection  ---------------------------------------------------------

#### a) fit_models function ----
fit_models <- function(temp, dev_rate, models){
  list_param <- tibble(param_name = NULL,
                       start_vals = NULL,
                       param_est = NULL,
                       param_se = NULL,
                       model_name = NULL,
                       model_AIC = NULL)
  if(models[1] == "all") { # it will be probably the most commonly used option for user's experience
    model_names <- c("briere1", "briere2", "mod_gaussian", "janisch",
                "lactin1", "lactin2", "linear_campbell", "wang",
                "ratkowsky", "rezende", "ssi", "mod_weibull", "mod_polynomial")
  } else {
      model_names <- models
    }
  for(i in model_names){
    model_i <- dev_model_table |> filter(model_name == i)
    print(paste0("fitting model ", i)) # to let people know that the function is working and R is not crashing
    if (model_i$package == "devRate") {
      start_vals <- start_vals_devRate(model_name = i,
                                       temperature = temp,
                                       dev_rate = dev_rate)
      } else if (model_i$package == "rTPC") {
      start_vals <- start_vals_rtpc(model_name = i,
                                    temperature = temp,
                                    dev_rate = dev_rate)
      }
    startvals_tbl <- tibble(temp = seq(0,50,.001),
                            model_name = i,
                            start_vals = list(start_vals))
    start_formula_tbl <- startvals_tbl |>
      select(temp, model_name) |>
   mutate(formula = case_when(model_name == "briere1" ~  "briere1(temp, tmin, tmax, a)",
                              model_name == "lactin1" ~ "lactin1(temp, a, tmax, delta_t)",
                              model_name == "janisch" ~ "janisch(temp, topt, dmin, a, b)",
                              model_name == "linear_campbell" ~ "linear_campbell(temp,intercept, slope)",
                              model_name == "wang" ~ "wang(temp, k, r, topt, tmin, tmax, a)",
                              model_name == "mod_polynomial" ~ "mod_polynomial(temp, a_0, a_1, a_2, a_3, a_4)",
                              model_name == "briere2" ~ "briere2(temp, tmin, tmax, a, b)",
                              model_name == "mod_gaussian" ~ "mod_gaussian(temp, rmax, topt, a)",
                              model_name == "lactin2" ~ "lactin2(temp, a, tmax, delta_t, b)",
                              model_name == "ratkowsky" ~ "ratkowsky(temp, tmin, tmax, a, b)",
                              model_name == "rezende" ~ "rezende(temp, q10, a, b, c)",
                              model_name == "ssi" ~ "ssi(temp, r_tref, e, el, tl, eh, th, tref = 20)",
                              model_name == "mod_weibull" ~ "mod_weibull(temp, a, topt, b, c)"
                              )
          )
    devdata <- tibble(temp = temp,
                      dev_rate = dev_rate)
    ## then fit model with nlme::gnls function
    fit_gnls <- gnls(model = reformulate(response = "dev_rate",
                                         termlabels = unique(start_formula_tbl$formula)),
                     data = devdata,
                     start = replace_na(start_vals, 0), #to avoid error if start values compute a NA, probably not converging
                     na.action = na.exclude, #to avoid problems in the model
                     weights = varExp(form = ~temp), #usually development at higher temperatures has higher variability due to higher mortality
                     control = gnlsControl(maxIter = 100,
                                           nlsTol = 1e-07,
                                           returnObject = TRUE)
                     )
    if (is.null(fit_gnls)){ #means that it has not converged, we'll return a NA
      list_param_tbl <- tibble(param_name = names(start_vals),
                               start_vals = replace_na(start_vals, 0),
                               param_est = NA,
                               param_se = NA,
                               model_name = i,
                               model_AIC = NA)
      list_param <- list_param |> bind_rows(list_param_tbl)
    warning(paste0("model ", i, " did not converge"))
    }
    else {
      sum_fit_gnls <- summary(fit_gnls)
      list_param_tbl <- tibble(param_name = names(coef(fit_gnls)),
                               start_vals = replace_na(start_vals, 0),
                               param_est = fit_gnls$coefficients,
                               param_se = sum_fit_gnls$tTable[1:length(fit_gnls$coefficients), 2],
                               model_name = i,
                               model_AIC = sum_fit_gnls$AIC
                               )
      list_param <- list_param |> bind_rows(list_param_tbl)
      list_param <- list_param |>
        mutate(preds = map2_dbl(.x = start_vals,
                                    .y = param_est,
                                    .f = ~if_else(.x == .y,
                                                  NA_real_,
                                                  .y)))
    }
  }
list_param <- list_param |> drop_na() # exclude false convergence
  return(list_param)
}

#### b) plot_fitted function ----

## plot_fitted
## to exploratory fit:
plot_devmodel <- function(temp, dev_rate, param_tbl){
  devdata <- tibble (temperature = temp,
                     development_rate = dev_rate)
  fitted_tbl <- param_tbl |> drop_na()
  predict2fill <- tibble(temp = NULL,
                         dev_rate = NULL,
                         model_name = NULL,
                         model_AIC = NULL)
  model_names2plot <- fitted_tbl |> distinct(model_name) |> pull(model_name)
  for(i in model_names2plot){
    fitted_tbl_i <- fitted_tbl |> filter(model_name == i)
    model_AIC_i <-fitted_tbl_i |> pull(model_AIC)
    params_i <- fitted_tbl_i |> pull(param_est)

    ##predict based on parameters
    explore_preds <- tibble(temp = seq(min(devdata$temperature)-5,
                                       max(devdata$temperature) +5,
                                           .01),
                            model_name = i,
                            model_AIC = model_AIC_i[1],
                            preds = NULL,
                            n_params = length(params_i))
    fit_vals_tbl <- explore_preds |>
      select(temp, model_name, model_AIC, n_params) |>
      mutate(formula = case_when(model_name == "briere1" ~  "briere1(.x, params_i[1], params_i[2], params_i[3])",
                                 model_name == "lactin1" ~ "lactin1(.x, params_i[1], params_i[2], params_i[3])",
                                 model_name == "janisch" ~ "janisch(.x, params_i[1], params_i[2], params_i[3], params_i[4])",
                                 model_name == "linear_campbell" ~ "linear_campbell(.x, params_i[1], params_i[2])",
                                 model_name == "wang" ~ "wang(.x, params_i[1], params_i[2], params_i[3], params_i[4], params_i[5], params_i[6])",
                                 model_name == "mod_polynomial" ~ "mod_polynomial(.x, params_i[1], params_i[2], params_i[3], params_i[4], params_i[5])",
                                 model_name == "briere2" ~ "briere2(.x, params_i[1], params_i[2], params_i[3], params_i[4])",
                                 model_name == "mod_gaussian" ~ "mod_gaussian(.x, params_i[1], params_i[2], params_i[3])",
                                 model_name == "lactin2" ~ "lactin2(.x, params_i[1], params_i[2], params_i[3], params_i[4])",
                                 model_name == "ratkowsky" ~ "ratkowsky(.x, params_i[1], params_i[2], params_i[3], params_i[4])",
                                 model_name == "rezende" ~ "rezende(.x, params_i[1], params_i[2], params_i[3], params_i[4])",
                                 model_name == "ssi" ~ "ssi(.x, params_i[1], params_i[2], params_i[3], params_i[4], params_i[5], params_i[6], params_i[7])",
                                 model_name == "mod_weibull" ~ "mod_weibull(.x, params_i[1], params_i[2], params_i[3], params_i[4])"
      )) |>
      mutate(preds = map_dbl(.x = temp,
                             .f = reformulate(unique(formula)))) |>
      filter(preds >= 0) |>
      select(-formula) |>
      mutate(preds = case_when(model_name == "ratkowsky" & temp > params_i[2] ~ NA_real_,
                               model_name == "ratkowsky" & temp < params_i[1] ~ NA_real_,
                               model_name == "briere1" & temp < params_i[1] ~ NA_real_,
                               model_name == "briere2" & temp < params_i[1] ~ NA_real_,
                               TRUE ~ preds)
             ) # to exclude biological non-sense predictions due to model mathematical properties
    predict2fill <- predict2fill |> bind_rows(fit_vals_tbl)
  }

  aic_text <-  predict2fill %>%
    group_by(model_name) %>%
    summarise(aic = mean(model_AIC),
              n_params = paste(mean(n_params), "parameters")) %>%
    arrange(aic)
  aic_order <- aic_text %>%
    select(model_name) %>%
    as_vector()
  aic_values <- aic_text %>%
    mutate(aic =   paste("AIC =",
                           round(aic, 2)),
           temp = min(devdata$temperature),
           preds = 1.5*max(devdata$development_rate))
  ggplot_models <- ggplot()+
    geom_point(data = devdata, aes(x = temperature,
                                   y = development_rate),
               color = "gray62", alpha = .4)+
    geom_line(data = predict2fill |>
                filter(preds < (1.5*max(devdata$development_rate))),
              aes(x = temp, y = preds, color = model_name),
              linewidth = 1.3)+
    facet_wrap(~factor(model_name, levels = aic_order))+
    theme_light()+
    theme(legend.position = "none")+
    labs(x = "Temperature (ºC)", y = "Development rate (1/days)")+
    geom_label(data = aic_values,
              aes(label = aic,
                  x = temp,
                  y = preds,
                  fill = model_name),
              color = "white",
              size = 3)+
    geom_label(data = aic_values,
              aes(x = temp,
                  y = preds-0.05,
                  label = n_params,
                  fill = model_name),
              color = "white",
              size = 3)
  return(ggplot_models)
}



# 5. Examples ----

#   ##### a) Aphis citricidus (main example) ---------------------------------------------------------
#   a.citricidus_tsai1999 <- readRDS(file = here::here("data/a.citricidus_tsai1999.rds"))
#
#   ## fit models to development data and return a tibble to select parameters from the chosen model(s)
#   example_fit <- fit_models(temp = a.citricidus_tsai1999$temperature,
#                             dev_rate = a.citricidus_tsai1999$rate_development,
#                             models = "all")
#   print(example_fit)
#
#   ## visualize plots of the model to
#   plot_example <- plot_devmodel(temp = a.citricidus_tsai1999$temperature,
#                                 dev_rate = a.citricidus_tsai1999$rate_development,
#                                 param_tbl = example_fit)
#   print(plot_example)
#
#   ##### b) Pieris rapae  ---------------------------------------------------------
#   pieris_data <- read_delim("~/GitHub/PhenoBrassicaPests/Data/pieris_devdata/gilbert_pupa.csv",
#                             delim = ";") |>
#     rename(stage = ...3,
#            parasite = ...4,
#            reference = interact) |>
#     filter(stage == "pupa")
#   ex_pieris <- fit_models(temp = pieris_data$temperature,
#                         dev_rate = pieris_data$devrate,
#                         models = "all")
#   plot_devmodel(temp = pieris_data$temperature,
#                 dev_rate = pieris_data$devrate,
#                 param_tbl = ex_pieris)
#
#   ##### c) Diaphorina citri ---------------------------------------------------------
#
#   d.citri_liu2000 <- readRDS(file = here::here("data/d.citri_liu2000.rds"))
#   ex_diaphorina <- fit_models(temp = d.citri_liu2000$temperature,
#                             dev_rate = d.citri_liu2000$rate_development,
#                             models = "all")
#   plot_devmodel(temp = d.citri_liu2000$temperature,
#                 dev_rate = d.citri_liu2000$rate_development,
#                 param_tbl = ex_diaphorina)
#
#   ##### d) Trioza erytreae ---------------------------------------------------------
#   ## much different thermal traits than the previous species, more difficult to converge.
#
#   t.erytreae_aidoo2022 <- readRDS(file = here::here("data/t.erytreae_aidoo2022.rds"))
#   ex_trioza <- fit_models(temp = t.erytreae_aidoo2022$temperature,
#                             dev_rate = t.erytreae_aidoo2022$rate_development,
#                             models = "all")
#   plot_devmodel(temp = t.erytreae_aidoo2022$temperature,
#                 dev_rate = t.erytreae_aidoo2022$rate_development,
#                 param_tbl = ex_trioza)
