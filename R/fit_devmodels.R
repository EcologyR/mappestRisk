#' Fit nonlinear regression models to development rate data
#'
#' @param temp a vector containing temperature treatments (predictor variable), must have at least three different temperature treatments
#' @param dev_rate a vector containing development rate estimates (1/days of development); must be of same length than temp
#' @param model_name "all" or alternatively one or several of the models listed in list_available_models
#'
#' @return this function returns a tibble with estimate and standard error for each parameter of the selected models and the AIC
#' @export
#'
#' @examples  a.citricidus_tsai1999 <- readRDS("data/a.citricidus_tsai1999.rds")
#' source("R/dario_model_fitting.R")
#'
#' aphis_citricida_fitted <- fit_models(temp = a.citricidus_tsai1999$temperature,
#'                                      dev_rate = a.citricidus_tsai1999$rate_development,
#'                                      models = c("all")) #might be a bit slow
#'
#'
#'
#'

fit_devmodels <- function(temp, dev_rate, model_name){
  source("data-raw/auxiliary_functions_modelfitting.R")
  data("list_available_models")
  `%!in%` <- Negate(`%in%`)
  if(any(model_name %!in% list_available_models) &&
     model_name != "all") {
    stop("model name not available. For available model names, run 'data(list_available_models); list_available_models'.")
  } else if(model_name[1] == "all") { # it will be probably the most commonly used option for user's experience
    model_names <- c("briere1", "briere2", "mod_gaussian", "janisch",
                "lactin1", "lactin2", "linear_campbell", "wang",
                "ratkowsky", "rezende", "ssi", "mod_weibull", "mod_polynomial")
  } else {
      model_names <- model_name
  }
  list_param <- tibble(param_name = NULL,
                       start_vals = NULL,
                       param_est = NULL,
                       param_se = NULL,
                       model_name = NULL,
                       model_AIC = NULL)
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
    fit_gnls <- suppressWarnings(gnls(model = reformulate(response = "dev_rate",
                                                          termlabels = unique(start_formula_tbl$formula)),
                                      data = devdata,
                                      start = replace_na(start_vals, 0), #to avoid error if start values compute a NA, probably not converging
                                      na.action = na.exclude, #to avoid problems in the model
                                      weights = varExp(form = ~temp), #usually development at higher temperatures has higher variability due to higher mortality
                                      control = gnlsControl(maxIter = 100,
                                      nlsTol = 1e-07,
                                      returnObject = TRUE)
                                      )
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
if(!is.null(fit_gnls) && nrow(list_param) == 0){
  stop(paste("Model(s)", model_name, "did not converged. Please try other models listed in `list_available_models`"))
} else {
    return(list_param)
  }
}

