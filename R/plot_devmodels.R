#' Explore shape of fitted thermal performance curves to choose an appropriate model based on both statistics and ecological sense
#'
#' @param temp a vector containing temperature treatments (predictor variable), must have at least three different temperature treatments
#' @param dev_rate a vector containing development rate estimates (1/days of development); must be of same length than temp
#' @param fitted_parameters a tibbl obtained with fit_models() function including parameter names, estimates, se and AICs
#'
#' @return a ggplot with facets printing development rate data provided by the
#' user and the predicted values by the models fitted with fit_devmodels
#' @export
#'
#' @examples   a.citricidus_tsai1999 <- readRDS("data/a.citricidus_tsai1999.rds")
#' source("R/dario_model_fitting.R")
#' plot_devmodels(temp = p.xylostella_liu2002$temperature,
#'                dev_rate = p.xylostella_liu2002$rate_development,
#'                fitted_parameters = cabbage_moth_fitted)
#'



plot_devmodels <- function(temp, dev_rate, fitted_parameters){
  devdata <- tibble (temperature = temp,
                     development_rate = dev_rate)
  fitted_tbl <- fitted_parameters |> tidyr::drop_na()
  predict2fill <- tibble(temp = NULL,
                         dev_rate = NULL,
                         model_name = NULL,
                         model_AIC = NULL)
  model_names2plot <- fitted_tbl |> distinct(model_name) |> pull(model_name)
  for(i in model_names2plot){
    fitted_tbl_i <- fitted_tbl |> filter(model_name == i)
    model_AIC_i <-fitted_tbl_i |> pull(model_AIC)
    params_i <- fitted_tbl_i |> pull(param_est)
    formula_i <- dev_model_table |> filter(model_name == i) |> pull(params_formula)
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
      mutate(formula = formula_i) |>
      mutate(preds = purrr::map_dbl(.x = temp,
                                    .f = reformulate(unique(formula_i)))) |>
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
    pull(model_name)
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
                   y = preds-0.20,
                   label = n_params,
                   fill = model_name),
               color = "white",
               size = 3)
  return(ggplot_models)
}



