
# a.citricidus_tsai1999 <- readRDS(file = here::here("data/a.citricidus_tsai1999.rds"))
#
# ## fit models to development data and return a tibble to select parameters from the chosen model(s)
# example_fit <- fit_models(temp = a.citricidus_tsai1999$temperature,
#                           dev_rate = a.citricidus_tsai1999$rate_development,
#                           models = c("mod_gaussian", "briere2", "mod_weibull", "wang"))
#

# plot_devmodel(temp = a.citricidus_tsai1999$temperature,
#               dev_rate = a.citricidus_tsai1999$rate_development,
#               param_tbl = example_fit)
#

thermal_suitability_bounds <- function(fitted_parameters,
                                       model_name = NULL,
                                       suitability_threshold = NULL) {
  `%!in%` <- Negate(`%in%`)
  data("list_available_models")
  fitted_parameters_nonas <- fitted_parameters |> drop_na()
  fitted_param_names <- fitted_parameters_nonas |> distinct(model_name) |> pull(model_name)
  if(is.null(suitability_threshold)){
    suitability_threshold <- 50
  }
  if(!is.null(model_name) && model_name %!in% list_available_models) {
    stop("model name not available. For available model names, run 'data(list_available_models); list_available_models'.")
  } else if(!is.null(model_name) && model_name %!in% fitted_param_names) {
    stop(paste("Model", model_name, "did not converge. Please use another one  in your fitted data frame instead")
  }
  else if(is.null(model_name)){
    fitted_parameters_nonas <- example_fit
    model_name <- fitted_parameters_nonas |> drop_na() |> distinct(model_name) |> pull()
    predict2fill <- tibble(temp = NULL,
                           dev_rate = NULL,
                           model_name = NULL)
    tvals <- tibble(model_name = NULL,
                    tval_left = NULL,
                    tval_right = NULL)
    for(i in model_name){
      params_i <- fitted_parameters_nonas |>
        filter(model_name == i)  |>
        pull(param_est)

      ##predict based on parameters
      explore_preds <- tibble(temp = seq(0,50, 0.001),
                              model_name = i,
                              preds = NULL,
      )
      fit_vals_tbl <- explore_preds |>
        select(temp, model_name) |>
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
      topt_pred <- predict2fill |> slice_max(preds) |> pull(temp)
      devrate_max <- predict2fill |> slice_max(preds) |> pull(preds)
      half_left <- predict2fill |> filter(temp < topt_pred)
      half_right <- predict2fill |> filter(temp >= topt_pred)
      therm_suit_left <- half_left |>
        slice(max(which(half_left$preds <= devrate_max*0.01*suitability_threshold), na.rm = TRUE)) |>
        pull(temp)
      therm_suit_right <- half_right |>
        slice(min(which(half_right$preds <= devrate_max*0.01*suitability_threshold), na.rm = TRUE)) |>
        pull(temp)
      tvals_i <- tibble(model_name = i,
                        tval_left = therm_suit_left,
                        tval_right = therm_suit_right)
      tvals <- bind_rows(tvals, tvals_i)
    } #loop ends
    return(tvals)
  } else {
    model2predict <- model_name
    predict2fill <- tibble(temp = NULL,
                           dev_rate = NULL,
                           model_name = NULL)
    params_i <- fitted_parameters_nonas |>
      filter(model_name == model2predict)  |>
      pull(param_est)

    ##predict based on parameters
    explore_preds <- tibble(temp = seq(0,50, 0.001),
                            model_name = model2predict,
                            preds = NULL,
    )
    fit_vals_tbl <- explore_preds |>
      select(temp, model_name) |>
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
    topt_pred <- predict2fill |> slice_max(preds) |> pull(temp)
    devrate_max <- predict2fill |> slice_max(preds) |> pull(preds)
    half_left <- predict2fill |> filter(temp < topt_pred)
    half_right <- predict2fill |> filter(temp >= topt_pred)

    therm_suit_left <- half_left |>
      slice(max(which(half_left$preds <= devrate_max*0.01*suitability_threshold), na.rm = TRUE)) |>
      pull(temp)
    therm_suit_right <- half_right |>
      slice(min(which(half_right$preds <= devrate_max*0.01*suitability_threshold), na.rm = TRUE)) |>
      pull(temp)
    tvals <- tibble(model_name = model2predict,
                    tval_left = therm_suit_left,
                    tval_right = therm_suit_right)
    return(tvals)
  }
}



thermal_suitability_bounds(fitted_parameters = example_fit,
                           model_name = "lactin2",
                           suitability_threshold = 65)

