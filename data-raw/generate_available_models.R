model_names <- c("briere1", "briere2", "mod_gaussian", "regniere",
                 "lactin1", "lactin2", "linear_campbell", "wang",
                 "mod_polynomial", "ratkowsky", "rezende", "ssi", "mod_weibull")

pkg_model_names <- c("briere1_99", "briere2_1999", "gaussian_1987", "regniere_12",
                     "lactin1_95", "lactin2_1995", "campbell_74", "wang_82",
                     "poly4", "ratkowsky_1983", "rezende_2019",
                     "schoolfield_81", "weibull_1995")


dev_model_table <- tibble(model_name = model_names) |>
  mutate(package = if_else(model_name %in% c("briere1",  "lactin1",  # <- lactin 2 has convergence problems with rTPC
                                             "wang", "regniere", "ssi",
                                             "mod_polynomial", "linear_campbell"),
                           "devRate",
                           "rTPC"),
         source_model_name = pkg_model_names) |>
  mutate(formula = case_when(model_name == "briere1" ~  "briere1(temp, tmin, tmax, a)",
                             model_name == "lactin1" ~ "lactin1(temp, a, tmax, delta_t)",
                             model_name == "wang" ~ "wang(temp, k, r, topt, tmin, tmax, a)",
                             model_name == "linear_campbell" ~ "linear_campbell(temp,intercept, slope)",
                             model_name == "mod_polynomial" ~ "mod_polynomial(temp, a_0, a_1, a_2, a_3, a_4)",
                             model_name == "briere2" ~ "rTPC::briere2_1999(temp, tmin, tmax, a, b)",
                             model_name == "mod_gaussian" ~ "rTPC::gaussian_1987(temp, rmax, topt, a)",
                             model_name == "lactin2" ~ "rTPC::lactin2_1995(temp, a, b, tmax, delta_t)",
                             model_name == "ratkowsky" ~ "rTPC::ratkowsky_1983(temp, tmin, tmax, a, b)",
                             model_name == "rezende" ~ "rTPC::rezende_2019(temp, q10, a, b, c)",
                             model_name == "ssi" ~ "ssi(temp, p25, a, b, c, d, e)",
                             model_name == "mod_weibull" ~ "rTPC::weibull_1995(temp, a, topt, b, c)",
                             model_name == "regniere" ~ "regniere(temp, tmin, tmax, phi, delta_b, delta_m, b)"
  )) |>
    mutate(working_formula = case_when(model_name == "briere1" ~  "briere1(.x, start_vals[1], start_vals[2], start_vals[3])",
                               model_name == "lactin1" ~ "lactin1(.x, start_vals[1], start_vals[2], start_vals[3])",
                               model_name == "linear_campbell" ~ "linear_campbell(.x, start_vals[1], start_vals[2])",
                               model_name == "mod_polynomial" ~ "mod_polynomial(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4], start_vals[5])",
                               model_name == "briere2" ~ "rTPC::briere2_1999(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                               model_name == "mod_gaussian" ~ "rTPC::gaussian_1987(.x, start_vals[1], start_vals[2], start_vals[3])",
                               model_name == "lactin2" ~ "rTPC::lactin2_1995(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                               model_name == "ratkowsky" ~ "rTPC::ratkowsky_1983(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                               model_name == "rezende" ~ "rTPC::rezende_2019(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                               model_name == "ssi" ~ "ssi(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4], start_vals[5], start_vals[6])",
                               model_name == "mod_weibull" ~ "rTPC::weibull_1995(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                               model_name == "wang" ~ "wang(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4], start_vals[5], start_vals[6])",
                               model_name == "regniere" ~ "regniere(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4], start_vals[5], start_vals[6])"
    )) |>
  mutate(params_formula =  case_when(model_name == "briere1" ~  "briere1(.x, params_i[1], params_i[2], params_i[3])",
                                     model_name == "lactin1" ~ "lactin1(.x, params_i[1], params_i[2], params_i[3])",
                                     model_name == "linear_campbell" ~ "linear_campbell(.x, params_i[1], params_i[2])",
                                     model_name == "mod_polynomial" ~ "mod_polynomial(.x, params_i[1], params_i[2], params_i[3], params_i[4], params_i[5])",
                                     model_name == "briere2" ~ "rTPC::briere2_1999(.x, params_i[1], params_i[2], params_i[3], params_i[4])",
                                     model_name == "mod_gaussian" ~ "rTPC::gaussian_1987(.x, params_i[1], params_i[2], params_i[3])",
                                     model_name == "lactin2" ~ "rTPC::lactin2_1995(.x, params_i[1], params_i[2], params_i[3], params_i[4])",
                                     model_name == "ratkowsky" ~ "rTPC::ratkowsky_1983(.x, params_i[1], params_i[2], params_i[3], params_i[4])",
                                     model_name == "rezende" ~ "rTPC::rezende_2019(.x, params_i[1], params_i[2], params_i[3], params_i[4])",
                                     model_name == "ssi" ~ "ssi(.x, params_i[1], params_i[2], params_i[3], params_i[4], params_i[5], params_i[6])",
                                     model_name == "mod_weibull" ~ "rTPC::weibull_1995(.x, params_i[1], params_i[2], params_i[3], params_i[4])",
                                     model_name == "wang" ~ "wang(.x, params_i[1], params_i[2], params_i[3], params_i[4], params_i[5], params_i[6])",
                                     model_name == "regniere" ~ "regniere(.x, params_i[1], params_i[2], params_i[3], params_i[4], params_i[5], params_i[6])"
  ))


save(dev_model_table, file = here::here("data/available_models.rda"))











