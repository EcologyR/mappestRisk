model_names <- c("beta", "boatman","briere1", "briere2",
                 "joehnk", "kamykowski", "lactin1", "lactin2",
                 "mod_weibull", "mod_polynomial", "oneill", "pawar",
                 "ratkowsky", "schoolfield", "regniere",  "thomas", "wang")

pkg_model_names <- c("beta_2012","boatman_2017", "briere1_99", "briere2_1999",
                     "joehnk_2008", "kamykowski_1985", "lactin1_95", "lactin2_1995",
                     "weibull_1995", "poly4", "oneill_1972", "pawar_2018",
                     "ratkowsky_1983", "sharpeschoolfull_1981", "regniere_12","thomas_2012", "wang_82")

dev_model_table <- tibble(model_name = model_names) |>
  mutate(package = if_else(model_name %in% c("briere1",  "lactin1",  # <- lactin 2 has convergence problems with rTPC
                                             "wang", "regniere",
                                             "mod_polynomial"),
                           "devRate",
                           "rTPC"),
         source_model_name = pkg_model_names) |>
  mutate(formula = case_when(model_name == "beta" ~ "rTPC::beta_2012(temp, a, b, c, d, e)",
                             model_name == "boatman" ~"rTPC::boatman_2017(temp, rmax, tmin, tmax, a, b)",
                             model_name == "briere1" ~  "briere1(temp, tmin, tmax, a)",
                             model_name == "briere2" ~ "rTPC::briere2_1999(temp, tmin, tmax, a, b)",
                             model_name == "joehnk" ~ "rTPC::joehnk_2008(temp, rmax, topt, a, b, c)",
                             model_name == "kamykowski" ~ "rTPC::kamykowski_1985(temp, tmin, tmax, a, b, c)",
                             model_name == "lactin1" ~ "lactin1(temp, a, tmax, delta_t)",
                             model_name == "lactin2" ~ "rTPC::lactin2_1995(temp, a, b, tmax, delta_t)",
                             model_name == "mod_weibull" ~ "rTPC::weibull_1995(temp, a, topt, b, c)",
                             model_name == "mod_polynomial" ~ "mod_polynomial(temp, a_0, a_1, a_2, a_3, a_4)",
                             model_name == "oneill" ~ "rTPC::oneill_1972(temp, rmax, ctmax, topt, q10)",
                             model_name == "pawar" ~ "rTPC::pawar_2018(temp, r_tref, e, eh, topt, tref = 25)",
                             model_name == "ratkowsky" ~ "rTPC::ratkowsky_1983(temp, tmin, tmax, a, b)",
                             model_name == "schoolfield" ~ "rTPC::sharpeschoolfull_1981(temp, rtref, e, el, tl, eh, th, tref = 25)",
                             model_name == "regniere" ~ "regniere(temp, tmin, tmax, phi, delta_b, delta_m, b)",
                             model_name == "thomas" ~ "rTPC::thomas_2012(temp, a, b, c, topt)",
                             model_name == "wang" ~ "wang(temp, k, r, topt, tmin, tmax, a)",
  )) |>
  mutate(working_formula = case_when(model_name == "beta" ~  "beta_2012(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4], start_vals[5])",
                                     model_name == "boatman" ~ "rTPC::boatman_2017(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4], start_vals[5])",
                                     model_name == "briere1" ~  "briere1(.x, start_vals[1], start_vals[2], start_vals[3])",
                                     model_name == "briere2" ~ "rTPC::briere2_1999(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                                     model_name == "joehnk" ~ "rTPC::joehnk_2008(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4], start_vals[5])",
                                     model_name == "kamykowski" ~ "rTPC::kamykowski_1985(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4], start_vals[5])",
                                     model_name == "lactin1" ~ "lactin1(.x, start_vals[1], start_vals[2], start_vals[3])",
                                     model_name == "lactin2" ~ "rTPC::lactin2_1995(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                                     model_name == "mod_weibull" ~ "rTPC::weibull_1995(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                                     model_name == "mod_polynomial" ~ "mod_polynomial(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4], start_vals[5])",
                                     model_name == "oneill" ~ "rTPC::oneill_1972(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                                     model_name == "pawar" ~ "rTPC::pawar_2018(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4], tref = 25)",
                                     model_name == "ratkowsky" ~ "rTPC::ratkowsky_1983(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                                     model_name == "schoolfield" ~ "rTPC::sharpeschoolfull_1981(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4], start_vals[5], start_vals[6], tref = 25)",
                                     model_name == "regniere" ~ "regniere(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4], start_vals[5], start_vals[6])",
                                     model_name == "thomas" ~ "rTPC::thomas_2012(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4])",
                                     model_name == "wang" ~ "wang(.x, start_vals[1], start_vals[2], start_vals[3], start_vals[4], start_vals[5], start_vals[6])"
  )) |>
  mutate(working_formula = case_when(model_name == "beta" ~  "beta_2012(.x, params_i[1], params_i[2], params_i[3], params_i[4], params_i[5])",
                                     model_name == "boatman" ~ "rTPC::boatman_2017(.x, params_i[1], params_i[2], params_i[3], params_i[4], params_i[5])",
                                     model_name == "briere1" ~  "briere1(.x, params_i[1], params_i[2], params_i[3])",
                                     model_name == "briere2" ~ "rTPC::briere2_1999(.x, params_i[1], params_i[2], params_i[3], params_i[4])",
                                     model_name == "joehnk" ~ "rTPC::joehnk_2008(.x, params_i[1], params_i[2], params_i[3], params_i[4], params_i[5])",
                                     model_name == "kamykowski" ~ "rTPC::kamykowski_1985(.x, params_i[1], params_i[2], params_i[3], params_i[4], params_i[5])",
                                     model_name == "lactin1" ~ "lactin1(.x, params_i[1], params_i[2], params_i[3])",
                                     model_name == "lactin2" ~ "rTPC::lactin2_1995(.x, params_i[1], params_i[2], params_i[3], params_i[4])",
                                     model_name == "mod_weibull" ~ "rTPC::weibull_1995(.x, params_i[1], params_i[2], params_i[3], params_i[4])",
                                     model_name == "mod_polynomial" ~ "mod_polynomial(.x, params_i[1], params_i[2], params_i[3], params_i[4], params_i[5])",
                                     model_name == "oneill" ~ "rTPC::oneill_1972(.x, params_i[1], params_i[2], params_i[3], params_i[4])",
                                     model_name == "pawar" ~ "rTPC::pawar_2018(.x, params_i[1], params_i[2], params_i[3], params_i[4], tref = 25)",
                                     model_name == "ratkowsky" ~ "rTPC::ratkowsky_1983(.x, params_i[1], params_i[2], params_i[3], params_i[4])",
                                     model_name == "schoolfield" ~ "rTPC::sharpeschoolfull_1981(.x, params_i[1], params_i[2], params_i[3], params_i[4], params_i[5], params_i[6], tref = 25)",
                                     model_name == "regniere" ~ "regniere(.x, params_i[1], params_i[2], params_i[3], params_i[4], params_i[5], params_i[6])",
                                     model_name == "thomas" ~ "rTPC::thomas_2012(.x, params_i[1], params_i[2], params_i[3], params_i[4])",
                                     model_name == "wang" ~ "wang(.x, params_i[1], params_i[2], params_i[3], params_i[4], params_i[5], params_i[6])"
  ))


save(dev_model_table, file = here::here("data/available_models.rda"))
