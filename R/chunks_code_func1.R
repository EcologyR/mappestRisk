#weibull
start_vals_weibull <- rTPC::get_start_vals(x = pieris_data$temperature,
                                           y = pieris_data$devrate,
                                           model_name = "weibull_1995")
weibull_pieris <- gnls(devrate ~ weibull_1995(temp = temperature,
                                              a, topt, b, c),
                       data = pieris_data,
                       start = start_vals_weibull,
                       na.action = na.exclude,
                       weights = varExp(form = ~temperature),
                       control = gnlsControl(nlsTol = 1e-02)
)
weibull_pieris

weibull_fit <- tibble(model_name = "weibull",
                      nls_model = list(weibull_pieris))

model_outputs <- bind_rows(ssi_fit, br1_fit, br2_fit, lactin2_fit, lactin1_fit,
                           ratkowsky_fit, rezende_fit, oneill_fit,
                           gaussian_fit, beta_fit, weibull_fit, ssilow_fit,
                           linear_fit) %>% 
  mutate(est = map(.x = nls_model, .f = coef),
         param = map(.x = nls_model, .f = param_names_extractor)) %>%
  select(-nls_model) %>%
  unnest(c(est, param))

seq_temps <- seq(3,45, by = 0.001)
model_preds <- bind_rows(ssi_fit, br1_fit, br2_fit, lactin2_fit, lactin1_fit,
                         ratkowsky_fit, rezende_fit, oneill_fit, gaussian_fit,
                         beta_fit, weibull_fit, ssilow_fit, linear_fit) %>% 
 mutate(case_when(model_name == "weibull" ~ list(weibull_1995(seq_temps,
                                              a = weibull_pieris$coefficients[1],
                                              topt = weibull_pieris$coefficients[2],
                                              b = weibull_pieris$coefficients[3],
                                              c = weibull_pieris$coefficients[4])))) %>% 
  ) %>% 
  unnest(preds) %>%
  group_by(model_name) %>% 
  mutate(temperature = seq_temps) %>%
  filter(preds >= 0 & preds <1)
        
