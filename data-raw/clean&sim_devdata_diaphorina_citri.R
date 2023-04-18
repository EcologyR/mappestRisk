# make example data: simulate individual data from pooled development days data in
# Liu & Tsai, 2000

d.citri_liu2000_raw <- read_csv(here::here("data/review_citrus_pests.csv")) |>
  filter(reference == "liu2000" & stage == "nymphs_all") |>
  mutate(se = if_else(is.na(se) == TRUE,
                      sd_devdays/sqrt(sample_size),
                      se),
         sd_devdays = if_else(is.na(sd_devdays) == TRUE,
                              se*sqrt(sample_size),
                              sd_devdays))

d.citri_liu2000_rep <- tibble(reference = rep(d.citri_liu2000_raw$reference, d.citri_liu2000_raw$sample_size),
                                    devdays = rep(d.citri_liu2000_raw$devdays, d.citri_liu2000_raw$sample_size),
                                    sd_devdays = rep(d.citri_liu2000_raw$sd_devdays, d.citri_liu2000_raw$sample_size),
                                    se = rep(d.citri_liu2000_raw$se, d.citri_liu2000_raw$sample_size),
                                    temperature = rep(d.citri_liu2000_raw$temperature, d.citri_liu2000_raw$sample_size),
                                    species = rep(d.citri_liu2000_raw$species, d.citri_liu2000_raw$sample_size),
                                    sample_size = rep(d.citri_liu2000_raw$sample_size, d.citri_liu2000_raw$sample_size))
set.seed(2023)
d.citri_liu2000_rep$devdays_sim = rnorm(n = nrow(d.citri_liu2000_rep),
                                              0,
                                              1)
d.citri_liu2000_rep$devdays_sim = rnorm(n = nrow(d.citri_liu2000_rep),
                                              mean = mean(d.citri_liu2000_rep$devdays, na.rm = TRUE),
                                              sd = mean(d.citri_liu2000_rep$sd_devdays, na.rm = TRUE))
sum_d.citri_liu2000 <- d.citri_liu2000_rep |>
  group_by(temperature, reference) |>
  summarise(devdays_sum = mean(devdays_sim),
            sd_sum = sd(devdays_sim))
merging_d.citri_liu2000 <- inner_join(d.citri_liu2000_rep, sum_d.citri_liu2000) |>
  mutate(devdays_est = devdays + (devdays_sim - devdays_sum)*(sd_devdays/sd_sum)) |>
  select(reference, species, temperature, devdays_est, se) |>
  as_tibble()
se_study <- merging_d.citri_liu2000 |>
  group_by(reference) |>
  summarise(vi = mean(se, na.rm = TRUE))
d.citri_liu2000 <- merging_d.citri_liu2000  |>
  full_join(se_study)  |>
  select(-se, -vi) |>
  mutate(rate_development = 1/devdays_est) |>
  select(-devdays_est)
saveRDS(d.citri_liu2000,
        file = here::here("data/d.citri_liu2000.rds"))
