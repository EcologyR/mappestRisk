# make example data: simulate individual data from pooled development days data in
# Liu & Tsai, 2000

t.erytreae_aidoo2022_raw <- read_csv(here::here("data/review_citrus_pests.csv")) |>
  filter(reference == "aidoo2022" & stage == "nymph V") |>
  mutate(se = if_else(is.na(se) == TRUE,
                      sd_devdays/sqrt(sample_size),
                      se),
         sd_devdays = if_else(is.na(sd_devdays) == TRUE,
                              se*sqrt(sample_size),
                              sd_devdays))

t.erytreae_aidoo2022_rep <- tibble(reference = rep(t.erytreae_aidoo2022_raw$reference, t.erytreae_aidoo2022_raw$sample_size),
                                    devdays = rep(t.erytreae_aidoo2022_raw$devdays, t.erytreae_aidoo2022_raw$sample_size),
                                    sd_devdays = rep(t.erytreae_aidoo2022_raw$sd_devdays, t.erytreae_aidoo2022_raw$sample_size),
                                    se = rep(t.erytreae_aidoo2022_raw$se, t.erytreae_aidoo2022_raw$sample_size),
                                    temperature = rep(t.erytreae_aidoo2022_raw$temperature, t.erytreae_aidoo2022_raw$sample_size),
                                    species = rep(t.erytreae_aidoo2022_raw$species, t.erytreae_aidoo2022_raw$sample_size),
                                    sample_size = rep(t.erytreae_aidoo2022_raw$sample_size, t.erytreae_aidoo2022_raw$sample_size))
set.seed(2023)
t.erytreae_aidoo2022_rep$devdays_sim = rnorm(n = nrow(t.erytreae_aidoo2022_rep),
                                              0,
                                              1)
t.erytreae_aidoo2022_rep$devdays_sim = rnorm(n = nrow(t.erytreae_aidoo2022_rep),
                                              mean = mean(t.erytreae_aidoo2022_rep$devdays, na.rm = TRUE),
                                              sd = mean(t.erytreae_aidoo2022_rep$sd_devdays, na.rm = TRUE))
sum_t.erytreae_aidoo2022 <- t.erytreae_aidoo2022_rep |>
  group_by(temperature, reference) |>
  summarise(devdays_sum = mean(devdays_sim),
            sd_sum = sd(devdays_sim))
merging_t.erytreae_aidoo2022 <- inner_join(t.erytreae_aidoo2022_rep, sum_t.erytreae_aidoo2022) |>
  mutate(devdays_est = devdays + (devdays_sim - devdays_sum)*(sd_devdays/sd_sum)) |>
  select(reference, species, temperature, devdays_est, se) |>
  as_tibble()
se_study <- merging_t.erytreae_aidoo2022 |>
  group_by(reference) |>
  summarise(vi = mean(se, na.rm = TRUE))
t.erytreae_aidoo2022 <- merging_t.erytreae_aidoo2022  |>
  full_join(se_study)  |>
  select(-se, -vi) |>
  mutate(rate_development = 1/devdays_est) |>
  select(-devdays_est)
saveRDS(t.erytreae_aidoo2022,
        file = here::here("data/t.erytreae_aidoo2022.rds"))
