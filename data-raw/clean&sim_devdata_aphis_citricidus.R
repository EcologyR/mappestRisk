# make example data: simulate individual data from pooled development days data in
# Liu & Tsai, 2000

a.citricidus_tsai1999_raw <- read_csv(here::here("data/review_citrus_pests.csv")) |>
  filter(reference == "tsai1999" & stage == "nymph") |>
  mutate(se = if_else(is.na(se) == TRUE,
                      sd_devdays/sqrt(sample_size),
                      se),
         sd_devdays = if_else(is.na(sd_devdays) == TRUE,
                              se*sqrt(sample_size),
                              sd_devdays))

a.citricidus_tsai1999_rep <- tibble(reference = rep(a.citricidus_tsai1999_raw$reference, a.citricidus_tsai1999_raw$sample_size),
                                    devdays = rep(a.citricidus_tsai1999_raw$devdays, a.citricidus_tsai1999_raw$sample_size),
                                    sd_devdays = rep(a.citricidus_tsai1999_raw$sd_devdays, a.citricidus_tsai1999_raw$sample_size),
                                    se = rep(a.citricidus_tsai1999_raw$se, a.citricidus_tsai1999_raw$sample_size),
                                    temperature = rep(a.citricidus_tsai1999_raw$temperature, a.citricidus_tsai1999_raw$sample_size),
                                    species = rep(a.citricidus_tsai1999_raw$species, a.citricidus_tsai1999_raw$sample_size),
                                    sample_size = rep(a.citricidus_tsai1999_raw$sample_size, a.citricidus_tsai1999_raw$sample_size))
set.seed(2023)
a.citricidus_tsai1999_rep$devdays_sim = rnorm(n = nrow(a.citricidus_tsai1999_rep),
                                              0,
                                              1)
a.citricidus_tsai1999_rep$devdays_sim = rnorm(n = nrow(a.citricidus_tsai1999_rep),
                                              mean = mean(a.citricidus_tsai1999_rep$devdays, na.rm = TRUE),
                                              sd = mean(a.citricidus_tsai1999_rep$sd_devdays, na.rm = TRUE))
sum_a.citricidus_tsai1999 <- a.citricidus_tsai1999_rep |>
  group_by(temperature, reference) |>
  summarise(devdays_sum = mean(devdays_sim),
            sd_sum = sd(devdays_sim))
merging_a.citricidus_tsai1999 <- inner_join(a.citricidus_tsai1999_rep, sum_a.citricidus_tsai1999) |>
  mutate(devdays_est = devdays + (devdays_sim - devdays_sum)*(sd_devdays/sd_sum)) |>
  select(reference, species, temperature, devdays_est, se) |>
  as_tibble()
se_study <- merging_a.citricidus_tsai1999 |>
  group_by(reference) |>
  summarise(vi = mean(se, na.rm = TRUE))
a.citricidus_tsai1999 <- merging_a.citricidus_tsai1999  |>
  full_join(se_study)  |>
  select(-se, -vi) |>
  mutate(rate_development = 1/devdays_est) |>
  select(-devdays_est)
saveRDS(a.citricidus_tsai1999,
        file = here::here("data/a.citricidus_tsai1999.rds"))
