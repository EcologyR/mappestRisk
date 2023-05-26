# make example data: simulate individual data from pooled development days data in
# Liu, S. S., Chen, F. Z. & Zalucki, M. P., 2002. Development and survival of the
# diamondback moth (Lepidoptera: Plutellidae) at constant and alternating temperatures.
# Environmental Entomology, 31, 2: 221-241. https://doi.org/10.1603/0046-225X-31.2.221
#


h.vitripennis_pilkington2014_raw <- readxl::read_xlsx("data-raw/homalodisca_vitripennis_pilkington2014.xlsx") |>
  mutate(sd_devdays = devdays_se*sqrt(sample_size),
         rate_development = 1/devdays) |>
  filter(life_stage == "total") |>
  mutate(reference = "Pilkington2014")

h.vitripennis_pilkington2014_rep <- tibble(reference = rep(h.vitripennis_pilkington2014_raw$reference, h.vitripennis_pilkington2014_raw$sample_size),
                                   devdays = rep(h.vitripennis_pilkington2014_raw$devdays, h.vitripennis_pilkington2014_raw$sample_size),
                                   sd_devdays = rep(h.vitripennis_pilkington2014_raw$sd_devdays, h.vitripennis_pilkington2014_raw$sample_size),
                                   se = rep(h.vitripennis_pilkington2014_raw$devdays_se, h.vitripennis_pilkington2014_raw$sample_size),
                                   temperature = rep(h.vitripennis_pilkington2014_raw$temperature, h.vitripennis_pilkington2014_raw$sample_size),
                                   sample_size = rep(h.vitripennis_pilkington2014_raw$sample_size, h.vitripennis_pilkington2014_raw$sample_size))
set.seed(2023)
h.vitripennis_pilkington2014_rep$devdays_sim = rnorm(n = nrow(h.vitripennis_pilkington2014_rep),
                                             0,
                                             1)
h.vitripennis_pilkington2014_rep$devdays_sim = rnorm(n = nrow(h.vitripennis_pilkington2014_rep),
                                             mean = mean(h.vitripennis_pilkington2014_rep$devdays, na.rm = TRUE),
                                             sd = mean(h.vitripennis_pilkington2014_rep$sd_devdays, na.rm = TRUE))
sum_h.vitripennis_pilkington2014 <- h.vitripennis_pilkington2014_rep |>
  group_by(temperature, reference) |>
  summarise(devdays_sum = mean(devdays_sim),
            sd_sum = sd(devdays_sim))
merging_h.vitripennis_pilkington2014 <- inner_join(h.vitripennis_pilkington2014_rep, sum_h.vitripennis_pilkington2014) |>
  mutate(devdays_est = devdays + (devdays_sim - devdays_sum)*(sd_devdays/sd_sum)) |>
  select(reference, temperature, devdays_est, se) |>
  as_tibble()
se_study <- merging_h.vitripennis_pilkington2014 |>
  group_by(reference) |>
  summarise(vi = mean(se, na.rm = TRUE))
h.vitripennis_pilkington2014 <- merging_h.vitripennis_pilkington2014  |>
  full_join(se_study)  |>
  select(-se, -vi) |>
  mutate(rate_development = 1/devdays_est,
         stage = "total") |>
  select(-devdays_est) |>
  filter(rate_development >= 0 &
           rate_development < 1)
usethis::use_data(h.vitripennis_pilkington2014,
                  # overwrite = TRUE
                  )

