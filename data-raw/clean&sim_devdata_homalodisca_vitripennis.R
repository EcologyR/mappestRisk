# make example data: simulate individual data from pooled development days data in
# Liu, S. S., Chen, F. Z. & Zalucki, M. P., 2002. Development and survival of the
# diamondback moth (Lepidoptera: Plutellidae) at constant and alternating temperatures.
# Environmental Entomology, 31, 2: 221-241. https://doi.org/10.1603/0046-225X-31.2.221
#


h.vitripennis_pilkington2014 <- readxl::read_xlsx("data-raw/homalodisca_vitripennis_pilkington2014.xlsx") |>
  mutate(sd_devdays = devdays_se*sqrt(sample_size),
         rate_development = 1/devdays) |>
  filter(life_stage == "total") |>
  mutate(reference = "Pilkington2014")

usethis::use_data(h.vitripennis_pilkington2014,
                  overwrite = TRUE
                  )

