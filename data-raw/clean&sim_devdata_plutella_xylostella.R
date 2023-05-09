# make example data: simulate individual data from pooled development days data in
# Liu, S. S., Chen, F. Z. & Zalucki, M. P., 2002. Development and survival of the
# diamondback moth (Lepidoptera: Plutellidae) at constant and alternating temperatures.
# Environmental Entomology, 31, 2: 221-241. https://doi.org/10.1603/0046-225X-31.2.221
#


p.xylostella_liu2002_raw <- read_delim(here::here("data-raw/p.xylostella_liu2002.csv")) |>
  filter(stage == "larva IV") |> # the most damaging stage
  mutate(sd_devdays = se_devdays*sqrt(sample_size))

p.xylostella_liu2002_rep <- tibble(reference = rep(p.xylostella_liu2002_raw$reference, p.xylostella_liu2002_raw$sample_size),
                                   devdays = rep(p.xylostella_liu2002_raw$devdays, p.xylostella_liu2002_raw$sample_size),
                                   sd_devdays = rep(p.xylostella_liu2002_raw$sd_devdays, p.xylostella_liu2002_raw$sample_size),
                                   se = rep(p.xylostella_liu2002_raw$se_devdays, p.xylostella_liu2002_raw$sample_size),
                                   temperature = rep(p.xylostella_liu2002_raw$temperature, p.xylostella_liu2002_raw$sample_size),
                                   sample_size = rep(p.xylostella_liu2002_raw$sample_size, p.xylostella_liu2002_raw$sample_size))
set.seed(2023)
p.xylostella_liu2002_rep$devdays_sim = rnorm(n = nrow(p.xylostella_liu2002_rep),
                                             0,
                                             1)
p.xylostella_liu2002_rep$devdays_sim = rnorm(n = nrow(p.xylostella_liu2002_rep),
                                             mean = mean(p.xylostella_liu2002_rep$devdays, na.rm = TRUE),
                                             sd = mean(p.xylostella_liu2002_rep$sd_devdays, na.rm = TRUE))
sum_p.xylostella_liu2002 <- p.xylostella_liu2002_rep |>
  group_by(temperature, reference) |>
  summarise(devdays_sum = mean(devdays_sim),
            sd_sum = sd(devdays_sim))
merging_p.xylostella_liu2002 <- inner_join(p.xylostella_liu2002_rep, sum_p.xylostella_liu2002) |>
  mutate(devdays_est = devdays + (devdays_sim - devdays_sum)*(sd_devdays/sd_sum)) |>
  select(reference, temperature, devdays_est, se) |>
  as_tibble()
se_study <- merging_p.xylostella_liu2002 |>
  group_by(reference) |>
  summarise(vi = mean(se, na.rm = TRUE))
p.xylostella_liu2002 <- merging_p.xylostella_liu2002  |>
  full_join(se_study)  |>
  select(-se, -vi) |>
  mutate(rate_development = 1/devdays_est,
         stage = "larva IV") |>
  select(-devdays_est) |>
  filter(rate_development >= 0 &
           rate_development < 1)
save(p.xylostella_liu2002,
     file = here::here("data/p.xylostella_liu2002.rda"))
