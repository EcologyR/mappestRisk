##### a) climate data -----------------------------------------------
present_tmin_climate_wc <-  geodata::worldclim_global(var = "tmin",
                                                      res = 2.5,
                                                      path = "~/Dario Investigacion")
crop_tmin_present <- crop(present_tmin_climate_wc,
                          extent(-10.694771, 36.201587,28.832644, 47.210240))
europe_citrus_tmin_present <- mask(crop_tmin_present, citrus_europe)
europe_citrus_tmin_present <- brick(europe_citrus_tmin_present) #terra::rast()
present_tmin_tbl <- as(europe_citrus_tmin_present, "SpatialPixelsDataFrame") %>% #terra::as.data.frame()
  as_tibble() %>%
  pivot_longer(cols = 1:12, names_to = "month") %>%
  mutate(month = paste0("month", str_sub(month,-2)))

## tmax
present_tmax_climate_wc <-  geodata::worldclim_global(var = "tmax",
                                                      res = 2.5,
                                                      path = "~/Dario Investigacion")
crop_tmax_present <- crop(present_tmax_climate_wc,
                          extent(-10.694771, 36.201587,28.832644, 47.210240))
europe_citrus_tmax_present <- mask(crop_tmax_present, citrus_europe)
europe_citrus_tmax_present <- brick(europe_citrus_tmax_present)
present_tmax_tbl <- as(europe_citrus_tmax_present, "SpatialPixelsDataFrame") %>%
  as_tibble() %>%
  pivot_longer(cols = 1:12, names_to = "month") %>%
  mutate(month = paste0("month", str_sub(month,-2)))

## join and average
present_tavg_tbl <- full_join(present_tmin_tbl,
                              present_tmax_tbl) %>%
  group_by(x, y, month) %>%
  summarise(present_tavg = mean(value))
