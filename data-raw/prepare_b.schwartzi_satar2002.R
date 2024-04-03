# make example data: simulate individual data from pooled development days data in
# Satar, S., and Yokomi, R., 2002.Effect of Temperature and Host on Development of Brachycaudus schwartzi (Homoptera: Aphididae).
# Ann. Entomol. Soc. Am. 95(5): 597-602
#


b.schwartzi_satar2002 <- readxl::read_xlsx("data-raw/satar_data.xlsx") |>
  mutate(rate_value =  1/dev_days)

usethis::use_data(b.schwartzi_satar2002,
                  overwrite = TRUE
                  )

