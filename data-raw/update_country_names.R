if (packageVersion("geodata") > "0.5.3") {
  wrld <- geodata::world(path = tempdir())
  country_names <- sort(unique(wrld$NAME_0))
  Encoding(country_names) <- "UTF-8"
  usethis::use_data(country_names, overwrite = TRUE)
}
Encoding(country_names) <- "UTF-8"
usethis::use_data(country_names, overwrite = TRUE)
