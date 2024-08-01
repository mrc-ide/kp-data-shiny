library(googlesheets4)
library(tidyverse)

ethic <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1murnx2dH0W_94gFFuJ-oSW0ugdmv8WPinmog0_V-0FI/edit#gid=1735078680", sheet = "Study long format")

data <- readxl::read_excel("data/2024-07-31_key-population-collated-data.xlsx", sheet = "Data")

ethic %>%
  filter(study_idx %in% c(unique(data$study_idx), 373)) %>%
  mutate(country = countrycode::countrycode(iso3, "iso3c", "country.name")) %>%
  distinct(study_idx, iso3, country, kp, year, study, author, link, public_report) %>%
  mutate(study = str_replace_all(study, "\\\t", " ")) %>%
  write_csv("data/sources.csv", na="")
