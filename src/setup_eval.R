library(dplyr)
library(lubridate)
source(file.path("src", "write_config.R"))

write_config(
  noro_nowcast_dates = c("2023-11-05", "2023-11-12"),
  covid_nowcast_dates = c(
    "2021-11-22", "2021-12-29", "2022-01-30",
    "2022-02-27"
  ),
  age_groups_covid = NULL,
  permutations_covid = TRUE,
  filter_ref_dates_noro = NULL
)
