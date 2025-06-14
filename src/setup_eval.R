source(file.path("src", "write_config.R"))

write_config(
  noro_nowcast_dates = c("2023-12-10", "2024-01-21", "2024-02-25"),
  covid_nowcast_dates = c("2021-12-01", "2022-02-01", "2022-04-01"),
  age_groups_covid = NULL,
  permutations_covid = TRUE,
  n_training_volume_noro = 56,
  filter_ref_dates_noro = FALSE
)
