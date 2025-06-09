source(file.path("src", "write_config.R"))

write_config(
  noro_nowcast_dates = "2023-10-08",
  covid_nowcast_dates = NULL,
  age_groups_covid = NULL,
  permutations_covid = FALSE,
  n_training_volume_noro = 56,
  filter_ref_dates_noro = FALSE
)
