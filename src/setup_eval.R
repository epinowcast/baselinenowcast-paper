source(file.path("src", "write_config.R"))

write_config(
  noro_nowcast_dates = NULL,
  covid_nowcast_dates = NULL,
  age_groups_covid = NULL,
  permutations_covid = TRUE,
  n_training_volume_noro = NULL,
  filter_ref_dates_noro = NULL
)
