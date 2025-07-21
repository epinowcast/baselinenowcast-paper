load_data_targets <- list(

  ### Load and clean norovirus data to create long tidy dataframe--------------
  tar_target(
    name = noro_long,
    command = # readr::read_csv(config$norovirus$url) |>
    # Loads in the local real data to be fit on
      readr::read_csv(file.path(
        "output", "data", "noro", "original",
        "cases.csv"
      )) |>
        mutate(
          reference_date = specimen_date,
          report_date = specimen_date + days(days_to_reported)
        ) |>
        rename(count = target) |>
        select(reference_date, report_date, count)
  ),

  ## Load and clean German Nowcast Hub data ----------------------------------
  tar_target(
    name = covid_long_all_strata,
    command = get_covid_data(url = config$covid$url)
  )
)
