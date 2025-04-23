kit_nowcast_targets <- list(

  # Get the url
  tar_target(
    name = kit_nowcast_url,
    command = get_kit_nowcast_url(
      prefix = config$covid$KIT_nowcast_url_prefix,
      nowcast_date = nowcast_dates_covid
    )
  ),
  ### Load and clean the nowcasts for each date
  tar_target(
    name = kit_nowcast,
    command = readr::read_csv(kit_nowcast_url)
  )
  # Filter to only what you need (age groups and national, no regions)
)
