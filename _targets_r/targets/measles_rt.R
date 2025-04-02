tar_target(
  measles_rt,
  enw_filter_report_dates(measles_long,
    latest_date = nowcast_date
  ) |>
    # Make reporting triangle by hand
    mutate(delay = as.integer(difftime(report_date,
      reference_date,
      units = "days"
    ))) |>
    select(reference_date, delay, confirm) |>
    filter(delay <= 50, delay >= 0) |>
    pivot_wider(
      names_from = delay,
      values_from = confirm
    ),
  pattern = map(nowcast_dates)
)
