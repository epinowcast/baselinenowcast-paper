get_rep_tri_from_long_df <- function(long_df,
                                     nowcast_date,
                                     max_delay) {
  rep_tri_df <- enw_filter_report_dates(
    obs = long_df,
    latest_date = nowcast_date
  ) |>
    # Make reporting triangle by hand
    mutate(
      delay = as.integer(difftime(report_date,
        reference_date,
        units = "days"
      )),
      nowcast_date = nowcast_date
    ) |>
    select(reference_date, nowcast_date, delay, confirm) |>
    filter(delay <= max_delay, delay >= 0) |>
    pivot_wider(
      names_from = delay,
      values_from = confirm
    )

  return(rep_tri_df)
}
