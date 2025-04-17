#' Get reporting triangle from long tidy dataframe
#'
#' @param long_df Dataframe of the latest data by reference and report date
#' @param nowcast_date String indicating data to nowcast as of
#' @param max_delay Integer indicating the maximum delays
#'
#' @autoglobal
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select filter mutate
#' @importFrom epinowcast enw_filter_report_dates
#' @returns Dataframe in the form of a reporting triangle
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
    select(reference_date, nowcast_date, delay, count) |>
    filter(delay <= max_delay, delay >= 0) |>
    pivot_wider(
      names_from = delay,
      values_from = count
    )

  return(rep_tri_df)
}
