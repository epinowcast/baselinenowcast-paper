#' Get aggregated data as of a certain date from long tidy dataframe
#'
#' @param long_df Dataframe of the latest data by reference and report date
#' @param max_delay Integer indicating the maximum delays. Default is `Inf`,
#'    which means we would not restrict to only delays less than the
#'    maximum delay.
#' @param as_of_date String indicating the as of date
#'
#' @autoglobal
#' @importFrom dplyr ungroup mutate filter group_by summarise
#' @importFrom lubridate ymd
#' @returns Data.frame summarised by reference time as of the as of date
get_eval_data_from_long_df <- function(long_df,
                                       as_of_date,
                                       max_delay = Inf) {
  eval_df <- long_df |>
    mutate(
      delay = as.integer(difftime(report_date,
        reference_date,
        units = "days"
      ))
    ) |>
    filter(
      report_date <= ymd(as_of_date),
      delay <= max_delay
    )
  if ("age_group" %in% c(colnames(eval_df))) {
    eval_df_sum <- eval_df |>
      group_by(reference_date, age_group) |>
      summarise(
        observed = sum(count, na.rm = TRUE)
      )
  } else {
    eval_df_sum <- eval_df |>
      group_by(reference_date) |>
      summarise(
        observed = sum(count, na.rm = TRUE)
      )
  }
  eval_final <- eval_df_sum |>
    ungroup() |>
    mutate(
      as_of_date = ymd(as_of_date)
    ) |>
    filter(reference_date <= as_of_date)
  return(eval_final)
}
