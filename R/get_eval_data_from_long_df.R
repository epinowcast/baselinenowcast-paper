#' Get aggregated data as of a certain date from long tidy dataframe
#'
#' @param long_df Dataframe of the latest data by reference and report date
#' @param as_of_date String indicating the as of date
#'
#' @autoglobal
#' @importFrom dplyr ungroup mutate filter group_by summarise
#' @importFrom lubridate ymd
#' @returns Data.frame summarised by reference time as of the as of date
get_eval_data_from_long_df <- function(long_df,
                                       as_of_date) {
  eval_df <- long_df |>
    filter(report_date <= ymd(as_of_date)) |>
    group_by(reference_date) |>
    summarise(
      observed = sum(count, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
      as_of_date = ymd(as_of_date)
    ) |>
    filter(reference_date <= as_of_date)
  return(eval_df)
}
