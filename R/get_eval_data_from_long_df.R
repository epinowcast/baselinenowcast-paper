get_eval_data_from_long_df <- function(long_df,
                                       as_of_date) {
  eval_df <- long_df |>
    filter(report_date <= ymd(as_of_date)) |>
    group_by(reference_date) |>
    summarise(
      observed = sum(confirm, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
      as_of_date = ymd(as_of_date)
    ) |>
    filter(reference_date <= as_of_date)
  return(eval_df)
}
