tar_target(measles_long, {
  measles_line_list |>
    group_by(reference_date, report_date) |>
    summarise(confirm = n()) |>
    ungroup() |>
    complete(
      reference_date = seq(
        from = min(reference_date),
        to = max(reference_date),
        by = "day"
      ),
      report_date = seq(
        from = min(reference_date),
        to = max(report_date),
        by = "days"
      )
    ) |>
    mutate(confirm = ifelse(is.na(confirm), 0, confirm))
})
