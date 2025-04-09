load_data_targets <- list(
  ## Load and clean measles data----------------------------------------------
  tar_target(
    name = measles_line_list,
    command = read.delim(config$measles$url, sep = "") |> # nolint
      mutate(
        reference_date = ymd(onset.date),
        report_date = ymd(report.date)
      ) |>
      select(reference_date, report_date)
  ),

  ### Create long tidy dataframe up until final date---------------------------
  # Will be used to create reporting triangles as of nowcast dates +
  # evaluation data. Contains all info in reporting triangles
  tar_target(
    name = measles_long,
    command = measles_line_list |>
      group_by(reference_date, report_date) |>
      summarise(count = n()) |>
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
      mutate(confirm = ifelse(is.na(count), 0, count))
  ),

  ### Load and clean norovirus data to create long tidy dataframe--------------
  tar_target(
    name = noro_long,
    command = readr::read_csv(config$norovirus$url) |>
      mutate(
        reference_date = specimen_date,
        report_date = specimen_date + days(days_to_reported)
      ) |>
      rename(count = target) |>
      select(reference_date, report_date, count)
  )
)
