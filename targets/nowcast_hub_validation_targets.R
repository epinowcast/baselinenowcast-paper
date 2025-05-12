nowcast_hub_validation_targets <- list(
  # Scores --------------------------------------------------------------------
  tar_target(
    name = covid_scores_filtered,
    command = all_scores_covid |>
      filter(
        n_history_delay == 60,
        n_history_uncertainty == 60,
        borrow == FALSE,
      ) |>
      select(colnames(all_scores_kit)) |>
      mutate(
        nowcast_date = ymd(nowcast_date),
        model = "baselinenowcast"
      )
  ),
  # Ensures we are only comparing the same set of age groups
  tar_target(
    name = scores_kit,
    command = all_scores_kit |>
      filter(age_group %in% c(unique(covid_scores_filtered$age_group)))
  ),
  tar_target(
    name = validation_scores,
    command = bind_rows(
      covid_scores_filtered,
      scores_kit
    )
  ),
  # Coverage ------------------------------------------------------------------
  tar_target(
    name = covid_coverage_filtered,
    command = all_coverage_covid |>
      filter(
        n_history_delay == 60,
        n_history_uncertainty == 60,
        borrow == FALSE
      ) |>
      select(colnames(all_coverage_kit)) |>
      mutate(
        nowcast_date = ymd(nowcast_date),
        model = "baselinenowcast"
      )
  ),
  # Ensures we are only comparing the same set of age groups
  tar_target(
    name = coverage_kit,
    command = all_coverage_kit |>
      filter(age_group %in% c(unique(covid_coverage_filtered$age_group)))
  ),
  tar_target(
    name = validation_coverage,
    command = bind_rows(
      covid_coverage_filtered,
      coverage_kit
    )
  )
)
