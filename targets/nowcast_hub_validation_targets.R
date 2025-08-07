nowcast_hub_validation_targets <- list(
  # Nowcasts-----------------------------------------------------------------
  tar_target(
    name = combined_nowcasts,
    command = all_nowcasts_covid |>
      filter(
        model == "base",
        n_history_delay == 60,
        n_history_uncertainty == 60,
        borrow == FALSE,
        partial_rep_tri == TRUE,
        weekday_filter == FALSE,
      ) |>
      mutate(nowcast_date = as.Date(nowcast_date)) |>
      select(colnames(all_nowcasts_kit)) |>
      bind_rows(all_nowcasts_kit) |>
      mutate(
        horizon = as.integer(reference_date - nowcast_date),
        model = ifelse(model == "base", "baselinenowcast", model)
      )
  ),
  tar_target(
    name = delay_over_time_validation,
    command = all_mean_delays |>
      filter(
        model == "base",
        n_history_delay == 60,
        n_history_uncertainty == 60,
        borrow == FALSE,
        partial_rep_tri == TRUE,
        weekday_filter == FALSE
      )
  ),
  tar_target(
    name = mean_delay_validation,
    command = all_delay_dfs |>
      filter(
        model == "base",
        n_history_delay == 60,
        n_history_uncertainty == 60,
        borrow == FALSE,
        partial_rep_tri == TRUE,
        weekday_filter == FALSE
      ) |>
      # Estimate a delay for each age group across nowcast dates
      group_by(age_group, delay_time) |>
      summarise(mean_delay = mean(delay)) |>
      mutate(cdf = cumsum(mean_delay))
  ),
  # Scores --------------------------------------------------------------------
  tar_target(
    name = covid_scores_filtered,
    command = all_scores_covid |>
      filter(
        model == "base",
        n_history_delay == 60,
        n_history_uncertainty == 60,
        borrow == FALSE,
        partial_rep_tri == TRUE,
        weekday_filter == FALSE
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
  tar_target(
    name = scores_over_time_ag,
    command = validation_scores |>
      filter(age_group != "00+") |>
      # Summarise across horizons (reference dates) and age groups
      scoringutils::summarise_scores(by = c(
        "model",
        "nowcast_date"
      ))
  ),
  tar_target(
    name = scores_over_time_ntl,
    command = validation_scores |>
      filter(age_group == "00+") |>
      # Summarise across horizons (reference dates) and age groups
      scoringutils::summarise_scores(by = c(
        "model",
        "nowcast_date"
      ))
  ),
  tar_target(scores_by_age_group,
    command = validation_scores |>
      filter(age_group != "00+") |>
      # Summarise across horizons (reference dates) and nowcast_dates
      scoringutils::summarise_scores(by = c("model", "age_group"))
  ),
  # Coverage ------------------------------------------------------------------
  tar_target(
    name = covid_coverage_filtered,
    command = all_coverage_covid |>
      filter(
        model == "baselinenowcast",
        n_history_delay == 60,
        n_history_uncertainty == 60,
        borrow == FALSE,
        partial_rep_tri == TRUE,
        weekday_filter == FALSE
      ) |>
      select(colnames(all_coverage_kit)) |>
      mutate(
        nowcast_date = ymd(nowcast_date)
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
