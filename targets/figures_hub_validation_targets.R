figures_hub_validation_targets <- list(
  tar_target(
    name = horiz_bar_chart_sum_scores,
    command = get_plot_bar_chart_sum_scores(
      joined_scores = validation_scores,
      strata = "age groups"
    )
  ),
  tar_target(
    name = combined_nowcasts,
    command = all_nowcasts_covid |>
      filter(
        age_group == "00+",
        model == "base",
        n_history_delay == 60,
        n_history_uncertainty == 60,
        borrow == FALSE,
        partial_rep_tri == TRUE
      ) |>
      mutate(nowcast_date = as.Date(nowcast_date)) |>
      select(colnames(all_nowcasts_kit)) |>
      bind_rows(all_nowcasts_kit |> filter(age_group == "00+")) |>
      mutate(
        horizon = as.integer(reference_date - nowcast_date),
        model = ifelse(model == "base", "baselinenowcast", model)
      )
  ),
  tar_target(
    name = plot_nowcasts_over_time,
    command = get_plot_nowcasts_over_time(combined_nowcasts,
      horizon_to_plot = -14,
      facet = FALSE
    )
  )
)
