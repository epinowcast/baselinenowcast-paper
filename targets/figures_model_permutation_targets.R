figures_model_permutation_targets <- list(
  tar_target(
    name = plot_nowcasts_over_time_mp,
    command = get_plot_nowcasts_over_time_mp(combined_nowcasts_mp,
      age_group_to_plot = "00+",
      horizon_to_plot = -14
    )
  ),
  tar_target(
    name = bar_chart_wis_by_mp,
    command = get_plot_bar_chart_scores_mp(scores_mp,
      strata = "age groups"
    )
  ),
  tar_target(
    name = rel_wis_over_time_mp,
    command = get_plot_rel_wis_over_time_mp(scores_mp,
      strata = "age groups"
    )
  ),
  tar_target(
    name = bar_chart_coverage_mp,
    command = get_plot_coverage_by_mp(coverage_mp)
  ),
  tar_target(
    name = rel_wis_by_horizon_mp,
    command = get_plot_rel_wis_by_horizon_mp(scores_mp,
      strata = "age groups"
    )
  ),
  tar_target(
    name = rel_decomposed_wis_by_age_group,
    command = get_plot_rel_decomposed_wis(scores_mp,
      facet = TRUE
    )
  ),
  # Supplement-----------------------------------------------------------------
  tar_target(
    name = bar_chart_wis_by_age_group_mp,
    command = get_plot_wis_by_age_group_mp(scores_mp |>
      filter(age_group != "00+"))
  ),
  tar_target(
    name = bar_chart_wis_by_nowcast_horizon,
    command = get_plot_wis_by_horizon_mp(scores_mp,
      strata = "age groups"
    )
  ),
  tar_target(
    name = bar_chart_wis_by_nowcast_week,
    command = get_plot_wis_by_week_mp(scores_mp,
      strata = "age groups"
    )
  )
)
