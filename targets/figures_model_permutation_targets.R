figures_model_permutation_targets <- list(
  tar_target(
    name = plot_nowcasts_over_time_mp,
    command = get_plot_nowcast_perms_over_time(combined_nowcasts_mp,
      age_group_to_plot = "00+",
      horizon_to_plot = -14
    )
  ),
  tar_target(
    name = bar_chart_wis_by_model_perm,
    command = get_plot_bar_chart_sum_scores_mp(scores_mp,
      strata = "age groups"
    )
  )
)
