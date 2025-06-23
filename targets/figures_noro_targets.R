figures_noro_targets <- list(
  tar_target(
    name = plot_noro_nowcasts,
    command = get_plot_mult_nowcasts_noro(
      all_nowcasts = noro_nowcasts,
      pathogen = "Norovirus",
      title = "Model comparison" # nolint
    )
  ),
  tar_target(
    name = bar_chart_wis_noro,
    command = get_bar_chart_sum_scores_noro(noro_scores)
  ),
  tar_target(
    name = rel_wis_by_week_noro,
    command = get_plot_rel_wis_over_time(noro_scores)
  ),
  tar_target(
    name = rel_wis_by_weekday,
    command = get_plot_rel_wis_by_weekday(noro_scores)
  ),
  tar_target(
    name = plot_mean_delay_over_time_by_weekday,
    command = get_plot_mean_delay_over_time_by_weekday(
      delay_dfs = all_delay_dfs_noro
    )
  ),
  tar_target(
    name = plot_cdf_by_weekday,
    command = get_plot_cdf_by_weekday(delay_dfs = all_delay_dfs_noro)
  )
)
