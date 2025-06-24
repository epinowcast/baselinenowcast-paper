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
    name = plot_mean_delay_t_by_wday,
    command = get_plot_mean_delay_t_by_wday(
      delay_dfs = all_delay_dfs_noro
    )
  ),
  tar_target(
    name = plot_cdf_by_weekday,
    command = get_plot_cdf_by_weekday(delay_dfs = all_delay_dfs_noro)
  ),
  tar_target(
    name = fig_noro,
    command = make_fig_noro(
      plot_noro_nowcasts,
      bar_chart_wis_noro,
      rel_wis_by_week_noro,
      rel_wis_by_weekday,
      plot_mean_delay_t_by_wday,
      plot_cdf_by_weekday,
      fig_file_name = "noro"
    )
  ),
  # Supplement ---------------------------------------------------------------
  tar_target(
    name = wis_over_time,
    command = get_plot_wis_over_time_noro(noro_scores,
      fig_file_name = "wis_over_time_noro"
    )
  ),
  tar_target(
    name = wis_by_weekday,
    command = get_plot_wis_by_weekday(noro_scores,
      fig_file_name = "wis_by_weekday"
    )
  ),
  tar_target(
    name = coverage_by_model_noro,
    command = get_plot_coverage_by_model_noro(noro_coverage,
      fig_file_name = "noro_coverage"
    )
  ),
  tar_target(
    name = coverage_by_model_wday_noro,
    command = get_plot_cov_by_mod_wday_noro(noro_coverage,
      fig_file_name = "noro_cov_wday"
    )
  )
)
