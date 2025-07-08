figures_noro_targets <- list(
  tar_target(
    name = plot_noro_nowcasts_GAM,
    command = get_plot_mult_nowcasts_noro(
      all_nowcasts = noro_nowcasts |>
        filter(model %in% c("GAM", "baselinenowcast base")),
      facet_title = "GAM"
    )
  ),
  tar_target(
    name = plot_noro_nowcasts_epinowcast,
    command = get_plot_mult_nowcasts_noro(
      all_nowcasts = noro_nowcasts |>
        filter(model %in% c("epinowcast", "baselinenowcast base")),
      facet_title = "epinowcast"
    )
  ),
  tar_target(
    name = plot_noro_nowcasts_Mellor_baseline,
    command = get_plot_mult_nowcasts_noro(
      all_nowcasts = noro_nowcasts |>
        filter(model %in% c("baseline Mellor et al", "baselinenowcast base")),
      facet_title = "baseline Mellor et al"
    )
  ),
  tar_target(
    name = plot_noro_nowcasts_bnc_variants,
    command = get_plot_mult_nowcasts_noro(
      all_nowcasts = noro_nowcasts |>
        filter(!model %in% c("GAM", "epinowcast", "baseline Mellor et al")),
      facet_title = "baselinenowcast variants"
    )
  ),
  tar_target(
    name = rel_wis_by_week_noro_Mellor_baseline,
    command = get_plot_rel_wis_over_time(
      noro_scores |> filter(model %in% c(
        "baseline Mellor et al", "baselinenowcast base"
      ))
    )
  ),
  tar_target(
    name = rel_wis_by_week_noro_GAM,
    command = get_plot_rel_wis_over_time(
      noro_scores |> filter(model %in% c(
        "GAM", "baselinenowcast base"
      ))
    )
  ),
  tar_target(
    name = rel_wis_by_week_noro_epinowcast,
    command = get_plot_rel_wis_over_time(
      noro_scores |> filter(model %in% c(
        "epinowcast", "baselinenowcast base"
      ))
    )
  ),
  tar_target(
    name = rel_wis_by_week_noro_bnc,
    command = get_plot_rel_wis_over_time(
      noro_scores |> filter(!model %in% c(
        "GAM", "epinowcast", "baseline Mellor et al"
      ))
    )
  ),
  tar_target(
    name = panel_A_noro,
    command = make_panel_A_noro(
      plot_noro_nowcasts_Mellor_baseline,
      rel_wis_by_week_noro_Mellor_baseline,
      plot_noro_nowcasts_GAM,
      rel_wis_by_week_noro_GAM,
      plot_noro_nowcasts_epinowcast,
      rel_wis_by_week_noro_epinowcast,
      plot_noro_nowcasts_bnc_variants,
      rel_wis_by_week_noro_bnc
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
    name = wis_by_weekday,
    command = get_plot_wis_by_weekday(noro_scores,
      fig_file_name = "wis_by_weekday"
    )
  ),
  tar_target(
    name = distrib_mean_delay_weekday,
    command = get_plot_distrib_delays(all_delay_dfs_noro)
  ),
  tar_target(
    name = fig_noro,
    command = make_fig_noro(
      panel_A_noro,
      bar_chart_wis_noro,
      rel_wis_by_weekday,
      distrib_mean_delay_weekday,
      wis_by_weekday,
      plot_mean_delay_t_by_wday,
      plot_cdf_by_weekday,
      fig_file_name = "noro"
    )
  ),
  # Supplement ---------------------------------------------------------------
  tar_target(
    name = rel_delay_over_time,
    command = get_plot_rel_delay_t_by_wday(all_delay_dfs_noro,
      fig_file_name = "rel_mean_delay_over_time_noro"
    )
  ),
  tar_target(
    name = wis_over_time,
    command = get_plot_wis_over_time_noro(noro_scores,
      fig_file_name = "wis_over_time_noro"
    )
  ),
  tar_target(
    name = coverage_by_model_noro,
    command = get_plot_cov_by_model_noro(noro_coverage,
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
