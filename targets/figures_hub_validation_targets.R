figures_hub_validation_targets <- list(
  tar_target(
    name = horiz_bar_chart_sum_scores_ag,
    command = get_plot_bar_chart_sum_scores(
      joined_scores = validation_scores,
      strata = "age groups"
    )
  ),
  tar_target(
    name = horiz_bar_chart_sum_scores_ntl,
    command = get_plot_bar_chart_sum_scores(
      joined_scores = validation_scores,
      strata = "national"
    )
  ),
  tar_target(
    name = plot_nowcasts_over_time,
    command = get_plot_nowcasts_over_time(combined_nowcasts,
      age_group_to_plot = "00+",
      horizon_to_plot = -14,
      facet = FALSE
    )
  ),
  tar_target(
    name = plot_nowcasts_over_time_h_25,
    command = get_plot_nowcasts_over_time(combined_nowcasts,
      age_group_to_plot = "00+",
      horizon_to_plot = -25,
      facet = TRUE
    )
  ),
  tar_target(
    name = plot_nowcasts_over_time_h_7,
    command = get_plot_nowcasts_over_time(combined_nowcasts,
      age_group_to_plot = "00+",
      horizon_to_plot = -7,
      facet = TRUE
    )
  ),
  tar_target(
    name = plot_wis_comp_over_time_ntl,
    command = get_plot_wis_over_time(scores_over_time_ntl,
      strata = "national"
    )
  ),
  tar_target(
    name = plot_wis_comp_over_time_ag,
    command = get_plot_wis_over_time(scores_over_time_ag,
      strata = "age groups"
    )
  ),
  tar_target(
    name = bar_chart_scores_by_age_group,
    command = get_plot_score_by_age_group(scores_by_age_group)
  ),
  tar_target(
    name = plot_mean_delay_over_time_by_age,
    command = get_plot_mean_delay_over_time(delay_over_time_validation)
  ),
  tar_target(
    name = plot_mean_cdf_delay_by_age,
    command = get_plot_of_delay_cdf_by_age(mean_delay_validation)
  ),
  # Supplemental figures------------------------------------------------------
  tar_target(
    name = plot_rel_wis_by_age_group,
    command = get_plot_rel_wis_by_age_group(
      scores_by_age_group,
      fig_file_name = "rel_wis_by_age_group"
    )
  ),
  tar_target(
    name = plot_mean_wis_by_horizon_ntl,
    command = get_plot_mean_wis_by_horizon(
      validation_scores,
      strata = "national",
      fig_file_name = "mean_wis_by_horizon_ntl"
    )
  ),
  tar_target(
    name = plot_mean_wis_by_horizon_ag,
    command = get_plot_mean_wis_by_horizon(
      validation_scores,
      strata = "age groups",
      fig_file_name = "mean_wis_by_horizon_ag",
    )
  ),
  tar_target(
    name = plot_rel_wis_by_horizon_ntl,
    command = get_plot_rel_wis_by_horizon(
      validation_scores,
      strata = "national",
      fig_file_name = "rel_wis_by_horizon_ntl"
    )
  ),
  tar_target(
    name = plot_rel_wis_by_horizon_ag,
    command = get_plot_rel_wis_by_horizon(
      validation_scores,
      strata = "age groups",
      fig_file_name = "rel_wis_by_horizon_ag"
    )
  ),
  tar_target(
    name = plot_rel_wis_by_horizon_ag_revised,
    command = get_plot_rel_wis_by_horizon(
      validation_scores,
      strata = "age groups",
      KIT_comparison_model = "KIT simple nowcast revised",
      fig_file_name = "rel_wis_comp_revised_ag"
    )
  ),
  tar_target(
    name = plot_rel_wis_by_horizon_ntl_revised,
    command = get_plot_rel_wis_by_horizon(
      validation_scores,
      strata = "national",
      KIT_comparison_model = "KIT simple nowcast revised",
      fig_file_name = "rel_wis_comp_revised_ntl"
    )
  ),
  tar_target(
    name = horiz_bar_chart_coverage_ntl,
    command = get_plot_bar_chart_coverage(
      validation_coverage,
      strata = "national",
      fig_file_name = "bar_chart_cov_ntl"
    )
  ),
  tar_target(
    name = horiz_bar_chart_coverage_ag,
    command = get_plot_bar_chart_coverage(
      validation_coverage,
      strata = "age groups",
      fig_file_name = "bar_chart_cov_ag"
    )
  ),
  tar_target(
    name = plot_coverage_by_horizon_ntl,
    command = get_plot_coverage_by_horizon(
      validation_coverage,
      strata = "national",
      fig_file_name = "cov_by_horizon_ntl"
    )
  ),
  tar_target(
    name = plot_coverage_by_horizon_ag,
    command = get_plot_coverage_by_horizon(
      validation_coverage,
      strata = "age groups",
      fig_file_name = "cov_by_horizon_ag"
    )
  ),
  tar_target(
    name = plot_coverage_by_age_group,
    command = get_plot_coverage_by_age_group(
      validation_coverage,
      fig_file_name = "cov_by_age_group"
    )
  ),

  # Make panels---------------------------------------------------------
  tar_target(
    name = fig_hub_validation,
    command = make_fig_hub_validation(
      plot_nowcasts_over_time,
      horiz_bar_chart_sum_scores_ag,
      plot_wis_comp_over_time_ag,
      bar_chart_scores_by_age_group,
      plot_mean_delay_over_time_by_age,
      plot_mean_cdf_delay_by_age,
      fig_file_name = "fig_hub_validation",
      fig_file_dir = file.path("output", "figs"),
      save = TRUE
    )
  )
)
