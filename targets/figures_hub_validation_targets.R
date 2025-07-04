figures_hub_validation_targets <- list(
  # Filter the plot target inputs to exclude KIT simple nowcast revised-------
  tar_target(
    name = validation_scores2,
    command = validation_scores |>
      filter(model != "KIT simple nowcast revised")
  ),
  tar_target(
    name = combined_nowcasts2,
    command = combined_nowcasts |>
      filter(model != "KIT simple nowcast revised")
  ),
  tar_target(
    name = validation_coverage2,
    command = validation_coverage |>
      filter(model != "KIT simple nowcast revised")
  ),
  tar_target(
    name = scores_by_age_group2,
    command = scores_by_age_group |>
      filter(model != "KIT simple nowcast revised")
  ),
  tar_target(
    name = scores_over_time_ag2,
    command = scores_over_time_ag |>
      filter(model != "KIT simple nowcast revised")
  ),
  tar_target(
    name = scores_over_time_ntl2,
    command = scores_over_time_ntl |>
      filter(model != "KIT simple nowcast revised")
  ),
  # Alt: filter the plot target inputs to exclude KIT simple nowcast ------
  tar_target(
    name = validation_scoresr,
    command = validation_scores |>
      filter(model != "KIT simple nowcast")
  ),
  tar_target(
    name = combined_nowcastsr,
    command = combined_nowcasts |>
      filter(model != "KIT simple nowcast")
  ),
  tar_target(
    name = validation_coverager,
    command = validation_coverage |>
      filter(model != "KIT simple nowcast")
  ),
  tar_target(
    name = scores_by_age_groupr,
    command = scores_by_age_group |>
      filter(model != "KIT simple nowcast")
  ),
  tar_target(
    name = scores_over_time_agr,
    command = scores_over_time_ag |>
      filter(model != "KIT simple nowcast")
  ),
  tar_target(
    name = scores_over_time_ntlr,
    command = scores_over_time_ntl |>
      filter(model != "KIT simple nowcast")
  ),
  # Subplots for Supp Figure Hub validation vs KIT real-time ---------------------------
  tar_target(
    name = horiz_bar_chart_sum_scores_ag,
    command = get_plot_bar_chart_sum_scores(
      joined_scores = validation_scores2,
      strata = "age groups"
    )
  ),
  tar_target(
    name = horiz_bar_chart_sum_scores_ntl,
    command = get_plot_bar_chart_sum_scores(
      joined_scores = validation_scores2,
      strata = "national"
    )
  ),
  tar_target(
    name = plot_nowcasts_over_time,
    command = get_plot_nowcasts_over_time(combined_nowcasts2,
      age_group_to_plot = "00+",
      horizon_to_plot = -14,
      facet = FALSE
    )
  ),
  tar_target(
    name = plot_nowcast_illustration,
    command = get_plot_nowcast_illustration(
      combined_nowcasts2,
      nowcast_dates_to_plot = c(
        "2021-12-29",
        "2022-01-29",
        "2022-02-28",
        "2022-03-29",
        "2022-04-29"
      ),
      age_group_to_plot = "00+",
      facet = FALSE
    )
  ),
  tar_target(
    name = plot_nowcasts_over_time_h_25,
    command = get_plot_nowcasts_over_time(combined_nowcasts2,
      age_group_to_plot = "00+",
      horizon_to_plot = -25,
      facet = TRUE
    )
  ),
  tar_target(
    name = plot_nowcasts_over_time_h_7,
    command = get_plot_nowcasts_over_time(combined_nowcasts2,
      age_group_to_plot = "00+",
      horizon_to_plot = -7,
      facet = TRUE
    )
  ),
  tar_target(
    name = plot_wis_comp_over_time_ntl,
    command = get_plot_wis_over_time(
      scores_over_time_ntl2,
      strata = "national"
    )
  ),
  tar_target(
    name = plot_wis_comp_over_time_ag,
    command = get_plot_wis_over_time(
      scores_over_time_ag2,
      strata = "age groups"
    )
  ),
  tar_target(
    name = bar_chart_scores_by_age_group,
    command = get_plot_score_by_age_group(scores_by_age_group2)
  ),
  tar_target(
    name = plot_mean_delay_over_time_by_age,
    command = get_plot_mean_delay_over_time(delay_over_time_validation)
  ),
  tar_target(
    name = plot_mean_cdf_delay_by_age,
    command = get_plot_of_delay_cdf_by_age(mean_delay_validation)
  ),

  # Subplots for Main Text Figure Hub validation vs KIT nowcast revised -------
  tar_target(
    name = horiz_bar_chart_sum_scores_agr,
    command = get_plot_bar_chart_sum_scores(
      joined_scores = validation_scoresr,
      strata = "age groups"
    )
  ),
  tar_target(
    name = horiz_bar_chart_sum_scores_ntlr,
    command = get_plot_bar_chart_sum_scores(
      joined_scores = validation_scoresr,
      strata = "national"
    )
  ),
  tar_target(
    name = plot_nowcasts_over_timer,
    command = get_plot_nowcasts_over_time(combined_nowcastsr,
      age_group_to_plot = "00+",
      horizon_to_plot = -14,
      facet = FALSE
    )
  ),
  tar_target(
    name = plot_nowcast_illustrationr,
    command = get_plot_nowcast_illustration(
      combined_nowcastsr,
      nowcast_dates_to_plot = c(
        "2021-12-29",
        "2022-01-29",
        "2022-02-28",
        "2022-03-29",
        "2022-04-29"
      ),
      age_group_to_plot = "00+",
      facet = FALSE
    )
  ),
  tar_target(
    name = plot_wis_comp_over_time_ntlr,
    command = get_plot_wis_over_time(
      scores_over_time_ntlr,
      strata = "national"
    )
  ),
  tar_target(
    name = plot_wis_comp_over_time_agr,
    command = get_plot_wis_over_time(
      scores_over_time_agr,
      strata = "age groups"
    )
  ),
  tar_target(
    name = bar_chart_scores_by_age_groupr,
    command = get_plot_score_by_age_group(scores_by_age_groupr)
  ),
  # Supplemental figures------------------------------------------------------
  tar_target(
    name = plot_rel_wis_by_age_group,
    command = get_plot_rel_wis_by_age_group(
      scores_by_age_group2,
      fig_file_name = "rel_wis_by_age_group"
    )
  ),
  tar_target(
    name = plot_mean_wis_by_horizon_ntl,
    command = get_plot_mean_wis_by_horizon(
      validation_scores2,
      strata = "national",
      fig_file_name = "mean_wis_by_horizon_ntl"
    )
  ),
  tar_target(
    name = plot_mean_wis_by_horizon_ag,
    command = get_plot_mean_wis_by_horizon(
      validation_scores2,
      strata = "age groups",
      fig_file_name = "mean_wis_by_horizon_ag",
    )
  ),
  tar_target(
    name = plot_rel_wis_by_horizon_ntl,
    command = get_plot_rel_wis_by_horizon(
      validation_scores2,
      strata = "national",
      fig_file_name = "rel_wis_by_horizon_ntl"
    )
  ),
  tar_target(
    name = plot_rel_wis_by_horizon_ag,
    command = get_plot_rel_wis_by_horizon(
      validation_scores2,
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
      validation_coverage2,
      strata = "national",
      fig_file_name = "bar_chart_cov_ntl"
    )
  ),
  tar_target(
    name = horiz_bar_chart_coverage_ag,
    command = get_plot_bar_chart_coverage(
      validation_coverage2,
      strata = "age groups",
      fig_file_name = "bar_chart_cov_ag"
    )
  ),
  tar_target(
    name = plot_coverage_by_horizon_ntl,
    command = get_plot_coverage_by_horizon(
      validation_coverage2,
      strata = "national",
      fig_file_name = "cov_by_horizon_ntl"
    )
  ),
  tar_target(
    name = plot_coverage_by_horizon_ag,
    command = get_plot_coverage_by_horizon(
      validation_coverage2,
      strata = "age groups",
      fig_file_name = "cov_by_horizon_ag"
    )
  ),
  tar_target(
    name = plot_coverage_by_age_group,
    command = get_plot_coverage_by_age_group(
      validation_coverage2,
      fig_file_name = "cov_by_age_group"
    )
  ),

  # Make panels---------------------------------------------------------
  tar_target(
    name = fig_hub_validation_real_time,
    command = make_fig_hub_validation(
      plot_nowcast_illustration,
      horiz_bar_chart_sum_scores_ag,
      plot_wis_comp_over_time_ag,
      bar_chart_scores_by_age_group,
      plot_mean_delay_over_time_by_age,
      plot_mean_cdf_delay_by_age,
      fig_file_name = "fig_hub_validation_KIT_real_time",
      fig_file_dir = file.path("output", "figs", "supp"),
      save = TRUE
    )
  ),
  tar_target(
    name = fig_hub_validation,
    command = make_fig_hub_validation(
      plot_nowcast_illustrationr,
      horiz_bar_chart_sum_scores_agr,
      plot_wis_comp_over_time_agr,
      bar_chart_scores_by_age_groupr,
      plot_mean_delay_over_time_by_age,
      plot_mean_cdf_delay_by_age,
      fig_file_name = "fig_hub_validation",
      fig_file_dir = file.path("output", "figs"),
      save = TRUE
    )
  )
)
