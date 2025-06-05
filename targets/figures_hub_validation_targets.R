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
  tar_target(
    name = horiz_bar_chart_coverage_ntl,
    command = get_plot_bar_chart_coverage(validation_coverage,
      strata = "national"
    )
  ),
  tar_target(
    name = horiz_bar_chart_coverage_ag,
    command = get_plot_bar_chart_coverage(validation_coverage,
      strata = "age groups"
    )
  )
)
