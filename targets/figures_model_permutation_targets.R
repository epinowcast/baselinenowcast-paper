figures_model_permutation_targets <- list(
  tar_target(
    name = plot_nowcasts_over_time_mp,
    command = get_plot_nowcasts_over_time_mp(combined_nowcasts_mp,
      age_group_to_plot = "00+",
      horizon_to_plot = -14,
      fig_file_name = "nowcasts_over_time_ntl"
    )
  ),
  # Nowcasts over time with rel wis underlaid--------------------------------
  # Plots for 00-04 age group
  tar_target(
    name = plot_nowcasts_t_mp_00_04_borrow,
    command = get_plot_nowcasts_over_time_mp(combined_nowcasts_mp,
      age_group_to_plot = "00-04",
      permutation_grouping = "Borrow for delay and uncertainty estimation",
      horizon_to_plot = 0,
      fig_file_name = "nowcasts_over_time_00_04_borrow"
    )
  ),
  tar_target(
    name = plot_nowcasts_t_mp_00_04_rep_tri,
    command = get_plot_nowcasts_over_time_mp(combined_nowcasts_mp,
      age_group_to_plot = "00-04",
      permutation_grouping = "Reporting triangle completeness",
      horizon_to_plot = 0,
      fig_file_name = "nowcasts_over_time_00_04_rep_tri"
    )
  ),
  tar_target(
    name = plot_nowcasts_t_mp_00_04_volume,
    command = get_plot_nowcasts_over_time_mp(combined_nowcasts_mp,
      age_group_to_plot = "00-04",
      permutation_grouping = "Training volume",
      horizon_to_plot = 0,
      fig_file_name = "nowcasts_over_time_00_04_volume"
    )
  ),
  tar_target(
    name = plot_nowcasts_t_mp_00_04_wday,
    command = get_plot_nowcasts_over_time_mp(combined_nowcasts_mp,
      age_group_to_plot = "00-04",
      permutation_grouping = "Weekday filter",
      horizon_to_plot = 0,
      fig_file_name = "nowcasts_over_time_00_04_wday"
    )
  ),
  # Make the ones for the 60-79 year old age group
  tar_target(
    name = plot_nowcasts_t_mp_60_79_borrow,
    command = get_plot_nowcasts_over_time_mp(combined_nowcasts_mp,
      age_group_to_plot = "60-79",
      permutation_grouping = "Borrow for delay and uncertainty estimation",
      horizon_to_plot = 0,
      fig_file_name = "nowcasts_over_time_60_79_borrow"
    )
  ),
  tar_target(
    name = plot_nowcasts_t_mp_60_79_rep_tri,
    command = get_plot_nowcasts_over_time_mp(combined_nowcasts_mp,
      age_group_to_plot = "60-79",
      permutation_grouping = "Reporting triangle completeness",
      horizon_to_plot = 0,
      fig_file_name = "nowcasts_over_time_60_79_rep_tri"
    )
  ),
  tar_target(
    name = plot_nowcasts_t_mp_60_79_volume,
    command = get_plot_nowcasts_over_time_mp(combined_nowcasts_mp,
      age_group_to_plot = "60-79",
      permutation_grouping = "Training volume",
      horizon_to_plot = 0,
      fig_file_name = "nowcasts_over_time_60_79_volume"
    )
  ),
  tar_target(
    name = plot_nowcasts_t_mp_60_79_wday,
    command = get_plot_nowcasts_over_time_mp(combined_nowcasts_mp,
      age_group_to_plot = "60-79",
      permutation_grouping = "Weekday filter",
      horizon_to_plot = 0,
      fig_file_name = "nowcasts_over_time_60_79_wday"
    )
  ),
  # Make the relative WIS subplots --------------------------------
  tar_target(
    name = rel_wis_over_time_mp_00_04_borrow,
    command = get_plot_rel_wis_over_time_mp(scores_mp,
      permutation_grouping = "Borrow for delay and uncertainty estimation",
      strata = "age groups",
      age_group_to_plot = "00-04"
    )
  ),
  tar_target(
    name = rel_wis_over_time_mp_00_04_rep_tri,
    command = get_plot_rel_wis_over_time_mp(scores_mp,
      permutation_grouping = "Reporting triangle completeness",
      strata = "age groups",
      age_group_to_plot = "00-04"
    )
  ),
  tar_target(
    name = rel_wis_over_time_mp_00_04_volume,
    command = get_plot_rel_wis_over_time_mp(scores_mp,
      permutation_grouping = "Training volume",
      strata = "age groups",
      age_group_to_plot = "00-04"
    )
  ),
  tar_target(
    name = rel_wis_over_time_mp_00_04_wday,
    command = get_plot_rel_wis_over_time_mp(scores_mp,
      permutation_grouping = "Weekday filter",
      strata = "age groups",
      age_group_to_plot = "00-04"
    )
  ),
  tar_target(
    name = rel_wis_over_time_mp_60_79_borrow,
    command = get_plot_rel_wis_over_time_mp(scores_mp,
      permutation_grouping = "Borrow for delay and uncertainty estimation",
      strata = "age groups",
      age_group_to_plot = "60-79"
    )
  ),
  tar_target(
    name = rel_wis_over_time_mp_60_79_rep_tri,
    command = get_plot_rel_wis_over_time_mp(scores_mp,
      permutation_grouping = "Reporting triangle completeness",
      strata = "age groups",
      age_group_to_plot = "60-79"
    )
  ),
  tar_target(
    name = rel_wis_over_time_mp_60_79_volume,
    command = get_plot_rel_wis_over_time_mp(scores_mp,
      permutation_grouping = "Training volume",
      strata = "age groups",
      age_group_to_plot = "60-79"
    )
  ),
  tar_target(
    name = rel_wis_over_time_mp_60_79_wday,
    command = get_plot_rel_wis_over_time_mp(scores_mp,
      permutation_grouping = "Weekday filter",
      strata = "age groups",
      age_group_to_plot = "60-79"
    )
  ),
  tar_target(
    name = panel_A_nowcasts_over_time_00_04,
    command = make_panel_A_model_perms(
      plot_nowcasts_t_mp_00_04_borrow,
      rel_wis_over_time_mp_00_04_borrow,
      plot_nowcasts_t_mp_00_04_rep_tri,
      rel_wis_over_time_mp_00_04_rep_tri,
      plot_nowcasts_t_mp_00_04_volume,
      rel_wis_over_time_mp_00_04_volume
    )
  ),
  tar_target(
    name = rel_wis_over_time_mp,
    command = get_plot_rel_wis_over_time_all(
      scores_mp,
      strata = "age groups",
      fig_file_name = "rel_wis_over_time_mp_all_ag"
    )
  ),
  tar_target(
    name = panel_A_nowcasts_over_time_60_79,
    command = make_panel_A_model_perms(
      plot_nowcasts_t_mp_60_79_borrow,
      rel_wis_over_time_mp_60_79_borrow,
      plot_nowcasts_t_mp_60_79_rep_tri,
      rel_wis_over_time_mp_60_79_rep_tri,
      plot_nowcasts_t_mp_60_79_volume,
      rel_wis_over_time_mp_60_79_volume,
      fig_file_name = "model_permutation_comp_60_79",
      fig_file_dir = file.path("output", "figs", "supp"),
      save = TRUE
    )
  ),
  tar_target(
    name = panel_A_both_ags,
    command = make_panel_A_mps_2_ags(
      plot_nowcasts_t_mp_00_04_borrow,
      rel_wis_over_time_mp_00_04_borrow,
      plot_nowcasts_t_mp_00_04_rep_tri,
      rel_wis_over_time_mp_00_04_rep_tri,
      plot_nowcasts_t_mp_00_04_volume,
      rel_wis_over_time_mp_00_04_volume,
      plot_nowcasts_t_mp_00_04_wday,
      rel_wis_over_time_mp_00_04_wday,
      plot_nowcasts_t_mp_60_79_borrow,
      rel_wis_over_time_mp_60_79_borrow,
      plot_nowcasts_t_mp_60_79_rep_tri,
      rel_wis_over_time_mp_60_79_rep_tri,
      plot_nowcasts_t_mp_60_79_volume,
      rel_wis_over_time_mp_60_79_volume,
      plot_nowcasts_t_mp_60_79_wday,
      rel_wis_over_time_mp_60_79_wday
    )
  ),
  tar_target(
    name = bar_chart_wis_by_mp,
    command = get_plot_bar_chart_scores_mp(scores_mp,
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
    name = rel_decomp_wis_by_age_group,
    command = get_plot_rel_decomposed_wis(
      scores_mp |>
        filter(age_group != "00+")
    )
  ),
  # Supplement-----------------------------------------------------------------
  tar_target(
    name = bar_chart_wis_by_age_group_mp,
    command = get_plot_wis_by_age_group_mp(
      scores_mp |>
        filter(age_group != "00+"),
      fig_file_name = "mp_wis_by_age_group"
    )
  ),
  tar_target(
    name = bar_chart_wis_by_nowcast_horizon,
    command = get_plot_wis_by_horizon_mp(scores_mp,
      strata = "age groups",
      fig_file_name = "mp_wis_by_horizon"
    )
  ),
  tar_target(
    name = bar_chart_wis_by_nowcast_week,
    command = get_plot_wis_by_week_mp(scores_mp,
      strata = "age groups",
      fig_file_name = "mp_wis_by_nowcast_week"
    )
  ),

  # Make panels-------------------------------------------------------------
  tar_target(
    name = fig_model_permutations,
    command = make_fig_model_perms(
      panel_A_nowcasts_over_time_00_04,
      bar_chart_wis_by_mp,
      bar_chart_coverage_mp,
      rel_wis_by_horizon_mp,
      rel_decomp_wis_by_age_group,
      fig_file_name = "fig_model_permutations",
      fig_file_dir = file.path("output", "figs"),
      save = TRUE
    )
  ),
  tar_target(
    name = fig_model_permutations_alt,
    command = make_fig_model_perms(
      panel_A_both_ags,
      bar_chart_wis_by_mp,
      bar_chart_coverage_mp,
      rel_wis_by_horizon_mp,
      rel_decomp_wis_by_age_group,
      fig_file_name = "fig_model_permutations_2ags",
      fig_file_dir = file.path("output", "figs"),
      save = TRUE
    )
  )
)
