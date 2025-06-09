figures_model_permutation_targets <- list(
  tar_target(
    name = plot_nowcasts_over_time_mp,
    command = get_plot_nowcast_perms_over_time(combined_nowcasts_mp,
      age_group_to_plot = "00+",
      horizon_to_plot = -14
    )
  )
)
