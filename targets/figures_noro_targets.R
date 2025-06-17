figures_noro_targets <- list(
  tar_target(
    name = plot_noro_nowcasts,
    command = get_plot_mult_nowcasts_noro(
      all_nowcasts = all_nowcasts_noro,
      pathogen = "Norovirus",
      title = "Model comparison across different baselinenowcast implementations" # nolint
    )
  ),
)
