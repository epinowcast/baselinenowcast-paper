figures_hub_validation_targets <- list(
  tar_target(
    name = horiz_bar_chart_sum_scores,
    command = get_plot_bar_chart_sum_scores(
      joined_scores = all_scores_covid, # Will change this
      strata = "age groups"
    )
  )
)
