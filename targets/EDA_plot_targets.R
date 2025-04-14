EDA_plot_targets <- list(
  # Data plots
  tar_target(
    name = plot_noro_data,
    command = get_plot_data_as_of(
      final_df = noro_long,
      as_of_dates = c(
        "2023-12-10", "2024-01-21",
        "2024-02-25"
      ),
      pathogen = "Norovirus"
    ),
    format = "rds"
  ),
  tar_target(
    name = final_eval_data_noro,
    command = get_eval_data_from_long_df(
      long_df = noro_long,
      as_of_date = ymd(max(config$norovirus$nowcast_dates)) + days(config$norovirus$eval_timeframe)
    )
  ),
  # Make sure quantiled nowcasts are performing reasonably.
  tar_target(
    name = plot_noro_nowcasts,
    command = get_plot_mult_nowcasts(
      all_nowcasts = all_nowcasts_noro |>
        filter(model == "base", n_history_delay == 60),
      final_summed_data = final_eval_data_noro,
      pathogen = "Norovirus"
    )
  )
)
