EDA_plot_targets <- list(
  # Data plots
  tar_target(
    name = plot_measles_data,
    command = get_plot_data_as_of(
      final_df = measles_long,
      as_of_dates = c(
        "2013-07-01", "2013-10-01",
        "2014-02-25"
      ),
      pathogen = "Measles"
    ),
    format = "rds"
  ),
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
      as_of_date = ymd(max(config$norovirus$nowcast_dates)) + days(config$eval_timeframe)
    )
  ),
  # Make sure quantiled nowcasts are performing reasonably.
  tar_target(
    name = plot_noro_nowcasts,
    command = get_plot_mult_nowcasts(
      all_nowcasts = all_nowcasts_noro,
      final_summed_data = final_eval_data_noro,
      pathogen = "Norovirus"
    )
  )
)
