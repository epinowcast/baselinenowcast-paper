gen_noro_nowcasts_targets <- list(
  tar_target(
    name = samples_nowcast_noro,
    command = run_baselinenowcast_pipeline(
      long_df = noro_long,
      nowcast_date = nowcast_dates_noro,
      max_delay = config$norovirus$max_delay,
      n_history_delay = config$norovirus$n_history_delay,
      n_history_uncertainty = config$norovirus$n_history_uncertainty,
      n_draws = config$n_draws
    ),
    format = "rds"
  ),
  # Get evaluation data to join
  tar_target(
    name = eval_data,
    command = get_eval_data_from_long_df(
      long_df = noro_long,
      as_of_date = ymd(nowcast_dates_noro) + days(80)
    )
  ),
  # Get only the
  # Convert to a forecast sample object for scoringutils
  # tar_target(
  #   name = su_forecast_sample,
  #   command = scoringutils::as_forecast_sample(
  #     data = samples_nowcast_noro,
  #     observed = observed,
  #     predicted = "total_count"
  #   )
  # )
  tar_target(
    name = quantiled_nowcast_noro,
    command = trajectories_to_quantiles(
      trajectories = samples_nowcast_noro,
      quantiles = config$Hub_quantiles,
      timepoint_cols = "reference_date",
      value_col = "total_count",
      id_cols = "nowcast_date"
    )
  ),
  # Get a wide dataframe with only 50th and 95th for plotting
  tar_target(
    name = summary_nowcast_noro,
    command = quantiled_nowcast_noro |>
      filter(quantile_level %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
      pivot_wider(
        names_from = quantile_level,
        values_from = quantile_value,
        names_prefix = "q_"
      )
  )
)
