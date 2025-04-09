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
      as_of_date = ymd(nowcast_dates_noro) + days(config$eval_timeframe)
    )
  ),
  # Join eval data to the subset of the nowcast that we are evaluating
  tar_target(
    name = comb_nc_noro,
    command = samples_nowcast_noro |>
      filter(reference_date >=
        ymd(nowcast_date) - days(config$norovirus$days_to_eval)) |>
      left_join(eval_data, by = "reference_date")
  ),
  # Convert to a forecast sample object for scoringutils
  tar_target(
    name = su_forecast_sample,
    command = scoringutils::as_forecast_sample(
      data = comb_nc_noro,
      forecast_unit = c("nowcast_date", "reference_date"),
      observed = "observed",
      predicted = "total_count",
      sample_id = "draw"
    )
  ),
  # Forecast quantiles as a scoringutils forecast object
  tar_target(
    name = quantiled_nowcast_noro,
    command = scoringutils::as_forecast_quantile(
      data = su_forecast_sample,
      probs = config$Hub_quantiles
    )
  ),
  # Get a wide dataframe with only 50th and 95th for plotting
  tar_target(
    name = summary_nowcast_noro,
    command = quantiled_nowcast_noro |>
      filter(quantile_level %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
      pivot_wider(
        names_from = "quantile_level",
        values_from = "predicted",
        names_prefix = "q_"
      )
  )
)
