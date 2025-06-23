gen_noro_nowcasts_targets <- list(
  # We pass into the mapping the varying length of the training volume, and
  # then split in half for the delay and uncertainty.
  tar_target(
    name = n_history_delay,
    command = max(
      floor(n_history_training_volume / 2),
      config$noro$max_delay + 1
    )
  ),
  tar_target(
    name = n_history_uncertainty,
    command = max(
      floor(n_history_training_volume / 2),
      n_history_training_volume - n_history_delay
    )
  ),
  tar_target(
    name = comb_nc_noro,
    command = run_noro_nowcast_pipeline(
      noro_df = noro_long,
      nowcast_date = nowcast_dates_noro,
      max_delay = config$noro$max_delay,
      filter_ref_dates = filter_ref_dates,
      n_history_delay = n_history_delay,
      n_history_uncertainty = n_history_uncertainty,
      eval_timeframe = config$norovirus$eval_timeframe,
      days_to_eval = config$norovirus$days_to_eval,
      n_draws = config$n_draws,
      quantiles = config$norovirus$quantiles
    ) |>
      mutate(n_history_training_volume = n_history_training_volume)
  ),


  # Forecast objects ---------------------------------------------------------
  tar_target(
    name = su_sample_noro,
    command = scoringutils::as_forecast_sample(
      data = comb_nc_noro,
      forecast_unit = c(
        "nowcast_date",
        "reference_date",
        "model",
        "n_history_delay",
        "n_history_uncertainty"
      ),
      observed = "observed",
      predicted = "total_count",
      sample_id = "draw"
    )
  ),
  # Forecast quantiles as a scoringutils forecast object
  tar_target(
    name = su_quantile_noro,
    command = scoringutils::as_forecast_quantile(
      data = su_sample_noro,
      probs = config$norovirus$quantiles
    )
  ),
  tar_target(
    name = su_quantile_noro_plot,
    command = scoringutils::as_forecast_quantile(
      data = su_sample_noro,
      probs = config$norovirus$quantiles
    )
  ),
  # Get a wide dataframe with only 50th and 90th for plotting
  tar_target(
    name = summary_nowcast_noro,
    command = su_quantile_noro_plot |>
      pivot_wider(
        names_from = "quantile_level",
        values_from = "predicted",
        names_prefix = "q_"
      ) |> left_join(data_as_of_df, by = "reference_date")
  ),
  ## Scores--------------------------------------------------------------------
  tar_target(
    name = scores_sample_noro,
    command = scoringutils::score(su_sample_noro)
  ),
  tar_target(
    name = scores_quantile_noro,
    command = scoringutils::score(su_quantile_noro)
  ),
  tar_target(
    name = coverage_noro,
    command = scoringutils::get_coverage(
      su_quantile_noro,
      by = c(
        "model",
        "nowcast_date",
        "reference_date"
      )
    )
  )
)
