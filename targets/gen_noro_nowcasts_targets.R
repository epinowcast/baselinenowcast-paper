gen_noro_nowcasts_targets <- list(
  # Nowcasts------------------------------------------------------------------
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
  # Get as of data we want to join
  tar_target(
    name = data_as_of,
    command = noro_long |>
      filter(report_date <= nowcast_dates_noro) |>
      group_by(reference_date) |>
      summarise(
        data_as_of = sum(count, na.rm = TRUE)
      )
  ),
  # Join eval data to the subset of the nowcast that we are evaluating
  tar_target(
    name = comb_nc_noro,
    command = samples_nowcast_noro |>
      filter(reference_date >=
        ymd(nowcast_date) - days(config$norovirus$days_to_eval - 1)) |>
      left_join(eval_data, by = "reference_date")
  ),
  # Forecast objects ----------------------------------------------------------
  tar_target(
    name = su_sample_noro,
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
    name = su_quantile_noro,
    command = scoringutils::as_forecast_quantile(
      data = su_sample_noro,
      probs = config$Hub_quantiles
    )
  ),
  # Get a wide dataframe with only 50th and 95th for plotting
  tar_target(
    name = summary_nowcast_noro,
    command = su_quantile_noro |>
      filter(quantile_level %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
      pivot_wider(
        names_from = "quantile_level",
        values_from = "predicted",
        names_prefix = "q_"
      ) |> left_join(data_as_of, by = "reference_date")
  ),
  # Scores--------------------------------------------------------------------
  tar_target(
    name = scores_sample_noro,
    command = scoringutils::score(su_sample_noro)
  ),
  # start nolint
  # tar_target(
  #   name = overpred_noro,
  #   command = scoringutils::overprediction_sample(observed = su_sample_noro$observed[su_sample_noro$sample_id ==1],
  #                                                 predicted = as.matrix(su_sample_noro |>
  #                                                                         select(sample_id, predicted, reference_date) |>
  #                                                                         pivot_wider(names_from = sample_id,
  #                                                                                     values_from = predicted) |>
  #                                                                         select(-reference_date))
  #   )
  # ),
  # end nolint
  tar_target(
    name = scores_quantile_noro,
    command = scoringutils::score(su_quantile_noro)
  ),
  tar_target(
    name = coverage_noro,
    command = scoringutils::get_coverage(
      su_quantile_noro |>
        mutate(model = "baselinenowcast"),
      by = c("model", "nowcast_date")
    )
  )
)
