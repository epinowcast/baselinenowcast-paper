gen_covid_nowcast_targets <- list(
  tar_target(
    name = covid_long,
    command = covid_long_all_strata |>
      filter(age_group == age_group_to_nowcast)
  ),
  tar_target(
    name = long_df_for_borrow_delay,
    command = {
      if (isTRUE(borrow_delay)) {
        covid_long |> filter(age_group == "00+")
      } else {
        NULL
      }
    }
  ),
  tar_target(
    name = samples_nowcast_covid,
    command = run_baselinenowcast_pipeline(
      long_df = covid_long,
      nowcast_date = nowcast_dates_covid,
      max_delay = config$covid$max_delay,
      n_history_delay = n_history_delay,
      n_history_uncertainty = n_history_uncertainty,
      n_draws = config$n_draws,
      long_df_for_borrow = long_df_for_borrow,
      borrow_delay = borrow_delay,
      borrow_uncertainty = borrow_uncertainty
    ),
    format = "rds"
  ),
  # Get evaluation data to join
  tar_target(
    name = eval_data,
    command = get_eval_data_from_long_df(
      long_df = covid_long,
      as_of_date = ymd(nowcast_dates_covid) + days(config$covid$eval_timeframe)
    )
  ),
  # Get as of data we want to join
  tar_target(
    name = data_as_of,
    command = covid_long |>
      filter(report_date <= nowcast_dates_covid) |>
      group_by(reference_date) |>
      summarise(
        data_as_of = sum(count, na.rm = TRUE)
      )
  ),
  # Join eval data to the subset of the nowcast that we are evaluating
  tar_target(
    name = comb_nc_covid,
    command = samples_nowcast_covid |>
      filter(reference_date >=
        ymd(nowcast_dates_covid) - days(config$covid$days_to_eval - 1)) |>
      left_join(eval_data, by = "reference_date") |>
      mutate(age_group = age_group_to_nowcast)
  ),
  # Forecast objects ----------------------------------------------------------
  tar_target(
    name = su_sample_covid,
    command = scoringutils::as_forecast_sample(
      data = comb_nc_covid,
      # All the metadata we will want to keep track of
      forecast_unit = c(
        "nowcast_date",
        "reference_date",
        "age_group",
        "model",
        "n_history_delay",
        "n_history_uncertainty",
        "borrow_delay",
        "borrow_uncertainty"
      ),
      observed = "observed",
      predicted = "total_count",
      sample_id = "draw"
    )
  ),
  # Forecast quantiles as a scoringutils forecast object
  tar_target(
    name = su_quantile_covid,
    command = scoringutils::as_forecast_quantile(
      data = su_sample_covid,
      probs = config$covid$quantiles
    )
  ),
  # Get a wide dataframe with only 50th and 90th for plotting
  tar_target(
    name = summary_nowcast_covid,
    command = su_quantile_covid |>
      filter(quantile_level %in% config$plot_quantiles) |>
      pivot_wider(
        names_from = "quantile_level",
        values_from = "predicted",
        names_prefix = "q_"
      ) |> left_join(data_as_of, by = "reference_date")
  ),
  # Scores--------------------------------------------------------------------
  tar_target(
    name = scores_sample_covid,
    command = scoringutils::score(su_sample_covid)
  ),
  tar_target(
    name = scores_quantile_covid,
    command = scoringutils::score(su_quantile_covid)
  ),
  # Breakdown by overprediction, underprediction, dispersion
  tar_target(
    name = observed_data,
    command = eval_data |>
      filter(
        reference_date >=
          ymd(nowcast_dates_covid) - days(config$covid$days_to_eval - 1),
        reference_date <= ymd(nowcast_dates_covid)
      ) |>
      pull(observed),
    format = "rds"
  ),
  tar_target(
    name = predicted_matrix,
    command = su_quantile_covid |>
      select(quantile_level, predicted, reference_date) |>
      pivot_wider(
        names_from = quantile_level,
        values_from = predicted
      ) |>
      select(-reference_date) |>
      as.matrix(),
    format = "rds"
  ),
  tar_target(
    name = coverage_covid,
    command = scoringutils::get_coverage(
      su_quantile_covid |>
        mutate(model = "baselinenowcast"),
      by = c(
        "nowcast_date",
        "reference_date",
        "age_group",
        "model",
        "n_history_delay",
        "n_history_uncertainty",
        "borrow_delay",
        "borrow_uncertainty"
      )
    )
  )
)
