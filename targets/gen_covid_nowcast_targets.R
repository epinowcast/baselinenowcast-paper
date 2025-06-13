gen_covid_nowcast_targets <- list(
  tar_target(
    name = list_of_outputs,
    command = run_covid_nowcast_pipeline(
      long_df_all_strata = covid_long_all_strata,
      nowcast_date = nowcast_dates_covid,
      max_delay = config$covid$max_delay,
      age_group_to_nowcast = age_group_to_nowcast,
      borrow = borrow,
      partial_rep_tri = partial_rep_tri,
      n_history_delay = n_history_delay,
      n_history_uncertainty = n_history_uncertainty,
      eval_timeframe = config$covid$eval_timeframe,
      days_to_eval = config$covid$days_to_eval,
      n_draws = config$n_draws,
      quantiles = config$covid$quantiles,
      plot_quantiles = config$plot_quantiles,
      k = 7,
      fun_to_aggregate = sum
    )
  ),
  tar_target(
    name = summary_nowcast_covid,
    command = list_of_outputs[["summary_nowcast_covid"]]
  ),
  tar_target(
    name = scores_quantile_covid,
    command = list_of_outputs[["scores_quantile_covid"]]
  ),
  tar_target(
    name = coverage_covid,
    command = list_of_outputs[["coverage_covid"]]
  ),
  tar_target(
    name = pt_nowcast_7d,
    command = list_of_outputs[["pt_nowcast_7d"]]
  ),
  tar_target(
    name = mean_delay_df,
    command = list_of_outputs[["mean_delay_df"]]
  ),
  tar_target(
    name = delay_df,
    command = list_of_outputs[["delay_df"]]
  )
)
