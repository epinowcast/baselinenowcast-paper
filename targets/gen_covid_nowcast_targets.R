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
      left_join(eval_data, by = "reference_date")
  )
)
