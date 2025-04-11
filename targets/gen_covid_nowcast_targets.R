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
  )
)
