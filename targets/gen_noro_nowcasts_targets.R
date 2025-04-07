gen_noro_nowcasts_targets <- list(
  tar_target(
    name = summary_nowcast_noro,
    command = run_baselinenowcast_pipeline(
      long_df = noro_long,
      nowcast_date = nowcast_dates_noro,
      max_delay = config$norovirus$max_delay,
      n_history_delay = config$norovirus$n_history_delay,
      n_history_uncertainty = config$norovirus$n_history_uncertainty,
      n_draws = config$n_draws
    )
  )
)
