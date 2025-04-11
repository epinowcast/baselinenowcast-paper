write_config <- function(noro_nowcast_dates = NULL,
                         covid_nowcast_dates = NULL,
                         n_history_delays_noro = NULL,
                         filter_ref_dates_noro = NULL) {
  norovirus_url <- "https://raw.githubusercontent.com/jonathonmellor/norovirus-nowcast/refs/heads/main/outputs/data/cases_with_noise.csv" # nolint
  covid_url <- "https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/main/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv" # nolint

  if (is.null(noro_nowcast_dates)) {
    noro_nowcast_dates <- as.character(
      seq(
        from = ymd("2023-11-05"),
        to = ymd("2024-03-10"),
        by = "week"
      )
    )
  }

  if (is.null(noro_nowcast_dates)) {
    noro_nowcast_dates <- as.character(
      seq(
        from = ymd("2021-11-22"),
        to = ymd("2022-04-29"),
        by = "day"
      )
    )
  }

  # Set up the variations for filtering by wday
  if (is.null(filter_ref_dates_noro) & is.null(n_history_delays_noro)) {
    n_history_delay_noro_orig <- 60 # used by Mellor for their GAM *dbl check*
    n_history_delays_noro <- c(
      n_history_delay_noro_orig, n_history_delay_noro_orig,
      floor(n_history_delay_noro_orig / 7)
    )
    filter_ref_dates_noro <- c(FALSE, TRUE, TRUE)
  }
  df_noro <- expand.grid(
    nowcast_dates = noro_nowcast_dates,
    filter_ref_dates = filter_ref_dates_noro,
    n_history_delays = n_history_delays_noro
  )



  config <- list(
    norovirus = list(
      url = norovirus_url,
      max_delay = 14,
      n_history_delay_orig = 42,
      n_history_uncertainty = 10,
      borrow_delay = FALSE,
      borrow_uncertainty = FALSE,
      days_to_eval = 7,
      quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), # Used in Mellor et al
      eval_time_frame = 50,
      # Variables to map over
      nowcast_dates = df_noro |> pull(nowcast_dates) |> as.vector(),
      n_history_delays = df_noro |> pull(n_history_delays) |> as.vector(),
      filter_ref_dates = df_noro |> pull(filter_ref_dates) |> as.vector()
    ),
    covid = list(
      url = covid_url,
      nowcast_dates = covid_nowcast_dates,
      max_delay = 40,
      days_to_eval = 29, # 0 to - 28 horizon)
      quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
      eval_timeframe = 80,
      base_scenario = list(
        max_delay = 40,
        n_history_delay = 60,
        n_history_dispersion = 20,
        borrow_delay = FALSE,
        borrow_uncertainty = FALSE
      )
    ),
    n_draws = 100
  )

  yaml::write_yaml(config,
    file = file.path("input", "config", "config.yaml")
  )
  return(NULL)
}
