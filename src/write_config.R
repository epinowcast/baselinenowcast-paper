write_config <- function(noro_nowcast_dates = NULL,
                         covid_nowcast_dates = NULL) {
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


  config <- list(
    norovirus = list(
      url = norovirus_url,
      nowcast_dates = noro_nowcast_dates,
      max_delay = 14,
      n_history_delay = 42,
      n_history_uncertainty = 10,
      borrow_delay = FALSE,
      borrow_uncertainty = FALSE,
      days_to_eval = 7,
      quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), # Used in Mellor et al
      eval_time_frame = 50
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
