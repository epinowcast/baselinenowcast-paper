write_config <- function(noro_nowcast_dates = NULL,
                         measles_nowcast_dates = NULL) {
  norovirus_url <- "https://raw.githubusercontent.com/jonathonmellor/norovirus-nowcast/refs/heads/main/outputs/data/cases_with_noise.csv" # nolint
  if (is.null(noro_nowcast_dates)) {
    noro_nowcast_dates <- seq(
      from = ymd("2013-05-05"),
      to = ymd("2014-02-25"),
      by = "day"
    )
  }

  measles_url <- "https://raw.githubusercontent.com/kassteele/Nowcasting/refs/heads/master/data/measles_NL_2013_2014.dat" # nolint
  if (is.null(measles_nowcast_dates)) {
    measles_nowcast_dates <- seq(
      from = ymd("2023-11-05"),
      to = ymd("2024-03-10"),
      by = "week"
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
      days_to_eval = 7
    ),
    measles = list(
      url = measles_url,
      nowcast_dates = measles_nowcast_dates,
      max_delay = 50,
      n_history_delay = 52,
      n_history_dispersion = 50,
      borrow_delay = FALSE,
      borrow_uncertainty = FALSE
    ),
    n_draws = 100,
    Hub_quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
    eval_timeframe = 80
  )

  yaml::write_yaml(config,
    file = file.path("input", "config", "config.yaml")
  )
  return(NULL)
}
