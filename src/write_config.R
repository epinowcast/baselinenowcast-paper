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
    norovirus_url = norovirus_url,
    noro_nowcast_dates = noro_nowcast_dates,
    measles_url = measles_url,
    measles_nowcast_dates = measles_nowcast_dates
  )

  yaml::write_yaml(config,
    file = file.path("input", "config", "config.yaml")
  )
  return(NULL)
}
