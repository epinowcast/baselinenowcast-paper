source(file.path("src", "write_config.R"))

write_config(
  noro_nowcast_dates = c("2023-12-10", "2024-01-21", "2024-02-25"),
  measles_nowcast_dates = c("2013-07-01", "2013-10-01", "2014-02-25")
)
