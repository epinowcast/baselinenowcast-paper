EDA_plot_targets <- list(
  tar_target(
    name = plot_measles_data,
    command = get_plot_data_as_of(
      final_df = measles_long,
      as_of_dates = c(
        "2013-07-01", "2013-10-01",
        "2014-02-25"
      ),
      pathogen = "Measles"
    ),
    format = "rds"
  ),
  tar_target(
    name = plot_noro_data,
    command = get_plot_data_as_of(
      final_df = noro_long,
      as_of_dates = c(
        "2023-12-10", "2024-01-21",
        "2024-02-25"
      ),
      pathogen = "Norovirus"
    ),
    format = "rds"
  )
)
