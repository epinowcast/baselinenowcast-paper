#' Get norovirus delay outputs
#'
#' @inheritParams run_noro_nowcast_pipeline
#' @importFrom cli cli_abort
#' @returns Data.frame of delays with metadata
get_noro_delay_outputs <- function(noro_df,
                                   nowcast_date,
                                   max_delay,
                                   filter_ref_dates,
                                   n_history_delay) {
  if (isTRUE(filter_ref_dates)) {
    weekday_nums <- 1:7
    delay_dfs <- data.frame()
    for (i in seq_along(weekday_nums)) {
      noro_df_one_weekday <- noro_df[wday(
        noro_df$reference_date
      ) == weekday_nums[i], ]

      triangle <- get_rep_tri_from_long_df(
        long_df = noro_df_one_weekday,
        nowcast_date = nowcast_date,
        max_delay = max_delay
      ) |>
        select(-reference_date, -nowcast_date) |>
        as.matrix()
      if (nrow(triangle) == 0) {
        warning(
          "No data available for weekday ",
          weekday_nums[i],
          " on ", nowcast_date,
          call. = FALSE
        )
        next
      }
      # Here we estimate the delay pmf using the last 3 rows,
      # since each row has 1 and then 7, 7, days of data
      # and we want the latest delay
      triangle_trunc <- triangle[(nrow(triangle) - 2):nrow(triangle), ]
      delay_pmf <- colSums(triangle_trunc, na.rm = TRUE) / sum(triangle_trunc,
        na.rm = TRUE
      )
      delay_df <- data.frame(
        delay = delay_pmf,
        delay_time = 0:(length(delay_pmf) - 1)
      ) |>
        mutate(
          nowcast_date = nowcast_date,
          weekday = weekday_nums[i],
          weekday_name = as.character(wday(weekday_nums[i], label = TRUE)),
          n_history_delay = n_history_delay,
          filter_ref_dates = filter_ref_dates
        )

      delay_dfs <- bind_rows(
        delay_dfs,
        delay_df
      )
    }
  } else {
    triangle <- get_rep_tri_from_long_df(
      long_df = noro_df,
      nowcast_date = nowcast_date,
      max_delay = max_delay
    ) |>
      select(-reference_date, -nowcast_date) |>
      as.matrix()

    delay_pmf <- get_delay_estimate(
      reporting_triangle = triangle,
      max_delay = max_delay,
      n = max_delay + 1
    )

    delay_dfs <- data.frame(
      delay = delay_pmf,
      delay_time = (0:(length(delay_pmf) - 1))
    ) |>
      mutate(
        nowcast_date = nowcast_date,
        weekday = NA,
        weekday_name = "All",
        n_history_delay = n_history_delay,
        filter_ref_dates = filter_ref_dates
      )
  }
  # Order weekday names so that horizons are in order from -7 to 0
  delay_dfs$weekday_name <- factor(delay_dfs$weekday_name,
    levels = c(
      "Mon",
      "Tue",
      "Wed",
      "Thu",
      "Fri",
      "Sat",
      "Sun"
    )
  )

  return(delay_dfs)
}
