#' Get norovirus delay outputs
#'
#' @inheritParams run_noro_nowcast_pipeline
#'
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

      delay_pmf <- get_delay_estimate(
        reporting_triangle = triangle,
        max_delay = max_delay,
        n = min(nrow(triangle), n_history_delay)
      )
      reference_dates <- noro_df_one_weekday |>
        filter(
          reference_date <= nowcast_date
        ) |>
        distinct(reference_date) |>
        arrange(reference_date) |>
        pull()

      date_df <- tibble(reference_date = reference_dates) |>
        mutate(
          time = row_number() - 1
        )

      delay_df <- data.frame(
        delay = delay_pmf,
        delay_time = 0:(length(delay_pmf) - 1)
      ) |>
        left_join(
          date_df,
          by = c("delay_time" = "time") # nolint
        ) |>
        mutate(
          nowcast_date = nowcast_date,
          weekday = wday(reference_date),
          weekday_name = wday(reference_date, label = TRUE),
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
      n = n_history_delay
    )
    reference_dates <- noro_df |>
      filter(
        reference_date <= nowcast_date
      ) |>
      distinct(reference_date) |>
      arrange(reference_date) |>
      pull()

    date_df <- tibble(reference_date = reference_dates) |>
      mutate(
        time = row_number() - 1
      )

    delay_dfs <- data.frame(
      delay = delay_pmf,
      delay_time = (0:(length(delay_pmf) - 1))
    ) |>
      left_join(
        date_df,
        by = c("delay_time" = "time") # nolint
      ) |>
      mutate(
        nowcast_date = nowcast_date,
        weekday = wday(reference_date),
        weekday_name = wday(reference_date, label = TRUE),
        n_history_delay = n_history_delay,
        filter_ref_dates = filter_ref_dates
      )
  }

  return(delay_dfs)
}
