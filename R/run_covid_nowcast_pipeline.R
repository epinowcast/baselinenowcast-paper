#' Run covid nowcast pipeline
#'
#' @param long_df_all_strata  Dataframe of the latest data by reference and
#'    report date for all age groups
#' @param max_delay Integer indicating the maximum delay.
#' @param age_group_to_nowcast Character string indicating which age group
#'    to nowcast.
#' @param eval_timeframe Integer indicating the number of days after the
#'    nowcast date to evaluate the nowcasts.
#' @param n_draws Integer indicating number of draws to sample.
#' @param nowcast_date Character string in YYYY-MM-DD format indicating the
#'    date of the nowcast.
#' @param days_to_eval Integer indicating the number of days before the nowcast
#'    date to evaluate against.
#' @param n_history_delay Integer indicating number of reference times used for
#'    delay estimate.
#' @param n_history_uncertainty Integer indicating number of retrospective
#'    nowcast datasets to use for uncertainty estimation
#' @param borrow Boolean indicating whether or not the delay and dispersion
#'    is borrowed from another strata.
#' @param partial_rep_tri Boolean indicating whether or not a partially
#'    complete reporting triangle is used or the latest complete triangle is
#'    used.
#' @param weekday_filter Boolean indicating whether or not to estimate delays
#'    and perform nowcasting on individual weekdays and recombine.
#' @param age_group_to_nowcast Character string indicating the age group.
#' @param quantiles Vector of quantiles to generate.
#' @param plot_quantiles Vector of quantiles for plotting.
#' @param fun_to_aggregate Function to apply across the `k` reference dates,
#'    default is `sum`.
#' @param k Width of reference dates to apply transform to, default is `1`
#' @autoglobal
#' @importFrom dplyr filter mutate left_join select rename pull row_number
#'    arrange
#' @importFrom zoo rollsum rollapply
#' @importFrom tibble tibble
#' @importFrom baselinenowcast sample_predictions estimate_delay apply_delay
#' @importFrom lubridate ymd days
#' @importFrom scoringutils as_forecast_sample transform_forecasts log_shift
#'    as_forecast_quantile
#' @importFrom dplyr select filter mutate distinct arrange left_join desc
#' @returns scoringutils forecast quantile object

run_covid_nowcast_pipeline <- function(
    long_df_all_strata,
    nowcast_date,
    max_delay,
    age_group_to_nowcast,
    borrow,
    partial_rep_tri,
    weekday_filter,
    n_history_delay,
    n_history_uncertainty,
    eval_timeframe,
    days_to_eval,
    n_draws,
    quantiles,
    plot_quantiles,
    k = 7,
    fun_to_aggregate = sum) {
  triangle <- get_triangle(
    long_df = long_df_all_strata,
    nowcast_date = nowcast_date,
    max_delay = max_delay,
    age_group = age_group_to_nowcast,
    partial_rep_tri = TRUE
  )
  triangle_for_delay <- get_triangle(
    long_df = long_df_all_strata,
    nowcast_date = nowcast_date,
    max_delay = max_delay,
    age_group = ifelse(borrow, "00+", age_group_to_nowcast),
    partial_rep_tri = partial_rep_tri
  )
  if (isTRUE(weekday_filter)) {
    # Create triangle, triangle for delay,
    # delay_pmf, point nowcast mat, and dispersion by binding together
    # across the weekdays
    weekday_nums <- 1:7
    point_nowcast_mat <- data.frame()
    disp_params <- data.frame()
    for (i in seq_along(weekday_nums)) {
      long_df_all_strata_1wday <- long_df_all_strata[wday(
        long_df_all_strata$reference_date
      ) == weekday_nums[i], ]

      triangle1wday <- get_triangle(
        long_df = long_df_all_strata_1wday,
        nowcast_date = nowcast_date,
        max_delay = max_delay,
        age_group = age_group_to_nowcast,
        partial_rep_tri = TRUE
      )
      triangle_for_delay1wday <- get_triangle(
        long_df = long_df_all_strata_1wday,
        nowcast_date = nowcast_date,
        max_delay = max_delay,
        age_group = ifelse(borrow, "00+", age_group_to_nowcast),
        partial_rep_tri = partial_rep_tri
      )
      delay_pmf1wday <- estimate_delay(
        reporting_triangle = triangle_for_delay1wday,
        max_delay = max_delay,
        n = floor(n_history_delay / 7)
      )
      point_nowcast_mat1wday <- apply_delay(
        reporting_triangle = triangle1wday,
        delay_pmf = delay_pmf1wday
      )
      first_struct_val <- sum(!is.na(triangle1wday[nrow(triangle1wday), ]))
      reps_of_7 <- floor((ncol(triangle1wday) - first_struct_val - 1) / 7)
      this_structure <- c(
        first_struct_val,
        rep(7, reps_of_7)
      )
      disp_params1day <- estimate_uncertainty_wrapper(
        triangle_for_uncertainty = triangle1wday,
        n_history_uncertainty = floor(n_history_uncertainty / 7),
        n_history_delay = floor(n_history_delay / 7),
        structure = this_structure,
        fun_to_aggregate = sum,
        k = 1 # This is where I am a bit confused but I think it should be 1
      )
      reference_dates <- long_df_all_strata_1wday |>
        filter(
          reference_date <= nowcast_date,
          age_group == age_group_to_nowcast
        ) |>
        distinct(reference_date) |>
        arrange(reference_date) |>
        pull()
      date_df <- tibble(reference_date = reference_dates) |>
        mutate(
          time = row_number()
        )

      ptnowcast1_w_date <- data.frame(point_nowcast_mat1wday,
        time = seq_len(nrow(point_nowcast_mat1wday))
      ) |>
        left_join(date_df, by = "time")
      disp_w_date <- data.frame(disp_params = disp_params1day) |>
        mutate(
          reference_date = rev(date_df$reference_date)[seq_along(disp_params1day)] # nolint
        )

      point_nowcast_mat <- bind_rows(
        point_nowcast_mat,
        ptnowcast1_w_date
      )
      disp_params <- bind_rows(disp_params, disp_w_date)
    }
    point_nowcast_mat <- point_nowcast_mat |>
      arrange(reference_date) |>
      select(-time, -reference_date) |>
      as.matrix()
    # Dispersion needs to be in order from most to least recent
    disp_params <- disp_params |>
      arrange(desc(reference_date)) |>
      select(-reference_date) |>
      pull()
  } else {
    delay_pmf <- estimate_delay(
      reporting_triangle = triangle_for_delay,
      max_delay = max_delay,
      n = n_history_delay
    )

    point_nowcast_mat <- apply_delay(
      reporting_triangle = triangle,
      delay_pmf = delay_pmf
    )

    disp_params <- estimate_uncertainty_wrapper(
      triangle_for_uncertainty = triangle_for_delay,
      n_history_uncertainty = n_history_uncertainty,
      n_history_delay = n_history_delay,
      fun_to_aggregate = sum,
      k = k
    )
  }
  # Here begins the part where we do these things with or without the weekday
  # filter. The weekday filter components have already been bound together.
  reference_dates <- long_df_all_strata |>
    filter(
      age_group == !!age_group_to_nowcast,
      reference_date <= nowcast_date
    ) |>
    distinct(reference_date) |>
    arrange(reference_date) |>
    pull()
  date_df <- tibble(reference_date = reference_dates) |>
    mutate(
      time = row_number()
    )
  data_as_of_df <- long_df_all_strata |>
    filter(
      age_group == !!age_group_to_nowcast,
      report_date <= nowcast_date
    ) |>
    group_by(reference_date) |>
    summarise(
      data_as_of = sum(count, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
      data_as_of = rollsum(data_as_of,
        k = k,
        fill = NA, align = "right"
      )
    ) |>
    filter(reference_date >= min(reference_date) + days(k - 1)) # exclude NAs

  eval_data_7d <- get_eval_data_from_long_df(
    long_df = filter(long_df_all_strata, age_group == age_group_to_nowcast),
    max_delay = max_delay,
    as_of_date = ymd(nowcast_date) + days(eval_timeframe)
  ) |>
    arrange(reference_date) |>
    mutate(observed = rollsum(observed,
      k = k,
      fill = NA, align = "right"
    )) |>
    filter(reference_date >= min(reference_date) + days(k - 1))

  su_quantile_covid <- get_nowcast_quantiles(
    point_nowcast_matrix = point_nowcast_mat,
    reporting_triangle = triangle,
    dispersion = disp_params,
    draws = n_draws,
    days_to_eval = days_to_eval,
    nowcast_date = nowcast_date,
    date_df = date_df,
    data_as_of_df = data_as_of_df,
    eval_data = eval_data_7d,
    model = "base", # Here this is the only model we are using
    # These will all vary
    n_history_delay = n_history_delay,
    n_history_uncertainty = n_history_uncertainty,
    borrow = borrow,
    partial_rep_tri = partial_rep_tri,
    weekday_filter = weekday_filter,
    age_group = age_group_to_nowcast,
    quantiles = quantiles,
    fun_to_aggregate = fun_to_aggregate,
    k = k
  )

  summary_nowcast_covid <- su_quantile_covid |>
    filter(quantile_level %in% plot_quantiles) |>
    pivot_wider(
      names_from = "quantile_level",
      values_from = "predicted",
      names_prefix = "q_"
    ) |>
    left_join(data_as_of_df, by = "reference_date")

  pt_nowcast_df <- data.frame(pt_nowcast = rollapply(
    rowSums(point_nowcast_mat),
    k,
    fun_to_aggregate,
    fill = NA,
    align = "right"
  )) |>
    mutate(time = seq_len(nrow(point_nowcast_mat)))

  # Recompute delay_pmf using only the max_delay+1
  delay_pmf_recent <- estimate_delay(
    reporting_triangle = triangle_for_delay,
    max_delay = max_delay,
    n = max_delay + 1
  )


  delay_df <- data.frame(
    delay = delay_pmf_recent,
    delay_time = 0:(length(delay_pmf_recent) - 1)
  ) |>
    mutate(
      model = "base",
      nowcast_date = nowcast_date,
      age_group = age_group_to_nowcast,
      n_history_delay = n_history_delay,
      n_history_uncertainty = n_history_uncertainty,
      weekday_filter = weekday_filter,
      borrow = borrow,
      partial_rep_tri = partial_rep_tri
    )
  mean_delay_df <- data.frame(
    mean_delay =
      sum(delay_pmf_recent * (0:(length(delay_pmf_recent) - 1)))
  ) |>
    mutate(
      model = "base",
      nowcast_date = nowcast_date,
      age_group = age_group_to_nowcast,
      n_history_delay = n_history_delay,
      n_history_uncertainty = n_history_uncertainty,
      borrow = borrow,
      partial_rep_tri = partial_rep_tri,
      weekday_filter = weekday_filter
    )


  pt_nowcast_7d <- pt_nowcast_df |>
    left_join(date_df, by = "time") |>
    select(reference_date, pt_nowcast) |>
    filter(reference_date >=
      ymd(nowcast_date) - days(days_to_eval - 1)) |>
    mutate(
      nowcast_date = nowcast_date,
      age_group = age_group_to_nowcast
    ) |>
    rename(predicted = pt_nowcast) |>
    left_join(data_as_of_df, by = "reference_date") |>
    mutate(
      model = "base", # Here this is the only model we are using
      # These will all vary
      n_history_delay = n_history_delay,
      n_history_uncertainty = n_history_uncertainty,
      borrow = borrow,
      weekday_filter = weekday_filter,
      partial_rep_tri = partial_rep_tri
    ) |>
    left_join(eval_data_7d, by = c("reference_date", "age_group"))

  scores_quantile_covid <- scoringutils::score(su_quantile_covid)

  su_quantile_covid_mod <- mutate(su_quantile_covid,
    model = "baselinenowcast"
  )
  coverage_covid <- scoringutils::get_coverage(
    su_quantile_covid_mod,
    by = c(
      "nowcast_date",
      "reference_date",
      "age_group",
      "model",
      "n_history_delay",
      "n_history_uncertainty",
      "borrow",
      "weekday_filter",
      "partial_rep_tri"
    )
  )



  list_of_outputs <- list(
    summary_nowcast_covid = summary_nowcast_covid,
    scores_quantile_covid = scores_quantile_covid,
    coverage_covid = coverage_covid,
    pt_nowcast_7d = pt_nowcast_7d,
    mean_delay_df = mean_delay_df,
    delay_df = delay_df
  )
  return(list_of_outputs)
}
