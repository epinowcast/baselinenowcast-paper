#' Run noro nowcast pipeline
#'
#' @param noro_df  Dataframe of the latest data by reference and
#'    report date
#' @param max_delay Integer indicating the maximum delay.
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
#' @param filter_ref_dates Boolean indicating whether or not to make separate
#'    delay estimates by weekday.
#' @param quantiles Vector of quantiles to generate.
#' @autoglobal
#' @importFrom dplyr filter mutate left_join select rename pull row_number
#'    arrange
#' @importFrom zoo rollsum rollapply
#' @importFrom tibble tibble
#' @importFrom baselinenowcast get_nowcast_draws get_delay_estimate apply_delay
#' @importFrom lubridate ymd days
#' @importFrom scoringutils as_forecast_sample transform_forecasts log_shift
#'    as_forecast_quantile
#' @returns Data.frame of nowcast draws with training data and metadata.
run_noro_nowcast_pipeline <- function(
    noro_df,
    nowcast_date,
    max_delay,
    filter_ref_dates,
    n_history_delay,
    n_history_uncertainty,
    eval_timeframe,
    days_to_eval,
    n_draws,
    quantiles) {
  if (isTRUE(filter_ref_dates)) {
    weekdays <- 1:7

    samples_nowcast <- data.frame()
    for (i in seq_along(weekdays)) {
      noro_df_one_weekday <- noro_df[wday(
        noro_long$reference_date
      ) == weekdays[i]]

      sample_nowcast_one_wday <- get_noro_nowcast(
        long_df = noro_df_one_weekday,
        nowcast_date = nowcast_date,
        max_delay = max_delay,
        n_draws = n_draws,
        n_history_delay = n_history_delay,
        n_history_uncertainty = n_history_uncertainty
      )
      # Bind together the nowcasts from each weekday
      samples_nowcast <- bind_rows(
        samples_nowcast,
        samples_nowcast_one_wday
      )
    }
  } else {
    samples_nowcast <- get_noro_nowcast(
      long_df = noro_df,
      nowcast_date = nowcast_date,
      max_delay = max_delay,
      n_draws = n_draws,
      n_history_delay = n_history_delay,
      n_history_uncertainty = n_history_uncertainty
    )
  }


  eval_data <- get_eval_data_from_long_df(
    long_df = noro_df,
    as_of_date = ymd(nowcast_date) + days(eval_timeframe)
  )

  data_as_of_df <- noro_df |>
    filter(report_date <= nowcast_date) |>
    group_by(reference_date) |>
    summarise(
      data_as_of = sum(count, na.rm = TRUE)
    )
  n_history_training_volume <- n_history_delay + n_history_uncertainty
  comb_nc_noro <- samples_nowcast |>
    select(reference_date, draw, pred_count, nowcast_date) |>
    mutate(
      total_count = pred_count,
      model = case_when(
        isTRUE(filter_ref_dates) & n_history_training_volume > 8 ~ "filter weekday large training volume",
        isTRUE(filter_ref_dates) & n_history_training_volume <= 8 ~ "filter weekday",
        TRUE ~ "base"
      ),
      # These will all vary
      n_history_delay = n_history_delay,
      n_history_uncertainty = n_history_uncertainty
    ) |>
    filter(reference_date >=
      ymd(nowcast_date) - days(days_to_eval - 1)) |>
    left_join(eval_data, by = "reference_date")

  return(comb_nc_noro)
}

#' Get norovirus nowcast
#' @inheritParams run_noro_nowcast_pipeline
#' @param long_df Dataframe of the latest data by reference and
#'    report date, may or may not contain days for every referene date
#' @returns Data.frame of nowcast draws joined to training data.
get_noro_nowcast <- function(
    long_df,
    nowcast_date,
    max_delay,
    n_draws,
    n_history_delay,
    n_history_uncertainty) {
  triangle <- get_rep_tri_from_long_df(
    long_df = long_df,
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

  point_nowcast_mat <- apply_delay(
    rep_tri_to_nowcast = triangle,
    delay_pmf = delay_pmf
  )

  disp_params <- estimate_uncertainty(
    triangle_for_uncertainty = triangle,
    n_history_uncertainty = n_history_uncertainty,
    n_history_delay = n_history_delay
  )

  nowcast_draws_df <- get_nowcast_draws(
    point_nowcast_matrix = point_nowcast_mat,
    reporting_triangle = triangle,
    dispersion = disp_params,
    draws = n_draws
  )

  reference_dates <- long_df |>
    filter(
      reference_date <= nowcast_date
    ) |>
    distinct(reference_date) |>
    arrange(reference_date) |>
    pull()

  date_df <- tibble(reference_date = reference_dates) |>
    mutate(
      time = row_number()
    )

  data_as_of_df <- long_df |>
    filter(
      report_date <= nowcast_date
    ) |>
    group_by(reference_date) |>
    summarise(
      data_as_of = sum(count, na.rm = TRUE)
    ) |>
    ungroup()

  samples_w_metadata <- nowcast_draws_df |>
    left_join(date_df, by = "time") |>
    select(reference_date, draw, pred_count) |>
    mutate(nowcast_date = nowcast_date) |>
    left_join(data_as_of_df, by = "reference_date")

  return(samples_w_metadata)
}
