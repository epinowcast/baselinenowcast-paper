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
#' @param model Character string indicating which model is being used
#' @param n_history_delay Integer indicating number of reference times used for
#'    delay estimate.
#' @param n_history_uncertainty Integer indicating number of retrospective
#'    nowcast datasets to use for uncertainty estimation
#' @param borrow Boolean indicating whether or not the delay and dispersion
#'    is borrowed from another strata.
#' @param partial_rep_tri Boolean indicating whether or not a partially
#'    complete reporting triangle is used or the latest complete triangle is
#'    used.
#' @param age_group Character string indicating the age group.
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
#' @importFrom baselinenowcast get_nowcast_draws get_delay_estimate apply_delay
#' @importFrom lubridate ymd days
#' @importFrom scoringutils as_forecast_sample transform_forecasts log_shift
#'    as_forecast_quantile
#' @returns scoringutils forecast quantile object

run_covid_nowcast_pipeline <- function(
    long_df_all_strata,
    nowcast_date,
    max_delay,
    age_group_to_nowcast,
    borrow,
    partial_rep_tri,
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

  delay_pmf <- get_delay_estimate(
    reporting_triangle = triangle_for_delay,
    max_delay = max_delay,
    n = n_history_delay
  )

  point_nowcast_mat <- apply_delay(
    rep_tri_to_nowcast = triangle,
    delay_pmf = delay_pmf
  )

  disp_params <- estimate_uncertainty(
    triangle_for_uncertainty = triangle_for_delay,
    n_history_uncertainty = n_history_uncertainty,
    n_history_delay = n_history_delay,
    fun_to_aggregate = sum,
    k = k
  )
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

  delay_df <- data.frame(
    delay = delay_pmf,
    delay_time = 0:(length(delay_pmf) - 1)
  ) |>
    mutate(
      model = "base",
      nowcast_date = nowcast_date,
      age_group = age_group_to_nowcast,
      n_history_delay = n_history_delay,
      n_history_uncertainty = n_history_uncertainty,
      borrow = borrow,
      partial_rep_tri = partial_rep_tri
    )
  mean_delay_df <- data.frame(
    mean_delay =
      sum(delay_pmf * (0:(length(delay_pmf) - 1)))
  ) |>
    mutate(
      model = "base",
      nowcast_date = nowcast_date,
      age_group = age_group_to_nowcast,
      n_history_delay = n_history_delay,
      n_history_uncertainty = n_history_uncertainty,
      borrow = borrow,
      partial_rep_tri = partial_rep_tri
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
