#' Get nowcast quantiles from point nowcast, dispersion, and other metadata
#'
#' @param point_nowcast_matrix Matrix containing the point estimate of the
#'    nowcast elements with both observations and predictions.
#' @param reporting_triangle Matrix original reporting triangle
#'    (observations only).
#' @param dispersion Vector of dispersion parameters.
#' @param draws Integer indicating number of draws to sample.
#' @param nowcast_date Character string in YYYY-MM-DD format indicating the
#'    date of the nowcast.
#' @param date_df Dataframe of reference dates and time indices
#' @param data_as_of_df Dataframe containing the total counts across delays
#'    for each reference time.
#' @param eval_data Dataframe containing the final counts to evaluate against
#'    (should be transformed to the target already).
#' @param days_to_eval Integer indicating the number of days before the nowcast
#'    date to evaluate against.
#' @param model Character string indicatoing which model is being used
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
#' @param fun_to_aggregate Function to apply across the `k` reference dates,
#'    default is `sum`.
#' @param k Width of reference dates to apply transform to, default is `1`
#' @autoglobal
#' @importFrom dplyr filter mutate left_join select rename
#' @importFrom baselinenowcast get_nowcast_draws
#' @importFrom lubridate ymd days
#' @importFrom scoringutils as_forecast_sample transform_forecasts log_shift
#'    as_forecast_quantile
#' @returns scoringutils forecast quantile object
get_nowcast_quantiles <- function(point_nowcast_matrix,
                                  reporting_triangle,
                                  dispersion,
                                  draws,
                                  nowcast_date,
                                  date_df,
                                  data_as_of_df,
                                  eval_data,
                                  days_to_eval,
                                  model,
                                  n_history_delay,
                                  n_history_uncertainty,
                                  borrow,
                                  partial_rep_tri,
                                  age_group,
                                  fun_to_aggregate = sum,
                                  k = 1) {
  nowcast_draws_df <- get_nowcast_draws(
    point_nowcast_matrix = point_nowcast_matrix,
    reporting_triangle = reporting_triangle,
    dispersion = dispersion,
    draws = draws,
    fun_to_aggregate = fun_to_aggregate,
    k = k
  )
  samples_w_metadata <- nowcast_draws_df |>
    left_join(date_df, by = "time") |>
    select(reference_date, draw, pred_count) |>
    mutate(nowcast_date = nowcast_date) |>
    left_join(data_as_of_df, by = "reference_date") |>
    filter(reference_date >= ymd(nowcast_date) - days(days_to_eval - 1)) |>
    left_join(eval_data, by = "reference_date") |>
    rename(total_count = pred_count) |>
    mutate(
      model = "base", # Here this is the only model we are using
      # These will all vary
      n_history_delay = n_history_delay,
      n_history_uncertainty = n_history_uncertainty,
      borrow = borrow,
      partial_rep_tri = partial_rep_tri,
      age_group = age_group
    ) |>
    filter(reference_date >= min(reference_date) + days((k - 1))) # exclude NAs


  su_quantiles <- scoringutils::as_forecast_sample(samples_w_metadata,
    # All the metadata we will want to keep track of
    forecast_unit = c(
      "nowcast_date",
      "reference_date",
      "age_group",
      "model",
      "n_history_delay",
      "n_history_uncertainty",
      "borrow",
      "partial_rep_tri"
    ),
    observed = "observed",
    predicted = "total_count",
    sample_id = "draw"
  ) |>
    scoringutils::transform_forecasts(
      fun = scoringutils::log_shift,
      offset = 1
    ) |>
    scoringutils::as_forecast_quantile(
      probs = config$covid$quantiles
    )

  return(su_quantiles)
}
