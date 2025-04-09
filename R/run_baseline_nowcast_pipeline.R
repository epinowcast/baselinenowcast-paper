#' Run `baselinenowcast` pipeline
#'
#' This function ingests a long form dataframe containing cases by reference
#'    date and report date, the data we want to nowcast from, and the
#'    `baselinenowcast` model specifications and returns a probabilistic
#'    nowcast of the expected total cases by reference date up until the
#'    specified nowcast date.
#'
#' @param long_df Data.frame containing the following columns: `reference_date`,
#'    `report_date`, and `count` indicating the cases on each reference
#'    and report date.
#' @param nowcast_date String indicating the date we want to generate a nowcast.
#' @param max_delay Integer indicating the maximum delay we want to consider in
#'   the `baselinenowcast` model.
#' @param n_history_delay Integer indicating the number of reference time
#'    observations to use to estimate the delay.
#' @param n_history_uncertainty Integer indicating the number of reporting
#'    triangles to use to generate the estimate of the dispersion parameters
#' @param n_draws Integer indicating the number of draws from the observation
#'    model to use to generate the expected observed nowcasts. Default is
#'    `1000`.
#' @param reporting_triangle_to_borrow_delay Matrix to be used to estimate the
#'    delay if not the same as the one being nowcasted from, `long_df`.
#' @param reporting_triangle_to_borrow_uncertainty Matrix to be used to
#'    estimate the dispersion parameters if not the same as the one being
#'    nowcasted from, `long_df`.
#'
#' @autoglobal
#' @importFrom baselinenowcast add_obs_errors_to_nowcast
#'    aggregate_df_by_ref_time generate_point_nowcasts
#'    generate_triangles get_delay_estimate estimate_dispersion
#'    truncate_triangles apply_delay nowcast_list_to_df
#' @importFrom dplyr select distinct filter mutate arrange left_join row_number
#'    bind_rows pull
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @returns `summary_nowcast` A dataframe of the expected observed total counts
#'    for each reference date up until the nowcast date
run_baselinenowcast_pipeline <- function(long_df,
                                         nowcast_date,
                                         max_delay,
                                         n_history_delay,
                                         n_history_uncertainty,
                                         n_draws = 1000,
                                         reporting_triangle_to_borrow_delay = NULL, # nolint
                                         reporting_triangle_to_borrow_uncertainty = NULL) { # nolint

  rep_tri_df <- get_rep_tri_from_long_df(
    long_df = long_df,
    nowcast_date = nowcast_date,
    max_delay = max_delay
  )

  data_as_of_df <- long_df |>
    filter(report_date <= nowcast_date) |>
    group_by(reference_date) |>
    summarise(
      data_as_of = sum(count, na.rm = TRUE)
    )

  # Get the reporting triangle matrix
  triangle <- rep_tri_df |>
    select(-`reference_date`, -`nowcast_date`) |>
    as.matrix()

  # Estimate delay
  if (is.null(reporting_triangle_to_borrow_delay)) {
    triangle_for_delay <- triangle
  } else {
    triangle_for_delay <- reporting_triangle_to_borrow_delay
  }

  delay_pmf <- get_delay_estimate(
    triangle = triangle_for_delay,
    max_delay = max_delay,
    n = n_history_delay
  )

  # Get point estimate
  point_reporting_square <- apply_delay(
    triangle_to_nowcast = triangle,
    delay = delay_pmf
  )

  # Estimate uncertainty
  if (is.null(reporting_triangle_to_borrow_uncertainty)) {
    triangle_for_uncertainty <- triangle
  } else {
    triangle_for_uncertainty <- reporting_triangle_to_borrow_uncertainty
  }
  truncated_rts <- truncate_triangles(triangle_for_uncertainty,
    n = n_history_uncertainty
  )
  # Get retrospective triangles
  retro_rts <- generate_triangles(list_of_trunc_rts = truncated_rts)
  # Get retro nowcasts
  retro_nowcasts <- generate_point_nowcasts(list_of_rts = retro_rts)
  disp_params <- estimate_dispersion(
    list_of_nowcasts = retro_nowcasts,
    list_of_trunc_rts = truncated_rts
  )

  exp_obs_nowcasts <- add_obs_errors_to_nowcast(
    comp_rep_square = point_reporting_square,
    disp = disp_params,
    n_draws = n_draws
  )
  nowcast_draws <- nowcast_list_to_df(
    exp_obs_nowcasts
  )
  summary_nowcast <- aggregate_df_by_ref_time(nowcast_draws)
  # Join data and observed data to this!
  reference_dates <- long_df |>
    filter(reference_date <= nowcast_date) |>
    distinct(reference_date) |>
    arrange(reference_date) |>
    pull()
  date_df <- tibble(reference_date = reference_dates) |>
    mutate(
      time = row_number()
    )
  summary_nowcast <- summary_nowcast |>
    left_join(date_df, by = "time") |>
    select(reference_date, draw, total_count) |>
    mutate(nowcast_date = nowcast_date) |>
    left_join(data_as_of_df, by = "reference_date")

  return(summary_nowcast)
}
