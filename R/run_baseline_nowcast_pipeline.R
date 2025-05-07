#' Run `baselinenowcast` pipeline with options for combining
#'
#' This function ingests a long form dataframe containing cases by reference
#'    date and report date, the data we want to nowcast from, and the
#'    `baselinenowcast` model specifications and returns a probabilistic
#'    nowcast of the expected total cases by reference date up until the
#'    specified nowcast date. Note this is not being used.
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
#' @param filter_ref_date_by_wday Boolean indicating whether or not to run the
#'    pipeline using only training data from each weekday. Default is `FALSE`.
#' @param n_draws Integer indicating the number of draws from the observation
#'    model to use to generate the expected observed nowcasts. Default is
#'    `1000`.
#' @param long_df_for_borrow Data.frame containing the same columns as
#'    `long_df` but from the strata to borrow delay or uncertainty from. Default
#'    is `NULL` corresponding to no borrowing base case.
#' @param borrow_delay Boolean indicating whether to use the
#'    `long_df_for_borrow` to estimate the delay distribution, default is
#'    `FALSE`.
#' @param borrow_uncertainty Boolean indicating whether to use the
#'    `long_df_for_borrow` to estimate the dispersion parameters, default is
#'    `FALSE`.
#' @autoglobal
#' @importFrom dplyr select distinct filter mutate arrange left_join row_number
#'    bind_rows pull
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate wday
#' @returns `summary_nowcast` A dataframe of the expected observed total counts
#'    for each reference date up until the nowcast date
run_baselinenowcast_pipeline <- function(long_df,
                                         nowcast_date,
                                         max_delay,
                                         n_history_delay,
                                         n_history_uncertainty,
                                         filter_ref_date_by_wday = FALSE,
                                         n_draws = 1000,
                                         long_df_for_borrow = NULL,
                                         borrow_delay = FALSE,
                                         borrow_uncertainty = FALSE) { # nolint
  summary_nowcast <- tibble()
  if (isTRUE(filter_ref_date_by_wday)) {
    for (i in 1:7) {
      long_df_filtered <- long_df[wday(long_df$reference_date == i)]
      nowcast_df_wday <- baselinenowcast_pipeline(
        long_df = long_df_filtered,
        nowcast_date = nowcast_date,
        max_delay = max_delay,
        n_history_delay = n_history_delay,
        n_history_uncertainty = n_history_uncertainty,
        n_draws = n_draws,
        long_df_for_borrow = long_df_for_borrow,
        borrow_delay = borrow_delay,
        borrow_uncertainty = borrow_uncertainty
      )
      summary_nowcast <- bind_rows(summary_nowcast, nowcast_df_wday) |>
        arrange(nowcast_date) |>
        mutate(
          model = "wday_filtered",
          n_history_delay = n_history_delay,
          n_history_uncertainty = n_history_uncertainty,
          borrow_delay = borrow_delay,
          borrow_uncertainty = borrow_uncertainty
        )
    }
  } else {
    summary_nowcast <- baselinenowcast_pipeline(
      long_df = long_df,
      nowcast_date = nowcast_date,
      max_delay = max_delay,
      n_history_delay = n_history_delay,
      n_history_uncertainty = n_history_uncertainty,
      n_draws = n_draws,
      long_df_for_borrow = long_df_for_borrow,
      borrow_delay = borrow_delay,
      borrow_uncertainty = borrow_uncertainty
    ) |>
      arrange(nowcast_date) |>
      mutate(
        model = "base",
        n_history_delay = n_history_delay,
        n_history_uncertainty = n_history_uncertainty,
        borrow_delay = borrow_delay,
        borrow_uncertainty = borrow_uncertainty
      )
  }

  return(summary_nowcast)
}


#' Run `baselinenowcast` pipeline to generate an individual instance of a
#'    probabilistic nowcast
#'
#' This function ingests a long form dataframe containing cases by reference
#'    date and report date, the data we want to nowcast from, and the
#'    `baselinenowcast` model specifications and returns a probabilistic
#'    nowcast of the expected total cases by reference date up until the
#'    specified nowcast date. Note this is not being used.
#'
#' @param long_df Data.frame containing the following columns:
#'    `reference_date`, `report_date`, and `count` indicating the cases on
#'    each reference and report date. This might be filtered such that only
#'    reference dates from one day of the week are included
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
#' @param long_df_for_borrow Data.frame containing the same columns as
#'    `long_df` but from the strata to borrow delay or uncertainty from. Default
#'    is `NULL` corresponding to no borrowing base case.
#' @param borrow_delay Boolean indicating whether to use the
#'    `long_df_for_borrow` to estimate the delay distribution, default is
#'    `FALSE`.
#' @param borrow_uncertainty Boolean indicating whether to use the
#'    `long_df_for_borrow` to estimate the dispersion parameters, default is
#'    `FALSE`.
#'
#' @autoglobal
#' @importFrom baselinenowcast add_uncertainty
#'    aggregate_df_by_ref_time generate_pt_nowcast_mat_list
#'    generate_triangles get_delay_estimate estimate_dispersion
#'    truncate_triangles apply_delay nowcast_matrix_list_to_df
#' @importFrom dplyr select distinct filter mutate arrange left_join row_number
#'    bind_rows pull
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @returns `ind_nowcast` A dataframe of the expected observed total counts
#'    for each reference date up until the nowcast date
baselinenowcast_pipeline <- function(long_df,
                                     nowcast_date,
                                     max_delay,
                                     n_history_delay,
                                     n_history_uncertainty,
                                     n_draws = 1000,
                                     long_df_for_borrow = NULL,
                                     borrow_delay = FALSE,
                                     borrow_uncertainty = FALSE) { # nolint
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
  if (isTRUE(borrow_delay)) {
    triangle_for_delay <- get_rep_tri_from_long_df(
      long_df = long_df_for_borrow,
      nowcast_date = nowcast_date,
      max_delay = max_delay
    ) |>
      select(-`reference_date`, -`nowcast_date`) |>
      as.matrix()
  } else {
    triangle_for_delay <- triangle
  }

  delay_pmf <- get_delay_estimate(
    reporting_triangle = triangle_for_delay,
    max_delay = max_delay,
    n = n_history_delay
  )

  # Get point estimate
  point_nowcast_mat <- apply_delay(
    rep_tri_to_nowcast = triangle,
    delay_pmf = delay_pmf
  )

  # Estimate uncertainty
  if (isTRUE(borrow_uncertainty)) {
    triangle_for_uncertainty <- get_rep_tri_from_long_df(
      long_df = long_df_for_borrow,
      nowcast_date = nowcast_date,
      max_delay = max_delay
    ) |>
      select(-`reference_date`, -`nowcast_date`) |>
      as.matrix()
  } else {
    triangle_for_uncertainty <- triangle
  }
  truncated_rts <- truncate_triangles(
    reporting_triangle = triangle_for_uncertainty,
    n = n_history_uncertainty
  )
  # Get retrospective triangles
  retro_rts <- generate_triangles(trunc_rep_mat_list = truncated_rts)
  # Get retro nowcasts
  retro_nowcasts <- generate_pt_nowcast_mat_list(
    reporting_triangle_list = retro_rts
  )
  disp_params <- estimate_dispersion(
    pt_nowcast_mat_list = retro_nowcasts,
    trunc_rep_mat_list = truncated_rts
  )

  exp_obs_nowcasts <- add_uncertainty(
    point_nowcast_matrix = point_nowcast_mat,
    disp = disp_params,
    n_draws = n_draws
  )
  nowcast_draws <- nowcast_matrix_list_to_df(
    nowcast_matrix_list = exp_obs_nowcasts
  )
  ind_nowcast <- aggregate_df_by_ref_time(nowcast_draws)
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
  ind_nowcast <- ind_nowcast |>
    left_join(date_df, by = "time") |>
    select(reference_date, draw, total_count) |>
    mutate(nowcast_date = nowcast_date) |>
    left_join(data_as_of_df, by = "reference_date")

  return(ind_nowcast)
}
