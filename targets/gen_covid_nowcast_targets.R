gen_covid_nowcast_targets <- list(
  tar_target(
    name = covid_long,
    command = covid_long_all_strata |>
      filter(age_group == age_group_to_nowcast)
  ),
  tar_target(
    name = long_df_for_borrow,
    command = {
      if (isTRUE(borrow_delay)) {
        covid_long |> filter(age_group == "00+")
      } else {
        NULL
      }
    }
  ),
  # Run each step of the baselinenowcast pipeline individually
  # 1. Generate reporting triangle
  tar_target(
    name = triangle,
    command = get_rep_tri_from_long_df(
      long_df = covid_long,
      nowcast_date = nowcast_dates_covid,
      max_delay = config$covid$max_delay
    ) |> select(
      -reference_date, -nowcast_date
    ) |> as.matrix()
  ),
  # Get triangle for delay
  tar_target(
    name = triangle_for_delay,
    command = {
      if (borrow_delay) {
        get_rep_tri_from_long_df(
          long_df = long_df_for_borrow,
          nowcast_date = nowcast_dates_covid,
          max_delay = config$covid$max_delay
        ) |>
          select(
            -reference_date, -nowcast_date
          ) |>
          as.matrix()
      } else {
        triangle
      }
    }
  ),
  # Estimate delay
  tar_target(
    name = delay_pmf,
    command = get_delay_estimate(
      reporting_triangle = triangle_for_delay,
      max_delay = config$covid$max_delay,
      n = n_history_delay
    )
  ),
  # Get point nowcast matrix
  tar_target(
    name = point_nowcast_mat,
    command = apply_delay(
      rep_tri_to_nowcast = triangle,
      delay_pmf = delay_pmf
    )
  ),
  # Estimate uncertainty
  tar_target(
    name = triangle_for_uncertainty,
    command = {
      if (borrow_uncertainty) {
        get_rep_tri_from_long_df(
          long_df = long_df_for_borrow,
          nowcast_date = nowcast_dates_covid,
          max_delay = config$covid$max_delay
        ) |>
          select(
            -reference_date, -nowcast_date
          ) |>
          as.matrix()
      } else {
        triangle
      }
    }
  ),
  tar_target(
    name = truncated_rts,
    command = truncate_triangles(
      reporting_triangle = triangle_for_uncertainty,
      n = n_history_uncertainty
    ),
    format = "rds"
  ),
  tar_target(
    name = retro_rts,
    command = generate_triangles(
      trunc_rep_mat_list = truncated_rts
    ),
    format = "rds"
  ),
  tar_target(
    name = retro_nowcasts,
    command = generate_pt_nowcast_mat_list(
      reporting_triangle_list = retro_rts
    ),
    format = "rds"
  ),
  tar_target(
    name = disp_params,
    command = estimate_dispersion(
      pt_nowcast_mat_list = retro_nowcasts,
      trunc_rep_mat_list = truncated_rts
    )
  ),
  tar_target(
    name = exp_obs_nowcasts,
    command = add_uncertainty(
      point_nowcast_matrix = point_nowcast_mat,
      disp = disp_params,
      n_draws = config$n_draws
    ),
    format = "rds"
  ),
  tar_target(
    name = nowcast_draws_df,
    command = nowcast_matrix_list_to_df(
      nowcast_matrix_list = exp_obs_nowcasts
    )
  ),
  tar_target(
    name = ind_nowcast,
    command = aggregate_df_by_ref_time(nowcast_draws_df)
  ),
  tar_target(
    name = reference_dates,
    command = covid_long |>
      filter(reference_date <= nowcast_dates_covid) |>
      distinct(reference_date) |>
      arrange(reference_date) |>
      pull()
  ),
  tar_target(
    name = date_df,
    command = tibble(reference_date = reference_dates) |>
      mutate(
        time = row_number()
      )
  ),
  tar_target(
    name = data_as_of_df,
    command = covid_long |>
      filter(report_date <= nowcast_dates_covid) |>
      group_by(reference_date) |>
      summarise(
        data_as_of = sum(count, na.rm = TRUE)
      )
  ),
  tar_target(
    name = samples_nowcast_covid_daily,
    command = ind_nowcast |>
      left_join(date_df, by = "time") |>
      select(reference_date, draw, total_count) |>
      mutate(nowcast_date = nowcast_dates_covid) |>
      left_join(data_as_of_df, by = "reference_date")
  ),

  # Make nowcasts into 7 day incidence
  tar_target(
    name = samples_nowcast_covid_7d,
    command = samples_nowcast_covid_daily |>
      group_by(draw) |>
      arrange(reference_date) |>
      mutate(
        total_count = rollapply(total_count,
          width = 7,
          FUN = sum,
          fill = NA, align = "right"
        ),
        data_as_of = rollapply(data_as_of,
          width = 7,
          FUN = sum,
          fill = NA, align = "right"
        ),
      ) |>
      filter(reference_date >= min(reference_date) + days(6)) # exclude NA days
  ),

  # Get evaluation data to join
  tar_target(
    name = eval_data_daily,
    command = get_eval_data_from_long_df(
      long_df = covid_long,
      as_of_date = ymd(nowcast_dates_covid) + days(config$covid$eval_timeframe)
    )
  ),
  tar_target(
    name = eval_data_7d,
    command = eval_data_daily |>
      arrange(reference_date) |>
      mutate(observed = rollapply(observed,
        width = 7,
        FUN = sum,
        fill = NA, align = "right"
      )) |>
      filter(reference_date >= min(reference_date) + days(6)) # exclude NA days
  ),
  # Get as of data we want to join
  tar_target(
    name = data_as_of_daily,
    command = covid_long |>
      filter(report_date <= nowcast_dates_covid) |>
      group_by(reference_date) |>
      summarise(
        data_as_of = sum(count, na.rm = TRUE)
      )
  ),
  # Get as of data we want to join
  tar_target(
    name = data_as_of_7d,
    command = data_as_of_daily |>
      arrange(reference_date) |>
      mutate(data_as_of = rollapply(data_as_of,
        width = 7,
        FUN = sum,
        fill = NA, align = "right"
      )) |>
      filter(reference_date >= min(reference_date) + days(6)) # exclude NA days
  ),
  # Join eval data to the subset of the nowcast that we are evaluating
  tar_target(
    name = comb_nc_covid,
    command = samples_nowcast_covid_7d |>
      filter(reference_date >=
        ymd(nowcast_dates_covid) - days(config$covid$days_to_eval - 1)) |>
      left_join(eval_data_7d, by = "reference_date") |>
      mutate(age_group = age_group_to_nowcast)
  ),
  # Forecast objects ----------------------------------------------------------
  tar_target(
    name = su_sample_covid,
    command = scoringutils::as_forecast_sample(
      data = comb_nc_covid,
      # All the metadata we will want to keep track of
      forecast_unit = c(
        "nowcast_date",
        "reference_date",
        "age_group",
        "model",
        "n_history_delay",
        "n_history_uncertainty",
        "borrow_delay",
        "borrow_uncertainty"
      ),
      observed = "observed",
      predicted = "total_count",
      sample_id = "draw"
    )
  ),
  # Forecast quantiles as a scoringutils forecast object
  tar_target(
    name = su_quantile_covid,
    command = scoringutils::as_forecast_quantile(
      data = su_sample_covid,
      probs = config$covid$quantiles
    )
  ),
  # Get a wide dataframe with only 50th and 90th for plotting
  tar_target(
    name = summary_nowcast_covid,
    command = su_quantile_covid |>
      filter(quantile_level %in% config$plot_quantiles) |>
      pivot_wider(
        names_from = "quantile_level",
        values_from = "predicted",
        names_prefix = "q_"
      ) |> left_join(data_as_of_7d, by = "reference_date")
  ),
  # Scores--------------------------------------------------------------------
  tar_target(
    name = scores_sample_covid,
    command = scoringutils::score(su_sample_covid)
  ),
  tar_target(
    name = scores_quantile_covid,
    command = scoringutils::score(su_quantile_covid)
  ),
  tar_target(
    name = coverage_covid,
    command = scoringutils::get_coverage(
      su_quantile_covid |>
        mutate(model = "baselinenowcast"),
      by = c(
        "nowcast_date",
        "reference_date",
        "age_group",
        "model",
        "n_history_delay",
        "n_history_uncertainty",
        "borrow_delay",
        "borrow_uncertainty"
      )
    )
  )
)
