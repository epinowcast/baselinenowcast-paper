gen_covid_nowcast_targets <- list(
  tar_target(
    name = covid_long,
    command = covid_long_all_strata |>
      filter(age_group == age_group_to_nowcast)
  ),
  tar_target(
    name = long_df_for_borrow,
    command = {
      if (isTRUE(borrow)) {
        covid_long_all_strata |> filter(age_group == "00+")
      } else {
        NULL
      }
    }
  ),
  # Run baselinenowcast pipeline-----------------------------------------------
  # Generate reporting triangle
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
  # Get triangle for delay (may or may not be same as reporting triangle)
  tar_target(
    name = triangle_for_delay,
    command = {
      if (borrow) {
        get_rep_tri_from_long_df(
          long_df = long_df_for_borrow,
          nowcast_date = nowcast_dates_covid,
          max_delay = config$covid$max_delay
        ) |>
          select(
            -reference_date, -nowcast_date
          ) |>
          as.matrix()
      } else if (!partial_rep_tri) {
        get_rep_tri_from_long_df(
          long_df = covid_long,
          nowcast_date = nowcast_dates_covid,
          max_delay = config$covid$max_delay
        ) |>
          # Remove all the reference dates with incomplete data
          filter(reference_date <= ymd(nowcast_dates_covid) - days(config$covid$max_delay)) |>
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
  # Get triangle to estimate uncertainty (may or may not be reporting triangle
  # to nowcast)
  tar_target(
    name = triangle_for_uncertainty,
    command = {
      if (borrow) {
        get_rep_tri_from_long_df(
          long_df = long_df_for_borrow,
          nowcast_date = nowcast_dates_covid,
          max_delay = config$covid$max_delay
        ) |>
          select(
            -reference_date, -nowcast_date
          ) |>
          as.matrix()
      } else if (!partial_rep_tri) {
        get_rep_tri_from_long_df(
          long_df = covid_long,
          nowcast_date = nowcast_dates_covid,
          max_delay = config$covid$max_delay
        ) |>
          # Remove all the reference dates with incomplete data
          filter(reference_date <= ymd(nowcast_dates_covid) - days(config$covid$max_delay)) |>
          select(
            -reference_date, -nowcast_date
          ) |>
          as.matrix()
      } else {
        triangle
      }
    }
  ),
  # Get a list of truncated reporting triangles
  tar_target(
    name = truncated_rts,
    command = truncate_triangles(
      reporting_triangle = triangle_for_uncertainty,
      n = n_history_uncertainty
    ),
    format = "rds"
  ),
  # Generate retrospective reporting triangles (what would have been available
  # as of the last reference time)
  tar_target(
    name = retro_rts,
    command = generate_triangles(
      trunc_rep_tri_list = truncated_rts
    ),
    format = "rds"
  ),
  # Generate retrospective nowcasts
  tar_target(
    name = retro_nowcasts,
    command = generate_pt_nowcast_mat_list(
      reporting_triangle_list = retro_rts
    ),
    format = "rds"
  ),
  # Use retrospective nowcasts and the observations to estimate dispersion on
  # 7 day rolling sum
  tar_target(
    name = disp_params,
    command = estimate_dispersion(
      pt_nowcast_mat_list = retro_nowcasts,
      trunc_rep_tri_list = truncated_rts,
      reporting_triangle_list = retro_rts,
      fun_to_aggregate = sum,
      k = 7
    )
  ),
  # Make metadata dataframes --------------------------------------------------
  # Join predictions and observations
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
      ) |> ungroup() |>
      mutate(
        data_as_of = rollsum(data_as_of,
          k = 7,
          fill = NA, align = "right"
        )
      ) |>
      filter(reference_date >= min(reference_date) + days(6)) # exclude NA days
  ),
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
      mutate(observed = rollsum(observed,
        k = 7,
        fill = NA, align = "right"
      )) |>
      filter(reference_date >= min(reference_date) + days(6)) # exclude NA days
  ),
  # Generate summaries and scores with evaluation data ----------------------
  # Create a mega wrapper function so that we are not pulling all the draws
  # into memory when returning the `mapped_values`
  tar_target(
    name = su_quantile_covid,
    command = get_nowcast_quantiles(
      point_nowcast_matrix = point_nowcast_mat,
      reporting_triangle = triangle,
      dispersion = disp_params,
      draws = config$n_draws,
      fun_to_aggregate = sum,
      k = 7,
      days_to_eval = config$covid$days_to_eval,
      nowcast_date = nowcast_dates_covid,
      date_df = date_df,
      data_as_of_df = data_as_of_df,
      eval_data = eval_data_7d,
      model = "base", # Here this is the only model we are using
      # These will all vary
      n_history_delay = n_history_delay,
      n_history_uncertainty = n_history_uncertainty,
      borrow = borrow,
      partial_rep_tri = partial_rep_tri,
      age_group = age_group_to_nowcast
    )
  ),
  tar_target(
    name = pt_nowcast_df,
    command = data.frame(pt_nowcast = rollapply(
      rowSums(point_nowcast_mat),
      7,
      sum,
      fill = NA,
      align = "right"
    )) |>
      mutate(time = 1:nrow(point_nowcast_mat))
  ),
  tar_target(
    name = delay_df,
    command = data.frame(
      delay = delay_pmf,
      delay_time = 0:(length(delay_pmf) - 1)
    ) |>
      mutate(
        model = "base",
        nowcast_date = nowcast_dates_covid,
        age_group = age_group_to_nowcast,
        n_history_delay = n_history_delay,
        n_history_uncertainty = n_history_uncertainty,
        borrow = borrow,
        partial_rep_tri = partial_rep_tri
      )
  ),
  tar_target(
    name = mean_delay_df,
    command = data.frame(
      mean_delay =
        sum(delay_pmf * (0:(length(delay_pmf) - 1)))
    ) |>
      mutate(
        model = "base",
        nowcast_date = nowcast_dates_covid,
        age_group = age_group_to_nowcast,
        n_history_delay = n_history_delay,
        n_history_uncertainty = n_history_uncertainty,
        borrow = borrow,
        partial_rep_tri = partial_rep_tri
      )
  ),

  ## Forecast objects ---------------------------------------------------------

  # Get a wide dataframe with only 50th and 90th for plotting
  tar_target(
    name = summary_nowcast_covid,
    command = su_quantile_covid |>
      # Need to transform back to natural scale
      mutate(
        observed = exp(observed),
        predicted = exp(predicted)
      ) |>
      filter(quantile_level %in% config$plot_quantiles) |>
      pivot_wider(
        names_from = "quantile_level",
        values_from = "predicted",
        names_prefix = "q_"
      ) |> left_join(data_as_of_df, by = "reference_date")
  ),
  tar_target(
    name = pt_nowcast_7d,
    command = pt_nowcast_df |>
      left_join(date_df, by = "time") |>
      select(reference_date, pt_nowcast) |>
      filter(reference_date >=
        ymd(nowcast_dates_covid) - days(config$covid$days_to_eval - 1)) |>
      mutate(
        nowcast_date = nowcast_dates_covid,
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
        partial_rep_tri = partial_rep_tri,
      ) |>
      filter(reference_date >= min(reference_date) + days(6)) |> # exclude NA days
      left_join(eval_data_7d, by = "reference_date")
  ),

  ## Scores--------------------------------------------------------------------
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
        "borrow",
        "partial_rep_tri"
      )
    )
  )
)
