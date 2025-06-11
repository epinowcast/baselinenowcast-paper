gen_noro_nowcasts_targets <- list(
  # If specified, filter to only a certain weekday for the pipeline run.
  # These will be joined together later.
  # This will ultimate produce nowcasts only for one specific weekday.
  tar_target(
    name = noro_df,
    command = {
      if (filter_ref_dates == TRUE) {
        noro_df <- noro_long[wday(noro_long$reference_date) == weekdays_noro]
      } else {
        noro_long
      }
    }
  ),
  # We pass into the mapping the varying length of the training volume, and
  # then split in half for the delay and uncertainty.
  tar_target(
    name = n_history_delay,
    command = floor(n_history_training_volume / 2)
  ),
  tar_target(
    name = n_history_uncertainty,
    command = floor(n_history_training_volume / 2)
  ),


  # Run baselinenowcast pipeline------------------------------------------------
  # Generate reporting triangle
  tar_target(
    name = triangle,
    command = get_rep_tri_from_long_df(
      long_df = noro_df,
      nowcast_date = nowcast_dates_noro,
      max_delay = config$noro$max_delay
    ) |> select(
      -reference_date, -nowcast_date
    ) |> as.matrix()
  ),
  # Estimate delay
  tar_target(
    name = delay_pmf,
    command = get_delay_estimate(
      reporting_triangle = triangle,
      max_delay = config$noro$max_delay,
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
  # Get a list of truncated reporting triangles
  tar_target(
    name = truncated_rts,
    command = truncate_triangles(
      reporting_triangle = triangle,
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
  # Use retrospective nowcasts and the observations to estimate dispersion
  tar_target(
    name = disp_params,
    command = estimate_dispersion(
      pt_nowcast_mat_list = retro_nowcasts,
      trunc_rep_tri_list = truncated_rts,
      reporting_triangle_list = retro_rts
    )
  ),
  # Aggregate predictions by reference time
  tar_target(
    name = nowcast_draws_df,
    command = get_nowcast_draws(
      point_nowcast_matrix = point_nowcast_mat,
      reporting_triangle = triangle,
      dispersion = disp_params,
      draws = config$n_draws
    )
  ),
  # Generate summaries and scores with evaluation data-----------------------
  # Join predictions and observations
  tar_target(
    name = reference_dates,
    command = noro_df |>
      filter(reference_date <= nowcast_dates_noro) |>
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
  # Get evaluation data to join
  tar_target(
    name = eval_data,
    command = get_eval_data_from_long_df(
      long_df = noro_df,
      max_delay = config$norovirus$max_delay,
      as_of_date = ymd(nowcast_dates_noro) + days(config$norovirus$eval_timeframe)
    )
  ),
  # Get as of data we want to join
  tar_target(
    name = data_as_of_df,
    command = noro_df |>
      filter(report_date <= nowcast_dates_noro) |>
      group_by(reference_date) |>
      summarise(
        data_as_of = sum(count, na.rm = TRUE)
      )
  ),
  # Join eval data to the subset of the nowcast that we are evaluating
  tar_target(
    name = comb_nc_noro,
    command = nowcast_draws_df |>
      left_join(date_df, by = "time") |>
      select(reference_date, draw, pred_count) |>
      mutate(nowcast_date = nowcast_dates_noro) |>
      left_join(data_as_of_df, by = "reference_date") |>
      mutate(
        total_count = pred_count,
        model = ifelse(filter_ref_dates, "filter_weekday", "base"),
        # These will all vary
        n_history_delay = n_history_delay,
        n_history_uncertainty = n_history_uncertainty
      ) |>
      filter(reference_date >=
        ymd(nowcast_dates_noro) - days(config$norovirus$days_to_eval - 1)) |>
      left_join(eval_data, by = "reference_date")
  ),
  ## Forecast objects ---------------------------------------------------------
  tar_target(
    name = su_sample_noro,
    command = scoringutils::as_forecast_sample(
      data = comb_nc_noro,
      forecast_unit = c(
        "nowcast_date",
        "reference_date",
        "model",
        "n_history_delay",
        "n_history_uncertainty"
      ),
      observed = "observed",
      predicted = "total_count",
      sample_id = "draw"
    )
  ),
  # Forecast quantiles as a scoringutils forecast object
  tar_target(
    name = su_quantile_noro,
    command = scoringutils::as_forecast_quantile(
      data = su_sample_noro,
      probs = config$norovirus$quantiles
    )
  ),
  tar_target(
    name = su_quantile_noro_plot,
    command = scoringutils::as_forecast_quantile(
      data = su_sample_noro,
      probs = config$plot_quantiles
    )
  ),
  # Get a wide dataframe with only 50th and 90th for plotting
  tar_target(
    name = summary_nowcast_noro,
    command = su_quantile_noro_plot |>
      filter(quantile_level %in% config$plot_quantiles) |>
      pivot_wider(
        names_from = "quantile_level",
        values_from = "predicted",
        names_prefix = "q_"
      ) |> left_join(data_as_of_df, by = "reference_date")
  ),
  ## Scores--------------------------------------------------------------------
  tar_target(
    name = scores_sample_noro,
    command = scoringutils::score(su_sample_noro)
  ),
  tar_target(
    name = scores_quantile_noro,
    command = scoringutils::score(su_quantile_noro)
  ),
  tar_target(
    name = coverage_noro,
    command = scoringutils::get_coverage(
      su_quantile_noro,
      by = c(
        "model",
        "nowcast_date",
        "reference_date"
      )
    )
  )
)
