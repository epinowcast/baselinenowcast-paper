kit_nowcast_targets <- list(

  # Get the url
  tar_target(
    name = kit_nowcast_url,
    command = get_kit_nowcast_url(
      prefix = config$covid$KIT_nowcast_url_prefix,
      nowcast_date = nowcast_dates_covid
    )
  ),
  ### Load and clean the nowcasts for each date-------------------------------
  tar_target(
    name = kit_nowcast_raw,
    # Filter to only what you need (age groups and national, no regions)
    command = readr::read_csv(kit_nowcast_url) |>
      rename(
        nowcast_date = forecast_date,
        reference_date = target_end_date,
        predicted = value
      ) |>
      mutate(
        model = "KIT simple nowcast"
      )
  ),
  tar_target(
    name = kit_nowcast,
    command = kit_nowcast_raw |>
      filter(
        location == "DE",
        pathogen == "COVID-19",
        type == "quantile"
      ) |>
      select(
        nowcast_date, reference_date,
        model, age_group, quantile, predicted
      )
  ),
  tar_target(
    name = kit_pt_nowcast,
    command = kit_nowcast_raw |>
      filter(
        location == "DE",
        pathogen == "COVID-19",
        type == "mean"
      ) |>
      select(
        nowcast_date, reference_date,
        model, age_group, predicted
      )
  ),
  # Evaluation data-------------------------------------------------------------
  tar_target(
    name = eval_data_daily,
    command = covid_long_all_strata |>
      filter(report_date <= ymd(nowcast_dates_covid) +
        days(config$covid$eval_timeframe)) |>
      group_by(reference_date, age_group) |>
      summarise(
        observed = sum(count, na.rm = TRUE)
      ) |>
      ungroup() |>
      mutate(
        as_of_date = ymd(nowcast_dates_covid) +
          days(config$covid$eval_timeframe)
      ) |>
      filter(reference_date <= ymd(nowcast_dates_covid) +
        days(config$covid$eval_timeframe))
  ),
  tar_target(
    name = eval_data_7d,
    command = eval_data_daily |>
      arrange(reference_date) |>
      group_by(age_group) |>
      mutate(observed = rollsum(observed,
        k = 7,
        fill = NA, align = "right"
      )) |>
      filter(reference_date >= min(reference_date) + days(6)) # exclude NA days
  ),
  # Get as of data we want to join
  tar_target(
    name = data_as_of_daily,
    command = covid_long_all_strata |>
      filter(report_date <= nowcast_dates_covid) |>
      group_by(reference_date, age_group) |>
      summarise(
        data_as_of = sum(count, na.rm = TRUE)
      )
  ),
  # Get as of data we want to join
  tar_target(
    name = data_as_of_7d,
    command = data_as_of_daily |>
      arrange(reference_date) |>
      group_by(age_group) |>
      mutate(data_as_of = rollsum(data_as_of,
        k = 7,
        fill = NA, align = "right"
      )) |>
      filter(reference_date >= min(reference_date) + days(6)) # exclude NA days
  ),
  tar_target(
    name = comb_nc_kit,
    command = kit_nowcast |>
      filter(reference_date >=
        ymd(nowcast_dates_covid) - days(config$covid$days_to_eval - 1)) |>
      left_join(eval_data_7d, by = c("reference_date", "age_group"))
  ),
  tar_target(
    name = summary_pt_nowcast_kit,
    command = kit_pt_nowcast |>
      filter(reference_date >=
        ymd(nowcast_dates_covid) - days(config$covid$days_to_eval - 1)) |>
      left_join(eval_data_7d, by = c("reference_date", "age_group")) |>
      left_join(data_as_of_7d, by = c("reference_date", "age_group"))
  ),


  ## Forecast objects---------------------------------------------------------
  tar_target(
    name = su_quantile_kit,
    command = scoringutils::as_forecast_quantile(
      comb_nc_kit,
      # All the metadata we will want to keep track of
      forecast_unit = c(
        "nowcast_date",
        "reference_date",
        "age_group",
        "model"
      ),
      observed = "observed",
      predicted = "predicted",
      quantile_level = "quantile"
    )
  ),
  # Get a wide dataframe with only 50th and 90th for plotting
  tar_target(
    name = summary_nowcast_kit,
    command = su_quantile_kit |>
      filter(quantile_level %in% config$plot_quantiles) |>
      pivot_wider(
        names_from = "quantile_level",
        values_from = "predicted",
        names_prefix = "q_"
      ) |> left_join(data_as_of_7d, by = c("reference_date", "age_group"))
  ),
  ## Scores -----------------------------------------------------------------
  tar_target(
    name = scores_quantile_kit,
    command = scoringutils::score(su_quantile_kit)
  ),
  tar_target(
    name = coverage_kit,
    command = scoringutils::get_coverage(
      su_quantile_kit,
      by = c(
        "nowcast_date",
        "reference_date",
        "age_group",
        "model"
      )
    )
  )
)
