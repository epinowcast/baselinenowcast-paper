EDA_plot_targets <- list(
  # Covid---------------------------------------------------------------------
  tar_target(
    name = plot_covid_data,
    command = get_plot_data_as_of(
      final_df = covid_long_all_strata |>
        filter(
          age_group == "00+",
          report_date <= "2022-07-01"
        ),
      as_of_dates = c("2021-12-01", "2022-02-01", "2022-04-01"),
      pathogen = "Covid"
    ),
    format = "rds"
  ),
  tar_target(
    name = final_eval_data_covid_daily,
    command = get_eval_data_from_long_df(
      long_df = covid_long_all_strata |> filter(age_group == "00+"),
      as_of_date = ymd(max(config$covid$nowcast_dates)) + days(config$covid$eval_timeframe)
    )
  ),
  tar_target(
    name = final_eval_data_covid_7d,
    command = final_eval_data_covid_daily |>
      arrange(reference_date) |>
      mutate(observed = rollapply(observed,
        width = 7,
        FUN = sum,
        fill = NA, align = "right"
      ))
  ),
  # Make sure quantiled nowcasts are performing reasonably.
  tar_target(
    name = plot_covid_nowcasts,
    command = get_plot_mult_nowcasts(
      all_nowcasts = all_nowcasts_covid |>
        filter(
          age_group == "00+",
          model == "base",
          n_history_delay == 60,
          n_history_uncertainty == 60,
          borrow == FALSE,
          partial_rep_tri == TRUE
        ),
      final_summed_data = final_eval_data_covid_7d,
      nowcast_dates_to_plot = c("2021-12-01", "2022-02-01", "2022-04-01"),
      pathogen = "Covid 7 day",
      title = "baselinenowcast nowcasts"
    )
  ),
  tar_target(
    name = plot_kit_nowcasts,
    command = get_plot_mult_nowcasts(
      all_nowcasts = all_nowcasts_kit |>
        filter(
          age_group == "00+"
        ),
      final_summed_data = final_eval_data_covid_7d,
      nowcast_dates_to_plot = c("2021-12-01", "2022-02-01", "2022-04-01"),
      pathogen = "Covid 7 day",
      title = "KIT nowcasts"
    )
  ),
  tar_target(
    name = pt_nowcasts_combined,
    command = all_pt_nowcasts |>
      filter(
        age_group == "00+",
        model == "base",
        n_history_delay == 60,
        n_history_uncertainty == 60,
        borrow == FALSE,
        partial_rep_tri == TRUE
      ) |>
      select(colnames(all_pt_nowcasts_kit)) |>
      mutate(nowcast_date = ymd(nowcast_date)) |>
      bind_rows(all_pt_nowcasts_kit |> filter(age_group == "00+"))
  ),
  tar_target(
    name = plot_pt_nowcast_comparison,
    command = get_plot_pt_nowcasts(
      pt_nowcasts_combined = pt_nowcasts_combined,
      final_summed_data = final_eval_data_covid_7d,
      nowcast_dates_to_plot = c("2021-12-01", "2022-02-01", "2022-04-01"),
      pathogen = "Covid 7 day",
      title = "Point nowcast comparison"
    )
  ),
  tar_target(
    name = plot_pt_nowcast_kit,
    command = get_plot_pt_nowcasts(
      pt_nowcasts_combined = all_pt_nowcasts_kit |> filter(age_group == "00+"),
      final_summed_data = final_eval_data_covid_7d,
      nowcast_dates_to_plot = c("2021-12-01", "2022-02-01", "2022-04-01"),
      pathogen = "Covid 7 day",
      title = "KIT point nowcasts"
    )
  ),
  tar_target(
    name = plot_pt_nowcast_bc,
    command = get_plot_pt_nowcasts(
      pt_nowcasts_combined = all_pt_nowcasts |>
        filter(
          age_group == "00+",
          model == "base",
          n_history_delay == 60,
          n_history_uncertainty == 60,
          borrow == FALSE,
          partial_rep_tri == TRUE
        ),
      final_summed_data = final_eval_data_covid_7d,
      nowcast_dates_to_plot = c("2021-12-01", "2022-02-01", "2022-04-01"),
      pathogen = "Covid 7 day",
      title = "baselinenowcast point nowcasts"
    )
  ),
  # Norovirus--------------------------------------------------------------------
  # Data plots
  tar_target(
    name = plot_noro_data,
    command = get_plot_data_as_of(
      final_df = noro_long,
      as_of_dates = c(
        "2023-12-10", "2024-01-21",
        "2024-02-25"
      ),
      pathogen = "Norovirus"
    ),
    format = "rds"
  ),
  tar_target(
    name = final_eval_data_noro,
    command = get_eval_data_from_long_df(
      long_df = noro_long,
      as_of_date = ymd(max(config$norovirus$nowcast_dates)) + days(config$norovirus$eval_timeframe)
    )
  ),
  # Make sure quantiled nowcasts are performing reasonably.
  tar_target(
    name = plot_noro_nowcasts,
    command = get_plot_mult_nowcasts(
      all_nowcasts = all_nowcasts_noro |>
        filter(model == "base", n_history_delay == 28),
      final_summed_data = final_eval_data_noro,
      nowcast_dates_to_plot = c("2023-12-10", "2024-01-21", "2024-02-25"),
      pathogen = "Norovirus"
    )
  )
)
