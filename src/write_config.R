write_config <- function(noro_nowcast_dates = NULL,
                         covid_nowcast_dates = NULL,
                         filter_ref_dates_noro = NULL,
                         age_groups_covid = NULL,
                         n_history_uncertainty_covid = NULL,
                         n_history_delay_covid = NULL,
                         borrow = NULL,
                         permutations_covid = TRUE) {
  norovirus_url <- "https://raw.githubusercontent.com/jonathonmellor/norovirus-nowcast/refs/heads/main/outputs/data/cases_with_noise.csv" # nolint
  # covid_url <- "https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/main/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv" # nolint
  # Use the august 8th data, as is in the paper
  covid_url <- "https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/11c745322c055cfbd4f0c8f72241642a50aea399/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv"
  # Lock to specific commit for KIT nowcasts
  KIT_nowcast_url_prefix <- "https://raw.githubusercontent.com/kaitejohnson/hospitalization-nowcast-hub/refs/heads/main/data-processed_retrospective/KIT-simple_nowcast_original"
  # point to the bug fixed quantiles, locking in on specific commit
  KIT_nowcast_revised_url_prefix <- "https://raw.githubusercontent.com/kaitejohnson/hospitalization-nowcast-hub/refs/heads/main/data-processed_retrospective/KIT-simple_nowcast_revised"
  if (is.null(noro_nowcast_dates)) {
    noro_nowcast_dates <- as.character(
      seq(
        from = ymd("2023-11-05"),
        to = ymd("2024-03-10"),
        by = "week"
      )
    )
  }

  if (is.null(covid_nowcast_dates)) {
    covid_nowcast_dates <- as.character(
      seq(
        from = ymd("2021-11-22"),
        to = ymd("2022-04-29"),
        by = "day"
      )
    )
  }

  # Norovirus vectors of permutations------------------------------------------
  n_training_volume_noro <- 56 # used by Mellor for their GAM
  df_base <- data.frame(
    nowcast_dates = noro_nowcast_dates
  ) |>
    mutate(
      n_history_training_volume = n_training_volume_noro,
      filter_ref_dates = FALSE
    )

  # Set up the variations for filtering by wday
  if (is.null(filter_ref_dates_noro)) {
    n_history_training_volume_orig <- n_training_volume_noro
    n_training_volume_noro <- c(
      n_history_training_volume_orig,
      floor(n_history_training_volume_orig / 7)
    )
    filter_ref_dates_noro <- TRUE

    df_filter <- expand.grid(
      nowcast_dates = noro_nowcast_dates,
      n_history_training_volume = n_training_volume_noro,
      filter_ref_dates = TRUE
    )

    df_noro <- bind_rows(
      df_base,
      df_filter
    )
  } else {
    df_noro <- df_base
  }



  # Covid vectors of permutations-------------------------------------------
  # Set up the pairwise alterations from the base case (but start with base)
  max_delay <- 40
  base_n_history <- 3 * max_delay
  base_borrow <- FALSE
  partial_rep_tri <- TRUE
  if (is.null(age_groups_covid)) {
    age_groups_covid <- c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+")
  }
  df_base_covid <- expand.grid(
    nowcast_dates = covid_nowcast_dates,
    age_groups = age_groups_covid
  ) |>
    # For the default case, n_history and n_uncertainty are split evenly
    mutate(
      n_history = base_n_history,
      n_history_delay = round(0.5 * base_n_history),
      n_history_uncertainty = round(0.5 * base_n_history),
      borrow = base_borrow,
      partial_rep_tri = partial_rep_tri,
      weekday_filter = FALSE
    )
  # result_df should be 7* length of df_base_covid
  if (permutations_covid) {
    result_df <- create_pairwise_variations(df_base_covid,
      multipliers = c(2.0, 0.5),
      max_delay = max_delay
    )
  } else {
    result_df <- df_base_covid
  }


  config <- list(
    norovirus = list(
      url = norovirus_url,
      max_delay = 14,
      borrow = FALSE,
      days_to_eval = 7,
      quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), # Used in Mellor et al
      eval_timeframe = 50,
      # Variables to map over
      nowcast_dates = df_noro |> pull(nowcast_dates) |> as.vector(),
      n_history_training_volume = df_noro |> pull(n_history_training_volume) |> as.vector(),
      filter_ref_dates = df_noro |> pull(filter_ref_dates) |> as.vector()
    ),
    covid = list(
      url = covid_url,
      KIT_nowcast_url_prefix = KIT_nowcast_url_prefix,
      KIT_nowcast_revised_url_prefix = KIT_nowcast_revised_url_prefix,
      nowcast_dates = result_df |> pull(nowcast_dates) |> as.vector(),
      age_groups = result_df |> pull(age_groups) |> as.vector(),
      n_history_delay = result_df |> pull(n_history_delay) |> as.vector(),
      n_history_uncertainty = result_df |> pull(n_history_uncertainty) |> as.vector(),
      n_history_dispersion = result_df |> pull(n_history_uncertainty) |> as.vector(),
      borrow = result_df |> pull(borrow) |> as.vector(),
      partial_rep_tri = result_df |> pull(partial_rep_tri) |> as.vector(),
      max_delay = 40,
      days_to_eval = 29, # 0 to - 28 horizon)
      quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
      eval_timeframe = 40
    ),
    n_draws = 1000,
    plot_quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975)
  )

  yaml::write_yaml(config,
    file = file.path("input", "config", "config.yaml")
  )
  return(NULL)
}


create_pairwise_variations <- function(df_base_covid,
                                       multipliers = c(2.0, 0.5),
                                       max_delay = max_delay) {
  # Original parameters
  params <- c("n_history_delay", "n_history_uncertainty")
  params_bool <- c("borrow", "partial_rep_tri", "weekday_filter")

  # Loop through each numeric parameter
  # Create pairwise variations
  for (j in seq_along(multipliers)) {
    mult <- multipliers[j]
    # Create a copy of the base dataframe
    df_variation <- df_base_covid
    df_variation[["n_history"]] <- df_base_covid[["n_history"]] * mult
    df_variation[["n_history_delay"]] <- max(
      df_variation[["n_history_delay"]] * mult,
      max_delay + 1
    )
    df_variation[["n_history_uncertainty"]] <- df_variation[["n_history"]] - df_variation[["n_history_delay"]] # nolint
    if (j == 1) {
      df_long <- bind_rows(df_base_covid, df_variation)
    } else {
      df_long <- bind_rows(df_long, df_variation)
    }
  }

  # Loop through each boolean parameter
  for (i in seq_along(params_bool)) {
    param_i <- params_bool[i]

    # Create pairwise variations
    df_variation <- df_base_covid

    # Apply multiplier to parameter i
    df_variation[[param_i]] <- !df_base_covid[[param_i]]
    df_long <- bind_rows(df_long, df_variation)
  }
  return(df_long)
}
