model_permutation_targets <- list(
  # Nowcasts-----------------------------------------------------------------
  tar_target(
    name = combined_nowcasts_mp,
    command = all_nowcasts_covid |>
      mutate(
        nowcast_date = as.Date(nowcast_date),
        horizon = as.integer(reference_date - nowcast_date),
        model = ifelse(model == "base", "baselinenowcast", model),
        training_volume = n_history_delay + n_history_uncertainty
      ) |>
      mutate(
        model_variation = case_when(
          (training_volume != 120 & borrow == FALSE & partial_rep_tri == TRUE) ~ "Training volume",
          (training_volume == 120 & borrow == FALSE & partial_rep_tri == FALSE) ~ "Reporting triangle completeness",
          (training_volume == 120 & borrow == FALSE & partial_rep_tri == TRUE) ~ "Baseline validation",
          (training_volume == 120 & borrow == TRUE & partial_rep_tri == TRUE) ~ "Borrow for delay and uncertainty estimation"
        ),
        model_variation_string =
          case_when(
            model_variation == "Training volume" ~ glue::glue("Delay:{n_history_delay},\nUncertainty:{n_history_uncertainty}"),
            model_variation == "Reporting triangle completeness" ~ "Complete reporting triangle",
            model_variation == "Borrow for delay and uncertainty estimation" ~ "Borrowed estimates from all age groups",
            model_variation == "Baseline validation" ~ "Baseline validation approach"
          )
      )
  )
)
