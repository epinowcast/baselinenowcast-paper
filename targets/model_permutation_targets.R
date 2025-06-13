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
      derive_model_variation()
  ),
  # Scores---------------------------------------------------------------------
  tar_target(
    name = scores_mp,
    command = all_scores_covid |>
      mutate(
        nowcast_date = as.Date(nowcast_date),
        horizon = as.integer(reference_date - nowcast_date),
        model = ifelse(model == "base", "baselinenowcast", model),
        training_volume = n_history_delay + n_history_uncertainty
      ) |>
      derive_model_variation()
  ),

  # Coverage-------------------------------------------------------------------
  tar_target(
    name = coverage_mp,
    command = all_coverage_covid |>
     derive_model_variation()
  )
)
