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
          (training_volume != 120 & isFALSE(borrow) & isTRUE(partial_rep_tri)) ~ "Training volume",
          (training_volume == 120 & isFALSE(borrow) & isFALSE(partial_rep_tri)) ~ "Reporting triangle completeness",
          (training_volume == 120 & isFALSE(borrow) & isTRUE(partial_rep_tri)) ~ "Baseline validation",
          (training_volume == 120 & isTRUE(borrow) & isTRUE(partial_rep_tri)) ~ "Borrow from all age groups"
        ),
        model_variation_string =
          case_when(
            model_variation == "Training volume" ~ glue::glue("Delay:{n_history_delay},\nUncertainty:{n_history_uncertainty}"),
            model_variation == "Reporting_triangle_completeness" ~ "Complete reporting triangle",
            model_variation == "Borrow from all age groups" ~ "Borrowed estimates from all age groups",
            model_variation == "Baseline validation approach" ~ "Baseline validation"
          )
      )
  )
)
