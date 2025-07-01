noro_comparison_targets <- list(
  # Load in scores and coverage from Mellor et al (for now use the same
  # data as from the model permutations with some noise)
  tar_target(
    name = mellor_model_nowcasts,
    command = get_mellor_et_al_outputs(
      days_to_eval = config$noro$days_to_eval,
      data_from_bnc = all_nowcasts_noro,
      local_load = TRUE
    )
  ),
  tar_target(
    name = mellor_baselinenowcast_nowcasts,
    command = get_mellor_et_al_outputs(
      data_from_bnc = all_nowcasts_noro,
      days_to_eval = config$noro$days_to_eval,
      model_names = c(
        "baselinenowcast_model1",
        "baselinenowcast_model2",
        "baselinenowcast_model3"
      ),
      local_load = TRUE
    )
  ),
  tar_target(
    name = su_mellor_model_quantiles,
    command = mellor_model_nowcasts |>
      pivot_longer(
        names_to = "quantile",
        values_to = "predicted",
        cols = starts_with("q_"),
        names_prefix = "q_"
      ) |>
      mutate(quantile_level = as.numeric(quantile)) |>
      scoringutils::as_forecast_quantile(
        forecast_unit = c(
          "nowcast_date",
          "reference_date",
          "model"
        )
      )
  ),
  tar_target(
    name = mellor_model_scores,
    command = su_mellor_model_quantiles |>
      scoringutils::score()
  ),
  tar_target(
    name = mellor_model_coverage,
    command = su_mellor_model_quantiles |>
      scoringutils::get_coverage(
        by = c(
          "nowcast_date",
          "reference_date",
          "model"
        )
      )
  ),

  # Combines scores and coverage from other models with baselinenowcast
  # permutations
  tar_target(
    name = noro_scores,
    command = bind_rows(
      mellor_model_scores,
      all_scores_noro
    ) |>
      add_column_for_noro_model_type()
  ),
  tar_target(
    name = noro_nowcasts,
    command = bind_rows(
      mellor_model_nowcasts,
      all_nowcasts_noro
    ) |>
      add_column_for_noro_model_type()
  ),
  tar_target(
    name = noro_coverage,
    command = bind_rows(
      mellor_model_coverage,
      all_coverage_noro
    ) |>
      add_column_for_noro_model_type()
  )
)
