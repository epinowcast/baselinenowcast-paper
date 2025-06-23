noro_comparison_targets <- list(
  # Load in scores and coverage from Mellor et al (for now use the same
  # data as from the model permutations with some noise)
  tar_target(
    name = mellor_model_scores,
    command = get_mellor_et_al_outputs(
      data_from_bnc = all_scores_noro
    )
  ),
  tar_target(
    name = mellor_model_coverage,
    command = get_mellor_et_al_outputs(
      data_from_bnc = all_coverage_noro
    )
  ),
  tar_target(
    name = mellor_model_nowcasts,
    command = get_mellor_et_al_outputs(
      data_from_bnc = all_nowcasts_noro
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
