noro_comparison_targets <- list(
  # Load in scores and coverage from Mellor et al (for now use the same
  # data as from the model permutations with some noise)
  tar_target(
    name = mellor_model_nowcasts,
    command = get_mellor_et_al_outputs(
      days_to_eval = config$noro$days_to_eval,
      eval_timeframe = config$noro$eval_timeframe,
      data_from_bnc = all_nowcasts_noro,
      synthetic_data = FALSE # set to TRUE to use public data
    )
  ),
  tar_target(
    name = mellor_baselinenowcast_nowcasts,
    command = get_mellor_et_al_outputs(
      data_from_bnc = all_nowcasts_noro,
      days_to_eval = config$noro$days_to_eval,
      eval_timeframe = config$noro$eval_timeframe,
      model_names = c(
        "baselinenowcast_model1",
        "baselinenowcast_model2",
        "baselinenowcast_model3"
      ),
      synthetic_data = FALSE # set to TRUE to use public data
    )
  ),
  tar_target(
    name = all_mellor_nowcasts,
    command = bind_rows(
      mellor_model_nowcasts,
      mellor_baselinenowcast_nowcasts
    )
  ),
  tar_target(
    name = su_mellor_model_quantiles,
    command = all_mellor_nowcasts |>
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
  # Compare both baselinenowcast implementations------------------------------
  tar_target(
    name = bnc_quantiles,
    command = bind_rows(
      all_nowcasts_noro,
      mellor_baselinenowcast_nowcasts
    ) |>
      mutate(
        model_type =
          case_when(
            model %in% c("base", "baselinenowcast_model1") ~ "default",
            model %in% c("baselinenowcast_model2", "filter weekday small training volume") ~ "dow small",
            model %in% c("baselinenowcast_model3", "filter weekday large training volume") ~ "dow large"
          )
      )
  ),
  tar_target(
    name = compare_bncs,
    command = get_plot_mult_nowcasts_noro(bnc_quantiles)
  ),
  tar_target(
    name = bnc_scores,
    command = bind_rows(
      all_scores_noro,
      mellor_model_scores |>
        filter(!model %in% c("epinowcast", "baseline Mellor et al", "GAM"))
    ) |>
      mutate(
        model_type =
          case_when(
            model %in% c("base", "baselinenowcast_model1") ~ "default",
            model %in% c("baselinenowcast_model2", "filter weekday small training volume") ~ "dow small",
            model %in% c("baselinenowcast_model3", "filter weekday large training volume") ~ "dow large"
          )
      )
  ),
  tar_target(
    name = compare_scores,
    command = get_bar_chart_sum_scores_noro(bnc_scores)
  ),

  # Combines scores and coverage from other models with baselinenowcast
  # permutations
  tar_target(
    name = noro_scores,
    command = mellor_model_scores |>
      replace_Mellor_name_with_ours() |>
      add_column_for_noro_model_type()
  ),
  tar_target(
    name = noro_nowcasts,
    command =
      all_mellor_nowcasts |>
        replace_Mellor_name_with_ours() |>
        add_column_for_noro_model_type()
  ),
  tar_target(
    name = noro_coverage,
    command = mellor_model_coverage |>
      replace_Mellor_name_with_ours() |>
      add_column_for_noro_model_type()
  )
)
