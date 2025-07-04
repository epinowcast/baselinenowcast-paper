#' Derive model variations
#'
#' @param df dataframe with columns of metadata on covid model permutation
#'    variations
#' @autoglobal
#' @importFrom glue glue
#' @importFrom dplyr case_when
#' @returns dataframe with two new columns for model_variation and
#'    model_variation_string
derive_model_variation <- function(df) {
  df_w_helpers <- df |>
    mutate(
      training_volume = n_history_delay + n_history_uncertainty,
      model_variation = case_when(
        training_volume != 120 & !borrow & partial_rep_tri ~
          "Training volume",
        training_volume == 120 & !borrow & !partial_rep_tri ~
          "Reporting triangle completeness",
        training_volume == 120 & !borrow & partial_rep_tri ~
          "Baseline validation",
        training_volume == 120 & borrow & partial_rep_tri ~
          "Borrow for delay and uncertainty estimation",
        .default = "Other"
      ),
      model_variation_string = dplyr::case_match(
        model_variation,
        "Training volume" ~
          glue("Delay:{n_history_delay},\nUncertainty:{n_history_uncertainty}"),
        "Reporting triangle completeness" ~
          "Complete reporting triangle",
        "Borrow for delay and uncertainty estimation" ~
          "Borrowed estimates from all age groups",
        "Baseline validation" ~
          "Baseline validation approach",
        .default = model_variation
      ),
      model_variation_string = case_when(
        n_history_delay == 41 && n_history_uncertainty == 19 ~
          "Reduced delay estimate",
        n_history_delay == 50 && n_history_uncertainty == 10 ~
          "Reduced uncertainty estimate",
        n_history_delay == 60 && n_history_uncertainty == 180 ~
          "Increased uncertainty estimate",
        n_history_delay == 180 && n_history_uncertainty == 60 ~
          "Increased delay estimate",
        TRUE ~ model_variation_string
      )
    )
  # Put them in order so increases are grouped together
  df_w_helpers$model_variation_string <- factor(
    df_w_helpers$model_variation_string,
    levels = c(
      "Baseline validation approach",
      "Borrowed estimates from all age groups",
      "Complete reporting triangle",
      "Increased delay estimate",
      "Increased uncertainty estimate",
      "Reduced delay estimate",
      "Reduced uncertainty estimate"
    )
  )

  return(df_w_helpers)
}
