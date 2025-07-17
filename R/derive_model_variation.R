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
          "Default",
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
        "Default" ~
          "Default",
        .default = model_variation
      ),
      model_variation_string = case_when(
        n_history_delay == 41 & n_history_uncertainty == 19 ~
          "50% reduced training volume",
        n_history_delay == 120 & n_history_uncertainty == 120 ~
          "200% increased training volume",
        TRUE ~ model_variation_string
      )
    )
  # Put them in order so increases are grouped together
  df_w_helpers$model_variation_string <- factor(
    df_w_helpers$model_variation_string,
    levels = c(
      "Default",
      "Borrowed estimates from all age groups",
      "Complete reporting triangle",
      "200% increased training volume",
      "50% reduced training volume"
    )
  )

  return(df_w_helpers)
}
