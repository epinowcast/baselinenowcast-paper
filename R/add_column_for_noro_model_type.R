#' Add a column to distinguish between nowcasts produced by baselinenowcast and
#'    those produced in Mellor et al.
#'
#' @param df Data.frame containing scores, coverage, or quantiled nowcasts
#'   for each of the three baselinenowcast model configurations
#' @autoglobal
#' @importFrom dplyr mutate case_when
#' @returns Data.frame with a new column for model type.
add_column_for_noro_model_type <- function(df) {
  df_w_add_column <- df |>
    mutate(
      model_type = case_when(
        model %in% c(
          "base",
          "filter weekday large training volume",
          "filter weekday small training volume"
        ) ~ "baselinenowcast", # nolint
        TRUE ~ "Comparison from Mellor et al"
      )
    )
  return(df_w_add_column)
}
