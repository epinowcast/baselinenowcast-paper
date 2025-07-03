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
          "baselinenowcast default",
          "filter weekday large training volume",
          "baselinenowcast weekday\nfilter small training volume",
          "filter weekday small training volume",
          "baselinenowcast weekday\nfilter large training volume"
        ) ~ "baselinenowcast", # nolint
        TRUE ~ "Comparison from Mellor et al"
      )
    )
  return(df_w_add_column)
}
#' Replace model name from Mellor et al with the one we are using for their
#'   outputs
#'
#' @param df Data.frame containing scores, coverage, or quantiled nowcasts
#'   for each of the three baselinenowcast model configurations
#' @autoglobal
#' @importFrom dplyr mutate case_when
#' @returns Data.frame with new model names in the `model` column.
replace_Mellor_name_with_ours <- function(df) {
  df_w_new_model_name <- df |>
    mutate(model = case_when(
      model == "baselinenowcast_model1" ~
        "baselinenowcast default",
      model == "baselinenowcast_model2" ~
        "baselinenowcast weekday\nfilter small training volume",
      model == "baselinenowcast_model3" ~
        "baselinenowcast weekday\nfilter large training volume",
      TRUE ~ model
    ))
  return(df_w_new_model_name)
}
