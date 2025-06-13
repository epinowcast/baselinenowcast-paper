#' Suppress output and messages for code.
#' @param code Code to run quietly.
#' @return The result of running the code.
#' @export
#' @examples
#' result <- quiet(message("This message should be suppressed"))
#' print(result)
quiet <- function(code) {
  sink(nullfile())
  on.exit(sink())
  return(suppressMessages(code))
}


#' Derive model variations
#'
#' @param df dataframe with columns of metadata on covid model permutation
#'    variations
#'
#' @returns dataframe with two new columns for model_variation and
#'    model_variation_string
derive_model_variation <- function(df) {
  df_w_helpers <- df |>
    mutate(
      training_volume = n_history_delay + n_history_uncertainty,
      model_variation = case_when(
        training_volume != 120 & !borrow & partial_rep_tri ~ "Training volume",
        training_volume == 120 & !borrow & !partial_rep_tri ~ "Reporting triangle completeness",
        training_volume == 120 & !borrow & partial_rep_tri ~ "Baseline validation",
        training_volume == 120 & borrow & partial_rep_tri ~ "Borrow for delay and uncertainty estimation",
        .default = "Other"
      ),
      model_variation_string = dplyr::case_match(
        model_variation,
        "Training volume" ~ glue("Delay:{n_history_delay},\nUncertainty:{n_history_uncertainty}"),
        "Reporting triangle completeness" ~ "Complete reporting triangle",
        "Borrow for delay and uncertainty estimation" ~ "Borrowed estimates from all age groups",
        "Baseline validation" ~ "Baseline validation approach",
        .default = model_variation
      )
    )

  return(df_w_helpers)
}
