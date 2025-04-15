#' Get horizontal bar chart of summarised scores by model and score component
#'
#' @param joined_scores Data.frame of a single model specification of
#'    baselinenowcast + a comparison model, with both models spanning the same
#'    nowcast dates and age groups
#' @param strata Character string indicating whether to summarise across the
#'    different age strata (`"age groups"`) or across the nation as a whole
#'    (`"national"`). Default is `"age groups"`.
#' @autoglobal
#' @returns ggplot object
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes ggplot labs theme_bw coord_flip
#' @importFrom ggpattern geom_col_pattern
get_plot_bar_chart_sum_scores <- function(joined_scores,
                                          strata = "age groups") {
  if (strata == "age groups") {
    joined_scores <- joined_scores |>
      filter(age_group != "00+")
  } else if (strata == "national") {
    joined_scores <- joined_scores |>
      filter(age_group == "00+")
  }

  scores_summary <- joined_scores |>
    summarise_scores(by = "model") |>
    select(model, overprediction, underprediction, dispersion) |>
    pivot_longer(!model)

  pattern_types <- c("stripe", "crosshatch", "circle")
  names(pattern_types) <- unique(scores_summary$name)
  scores_summary$pattern <- pattern_types[scores_summary$name]

  p <- ggplot(
    scores_summary,
    aes(x = model, y = value, fill = model, pattern = pattern)
  ) +
    geom_col_pattern(
      position = "stack",
      color = "black",
      pattern_fill = "black",
      pattern_angle = 45,
      pattern_density = 0.1,
      pattern_spacing = 0.05
    ) +
    coord_flip() +
    labs(x = "Model", y = "WIS", pattern = "Score Breakdown", color = "Model") +
    theme_bw()
  return(p)
}
