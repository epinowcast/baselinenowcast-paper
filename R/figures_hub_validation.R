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
#' @importFrom glue glue
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 aes ggplot labs theme_bw coord_flip
#' @importFrom ggpattern geom_col_pattern
get_plot_bar_chart_sum_scores <- function(joined_scores,
                                          strata = "age groups") {
  if (strata == "age groups") {
    joined_scores <- filter(joined_scores, age_group != "00+")
  } else if (strata == "national") {
    joined_scores <- filter(joined_scores, age_group == "00+")
  }

  scores_summary <- joined_scores |>
    summarise_scores(by = "model") |>
    select(model, overprediction, underprediction, dispersion) |>
    pivot_longer(!model)

  pattern_types <- c("stripe", "crosshatch", "circle")
  names(pattern_types) <- unique(scores_summary$name)
  scores_summary$pattern <- pattern_types[scores_summary$name]
  plot_colors <- plot_components()

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
    get_plot_theme() +
    scale_fill_manual(values = plot_colors$model_colors) +
    labs(
      x = "Model", y = "WIS",
      pattern = "Score Breakdown",
      color = "Model"
    ) +
    ggtitle(glue("Overall WIS: {strata}"))
  return(p)
}

#' Get a plot of the nowcasts over time for both KIT and baselinenowcast
#'
#' Hub validation figure A
#'
#' @param combined_nowcasts Dataframe of the combined quantiles across
#'    horizons and nowcast dates
#' @param horizon_to_plot Integer indicating which horizon to plot
#' @param facet Boolean indicating whether or not to make separate facets
#'    of each model
#' @importFrom glue glue
#' @importFrom ggplot2 aes ggplot ggtitle xlab ylab geom_line geom_ribbon
#'    facet_wrap scale_color_manual scale_fill_manual
#' @importFrom dplyr filter
#' @returns ggplot object
#' @autoglobal
get_plot_nowcasts_over_time <- function(combined_nowcasts,
                                        horizon_to_plot,
                                        facet = FALSE) {
  nc <- filter(combined_nowcasts, horizon == horizon_to_plot)
  plot_colors <- plot_components()
  p <- ggplot(nc) +
    geom_line(aes(
      x = reference_date, y = `q_0.5`,
      color = model
    )) +
    geom_ribbon(
      aes(
        x = reference_date,
        ymin = `q_0.25`,
        ymax = `q_0.75`, fill = model
      ),
      alpha = 0.3
    ) +
    geom_ribbon(
      aes(
        x = reference_date,
        ymin = `q_0.025`,
        ymax = `q_0.975`, fill = model
      ),
      alpha = 0.3
    ) +
    geom_line(aes(x = reference_date, y = observed),
      color = "red"
    ) +
    geom_line(aes(x = reference_date, y = data_as_of),
      color = "gray"
    ) +
    get_plot_theme() +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(values = plot_colors$model_colors) +
    scale_fill_manual(values = plot_colors$model_colors) +
    ggtitle(glue("Horizon: {-horizon_to_plot} days")) +
    xlab("") +
    ylab("7-day hospitalisation incidence")

  if (isTRUE(facet)) {
    p <- p + facet_wrap(~model)
  }


  return(p)
}
