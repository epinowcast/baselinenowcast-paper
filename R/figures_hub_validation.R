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
#' @param age_group_to_plot Character string indicating which age group to
#'    show in the plot, default is "00+" for all age groups.
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
                                        age_group_to_plot = "00+",
                                        facet = FALSE) {
  nc <- filter(
    combined_nowcasts, horizon == horizon_to_plot,
    age_group == age_group_to_plot
  )
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
    ggtitle(glue("Horizon: {-horizon_to_plot} days, strata: {age_group_to_plot} age group")) + # nolint
    xlab("") +
    ylab("7-day hospitalisation incidence")

  if (isTRUE(facet)) {
    p <- p + facet_wrap(~model)
  }


  return(p)
}

#' Plot the average WIS by model over time
#'
#' @param scores_summarised Dataframe of scores averaged across age groups and
#'    reference times
#'
#' @returns ggplot object
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_line aes scale_x_date scale_color_manual
#'    ggtitle xlab ylab
get_plot_wis_over_time <- function(scores_summarised) {
  plot_colors <- plot_components()
  p <- ggplot(scores_summarised) +
    geom_line(aes(
      x = nowcast_date, y = wis,
      color = model
    )) +
    get_plot_theme() +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(values = plot_colors$model_colors) +
    ggtitle(glue("WIS over time by model across all age groups and horizons")) +
    xlab("") +
    ylab("WIS")
  return(p)
}

#' Plot stacked bar chart of scores by age group colored by model and patterned
#'    by over/under prediction and dispersion
#'
#' @param scores_by_age_group Dataframe containing scores summarised across
#'    nowcast dates and reference dates, stratified by model and age group
#'
#' @returns ggplot object
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_col_pattern aes labs
#'    facet_grid theme scale_fill_manual
#'    ggtitle
get_plot_score_by_age_group <- function(scores_by_age_group) {
  plot_colors <- plot_components()

  scores_summary <- scores_by_age_group |>
    select(model, age_group, overprediction, underprediction, dispersion) |>
    pivot_longer(cols = c("overprediction", "underprediction", "dispersion"))
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
    get_plot_theme() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.placement = "outside",
      strip.background = element_rect(color = NA, fill = NA),
      legend.title = element_blank()
    ) +
    labs(
      y = "WIS", x = "",
      pattern = "Score Breakdown",
      color = "Model"
    ) +
    facet_grid(. ~ age_group, switch = "x") +
    scale_fill_manual(values = plot_colors$model_colors)
  return(p)
}

#' Plot the mean delay over time colored by age group
#'
#' @param delays_over_time Dataframe containing the average delay by nowcast
#'    date and age group, including the national data
#'
#' @returns ggplot object
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_line aes labs
#'    scale_x_date
#' @importFrom dplyr filter
get_plot_mean_delay_over_time <- function(delays_over_time) {
  plot_colors <- plot_components()
  p <- ggplot() +
    geom_line(
      data = delays_over_time |>
        filter(age_group != "00+"),
      aes(x = nowcast_date, y = mean_delay, color = age_group)
    ) +
    geom_line(
      data = delays_over_time |>
        filter(age_group == "00+"),
      aes(x = nowcast_date, y = mean_delay),
      color = "black", size = 2
    ) +
    get_plot_theme() +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    labs(
      color = "Age group",
      x = "", y = "Mean delay over time"
    )

  return(p)
}

#' Plot the delay cdf by age group and nationally
#'
#' @param avg_delays_by_age Dataframe containing the average delay cdf across
#'    nowcast dates for each age group
#'
#' @returns ggplot object
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_line aes labs
#' @importFrom dplyr filter
get_plot_of_delay_cdf_by_age <- function(avg_delays_by_age) {
  plot_colors <- plot_components()
  p <- ggplot() +
    geom_line(
      data = avg_delays_by_age |>
        filter(age_group != "00+"),
      aes(x = delay, y = cdf, color = age_group)
    ) +
    geom_line(
      data = avg_delays_by_age |>
        filter(age_group == "00+"),
      aes(x = delay, y = cdf),
      color = "black", size = 2
    ) +
    get_plot_theme() +
    labs(
      color = "Age group",
      x = "Delay", y = "Cumulative delay distribution"
    )

  return(p)
}
