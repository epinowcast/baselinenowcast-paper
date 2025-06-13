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
#' @importFrom ggplot2 aes ggplot labs theme_bw coord_flip geom_bar
#'    scale_alpha_manual
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
    pivot_longer(!model) |>
    mutate(name = factor(name, levels = c(
      "overprediction",
      "dispersion",
      "underprediction"
    )))
  plot_colors <- plot_components()
  p <- ggplot(
    scores_summary,
    aes(x = model, y = value, fill = model, alpha = name)
  ) +
    geom_bar(stat = "identity", position = "stack") +
    coord_flip() +
    get_plot_theme() +
    scale_fill_manual(values = plot_colors$model_colors) +
    labs(
      x = "Model", y = "WIS",
      pattern = "Score Breakdown",
      color = "Model"
    ) +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_colors$score_alpha
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
#' @param strata Character string indicating whether scores are across age
#'    groups or nationally.
#'
#' @returns ggplot object
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_line aes scale_x_date scale_color_manual
#'    ggtitle xlab ylab
get_plot_wis_over_time <- function(scores_summarised,
                                   strata) {
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
    ggtitle(glue("WIS over time by model across all horizons: {strata}")) +
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
#' @importFrom ggplot2 ggplot aes labs
#'    facet_grid theme scale_fill_manual
#'    ggtitle element_blank scale_alpha_manual geom_bar
get_plot_score_by_age_group <- function(scores_by_age_group) {
  plot_colors <- plot_components()

  scores_summary <- scores_by_age_group |>
    select(model, age_group, overprediction, underprediction, dispersion) |>
    pivot_longer(cols = c("overprediction", "underprediction", "dispersion")) |>
    mutate(name = factor(name, levels = c(
      "overprediction",
      "dispersion",
      "underprediction"
    )))
  p <- ggplot(
    scores_summary,
    aes(x = model, y = value, fill = model, alpha = name)
  ) +
    geom_bar(
      stat = "identity",
      position = "stack"
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
      color = "Model"
    ) +
    facet_grid(. ~ age_group, switch = "x") +
    scale_fill_manual(values = plot_colors$model_colors) +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_colors$score_alpha
    )
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
#'    scale_x_date scale_color_manual scale_linewidth_manual
#'    guides
get_plot_mean_delay_over_time <- function(delays_over_time) {
  plot_comps <- plot_components()
  p <- ggplot(data = delays_over_time) +
    geom_line(aes(
      x = ymd(nowcast_date), y = mean_delay,
      color = age_group,
      linewidth = age_group
    )) +
    guides(linewidth = "none") +
    get_plot_theme() +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(
      name = "Age group",
      values = plot_comps$age_colors
    ) +
    scale_linewidth_manual(
      values = plot_comps$age_linewidth,
      labels = NULL
    ) +
    xlab("") +
    ylab("Mean delay over time")

  return(p)
}

#' Plot the delay cdf by age group and nationally
#'
#' @param avg_delays_by_age Dataframe containing the average delay cdf across
#'    nowcast dates for each age group
#'
#' @returns ggplot object
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_line aes labs guides
get_plot_of_delay_cdf_by_age <- function(avg_delays_by_age) {
  plot_comps <- plot_components()
  p <- ggplot(avg_delays_by_age) +
    geom_line(
      aes(x = delay_time, y = cdf, color = age_group, linewidth = age_group)
    ) +
    guides(linewidth = "none") +
    scale_color_manual(
      name = "Age group",
      values = plot_comps$age_colors
    ) +
    scale_linewidth_manual(
      values = plot_comps$age_linewidth,
      labels = NULL
    ) +
    xlab("Delay (days)") +
    ylab("Cumulative delay distribution") +
    get_plot_theme()


  return(p)
}

#' Get a horizontal bar chart of coverage
#'
#' @param all_coverage Dataframe of the combined individual level coverage for
#'    days and intervals.
#' @param strata Character string indicating which strata to summarize over.
#' @param intervals Vector of integers to plot coverage of.
#' @importFrom ggplot2 ggplot geom_bar aes labs scale_alpha_manual
#'    scale_fill_manual geom_hline
#' @importFrom dplyr filter group_by summarise mutate n
#' @importFrom tidyr pivot_wider pivot_longer
#' @autoglobal
#' @returns bar chart
get_plot_bar_chart_coverage <- function(all_coverage,
                                        strata,
                                        intervals = c(50, 95)) {
  if (strata == "age groups") {
    coverage <- filter(
      all_coverage, age_group != "00+",
      interval_range %in% c(intervals)
    )
  } else if (strata == "national") {
    coverage <- filter(
      all_coverage, age_group == "00+",
      interval_range %in% c(intervals)
    )
  }

  coverage_summarised <- coverage |>
    group_by(model, interval_range) |>
    summarise(empirical_coverage = sum(interval_coverage) / n()) |>
    pivot_wider(
      names_from = interval_range,
      values_from = empirical_coverage
    ) |>
    mutate(`95` = `95` - `50`) |>
    pivot_longer(!model,
      names_to = "interval_range",
      values_to = "empirical_coverage"
    ) |>
    mutate(interval_range = factor(interval_range, levels = c("95", "50")))


  plot_comps <- plot_components()
  p <- ggplot(coverage_summarised) +
    geom_bar(
      aes(
        x = model, y = empirical_coverage,
        alpha = interval_range,
        fill = model
      ),
      stat = "identity", position = "stack"
    ) +
    scale_fill_manual(
      name = "",
      values = plot_comps$model_colors
    ) +
    scale_alpha_manual(
      name = "Empirical coverage",
      values = plot_comps$coverage_alpha
    ) +
    geom_hline(aes(yintercept = 0.50), linetype = "dashed") +
    geom_hline(aes(yintercept = 0.95), linetype = "dashed") +
    labs(
      x = "Empirical coverage", y = "",
      fill = ""
    ) +
    ggtitle(glue::glue("Empirical coverage: {strata}")) +
    coord_flip() +
    get_plot_theme()

  return(p)
}

#' Get a plot of relative WIS by age group
#'
#' @param scores_by_age_group Dataframe of the summarised scores by age group
#'    and model
#' @param KIT_comparison_model Character string indicating which of the KIT
#'     models to compare to. Default is `"KIT simple nowcast"`.
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_bar aes labs scale_fill_manual
#'    geom_hline scale_y_continuous
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom glue glue
#' @returns ggplot object
get_plot_rel_wis_by_age_group <- function(
    scores_by_age_group,
    KIT_comparison_model = "KIT simple nowcast") {
  KIT_comparison <- scores_by_age_group |>
    filter(model == KIT_comparison_model) |>
    rename(comparison_wis = wis) |>
    select(comparison_wis, age_group)

  rel_wis <- scores_by_age_group |>
    filter(model != KIT_comparison_model) |>
    left_join(KIT_comparison, by = "age_group") |>
    mutate(rel_wis = wis / pmax(comparison_wis, .Machine$double.eps))

  plot_comps <- plot_components()
  p <- ggplot(rel_wis) +
    geom_point(aes(
      x = age_group, y = rel_wis,
      fill = model
    )) +
    scale_fill_manual(
      name = "",
      values = plot_comps$model_colors
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    scale_y_continuous(trans = "log", limits = c(0.66, 1.5)) +
    coord_flip() +
    get_plot_theme() +
    labs(x = "", y = "Relative WIS") +
    ggtitle(glue::glue("Relative WIS by age group relative to {KIT_comparison_model}")) # nolint
  return(p)
}
#' Get a plot of mean WIS by horizon
#'
#' @param scores Dataframe of all the scores by individual reference and
#'    nowcast dates and model and age groups
#' @inheritParams get_plot_bar_chart_sum_scores
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_line aes labs scale_color_manual
#' @importFrom dplyr select filter
#' @importFrom scoringutils summarise_scores
#' @importFrom glue glue
#' @returns ggplot object
get_plot_mean_wis_by_horizon <- function(scores,
                                         strata) {
  if (strata == "age groups") {
    scores_filtered <- filter(
      scores, age_group != "00+"
    )
  } else if (strata == "national") {
    scores_filtered <- filter(
      scores, age_group == "00+"
    )
  }

  scores_sum <- scores_filtered |>
    mutate(horizon = as.integer(reference_date - nowcast_date)) |>
    scoringutils::summarise_scores(by = c("model", "horizon"))
  plot_comps <- plot_components()
  p <- ggplot(scores_sum, aes(x = horizon, y = wis, color = model)) +
    geom_line() +
    scale_color_manual(
      name = "",
      values = plot_comps$model_colors
    ) +
    get_plot_theme() +
    labs(x = "Horizon (days)", y = "Mean WIS") +
    ggtitle(glue::glue("Mean WIS by horizon: {strata}"))

  return(p)
}

#' Get a plot of relative WIS by horizon
#'
#' @param scores Dataframe of all the scores by individual reference and
#'    nowcast dates and model and age groups
#' @param KIT_comparison_model Character string indicating which model to
#'   compare to.
#' @inheritParams get_plot_bar_chart_sum_scores
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_line aes labs scale_color_manual
#' @importFrom dplyr select filter
#' @importFrom scoringutils summarise_scores
#' @importFrom glue glue
#' @returns ggplot object
get_plot_rel_wis_by_horizon <- function(
    scores,
    strata,
    KIT_comparison_model = "KIT simple nowcast") {
  if (strata == "age groups") {
    scores_filtered <- filter(
      scores, age_group != "00+"
    )
  } else if (strata == "national") {
    scores_filtered <- filter(
      scores, age_group == "00+"
    )
  }

  scores_sum <- scores_filtered |>
    mutate(horizon = as.integer(reference_date - nowcast_date)) |>
    scoringutils::summarise_scores(by = c("model", "horizon")) |>
    select(model, horizon, wis)

  KIT_comparison <- scores_sum |>
    filter(model == KIT_comparison_model) |>
    rename(comparison_wis = wis) |>
    select(comparison_wis, horizon)

  relative_wis <- scores_sum |>
    filter(model != KIT_comparison_model) |>
    left_join(KIT_comparison, by = "horizon") |>
    mutate(rel_wis = wis / pmax(comparison_wis, .Machine$double.eps))
  plot_comps <- plot_components()
  p <- ggplot(relative_wis) +
    geom_line(aes(x = horizon, y = rel_wis, color = model)) +
    get_plot_theme() +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    scale_y_continuous(trans = "log") +
    scale_color_manual(
      name = "",
      values = plot_comps$model_colors
    ) +
    labs(x = "Horizon (days)", y = "Relative WIS") +
    ggtitle(glue::glue("Relative WIS by horizon: {strata} relative to {KIT_comparison_model}")) # nolint

  return(p)
}

#' Get a plot of coverage by horizon for each model
#'
#' @param all_coverage Dataframe of the combined individual level coverage for
#'    days and intervals.
#' @param strata Character string indicating which strata to summarize over.
#' @param intervals Vector of integers to plot coverage of.
#' @importFrom ggplot2 ggplot geom_bar aes labs scale_alpha_manual
#'    scale_fill_manual geom_hline
#' @importFrom dplyr filter group_by summarise mutate n
#' @importFrom tidyr pivot_wider pivot_longer
#' @autoglobal
#' @returns bar chart
get_plot_coverage_by_horizon <- function(all_coverage,
                                         strata,
                                         intervals = c(50, 95)) {
  if (strata == "age groups") {
    coverage <- filter(
      all_coverage, age_group != "00+",
      interval_range %in% c(intervals)
    )
  } else if (strata == "national") {
    coverage <- filter(
      all_coverage, age_group == "00+",
      interval_range %in% c(intervals)
    )
  }

  coverage_summarised <- coverage |>
    mutate(horizon = as.integer(reference_date - nowcast_date)) |>
    group_by(model, interval_range, horizon) |>
    summarise(empirical_coverage = sum(interval_coverage) / n()) |>
    pivot_wider(
      names_from = interval_range,
      values_from = empirical_coverage
    ) |>
    pivot_longer(
      cols = c(`50`, `95`),
      names_to = "interval_range",
      values_to = "empirical_coverage"
    ) |>
    mutate(
      int_cont = as.numeric(interval_range) / 100,
      interval_range = factor(interval_range, levels = c("95", "50"))
    )


  plot_comps <- plot_components()
  p <- ggplot(coverage_summarised) +
    geom_line(
      aes(
        x = horizon, y = empirical_coverage,
        color = model
      )
    ) +
    geom_hline(aes(yintercept = int_cont), linetype = "dashed") +
    scale_color_manual(
      name = "",
      values = plot_comps$model_colors
    ) +
    facet_wrap(~interval_range) +
    labs(
      x = "Horizon(days)", y = "Empirical coverage",
      color = "Model"
    ) +
    ggtitle(glue::glue("Empirical coverage by horizon: {strata}")) +
    get_plot_theme()

  return(p)
}

#' Get a plot of coverage by age_group for each model
#'
#' @param all_coverage Dataframe of the combined individual level coverage for
#'    days and intervals.
#' @param intervals Vector of integers to plot coverage of.
#' @importFrom ggplot2 ggplot geom_bar aes labs scale_alpha_manual
#'    scale_fill_manual geom_hline
#' @importFrom dplyr filter group_by summarise mutate n
#' @importFrom tidyr pivot_wider pivot_longer
#' @autoglobal
#' @returns bar chart
get_plot_coverage_by_age_group <- function(all_coverage,
                                           intervals = c(50, 95)) {
  coverage <- filter(
    all_coverage, age_group != "00+",
    interval_range %in% c(intervals)
  )

  coverage_summarised <- coverage |>
    group_by(model, interval_range, age_group) |>
    summarise(empirical_coverage = sum(interval_coverage) / n()) |>
    pivot_wider(
      names_from = interval_range,
      values_from = empirical_coverage
    ) |>
    mutate(`95` = `95` - `50`) |>
    pivot_longer(
      cols = c(`50`, `95`),
      names_to = "interval_range",
      values_to = "empirical_coverage"
    ) |>
    mutate(interval_range = factor(interval_range, levels = c("95", "50")))


  plot_comps <- plot_components()
  p <- ggplot(coverage_summarised) +
    geom_bar(
      aes(
        x = model, y = empirical_coverage,
        alpha = interval_range,
        fill = model
      ),
      stat = "identity", position = "stack"
    ) +
    scale_fill_manual(
      name = "",
      values = plot_comps$model_colors
    ) +
    facet_grid(. ~ age_group, switch = "x") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.placement = "outside",
      strip.background = element_rect(color = NA, fill = NA),
      legend.title = element_blank()
    ) +
    scale_alpha_manual(
      name = "Empirical coverage",
      values = plot_comps$coverage_alpha
    ) +
    geom_hline(aes(yintercept = 0.50), linetype = "dashed") +
    geom_hline(aes(yintercept = 0.95), linetype = "dashed") +
    labs(
      x = "Empirical coverage", y = "",
      fill = ""
    ) +
    ggtitle("Empirical coverage by age group") +
    coord_flip() +
    get_plot_theme()


  return(p)
}

#' Title
#'
#' @param plot_nowcasts_over_time A
#' @param horiz_bar_chart_sum_scores_ag B
#' @param plot_wis_comp_over_time_ag C
#' @param bar_chart_scores_by_age_group D
#' @param plot_mean_delay_over_time_by_age E
#' @param plot_mean_cdf_delay_by_age F
#' @param fig_file_name Character string indicating name of the figure to be
#'    saved as the file name
#' @param fig_file_dir Path to save figure. Default is
#'    `file.path("output", "figs")`.
#' @param save Boolean indicating whether or not to save the figure to disk.
#'    Default is `TRUE`.
#' @autoglobal
#' @importFrom glue glue
#' @importFrom patchwork plot_layout
#' @importFrom ggplot ggsave theme
#' @importFrom fs dir_create
#' @returns ggplot object as a gridded panel
make_fig_hub_validation <- function(
    plot_nowcasts_over_time,
    horiz_bar_chart_sum_scores_ag,
    plot_wis_comp_over_time_ag,
    bar_chart_scores_by_age_group,
    plot_mean_delay_over_time_by_age,
    plot_mean_cdf_delay_by_age,
    fig_file_name,
    fig_file_dir = file.path("output", "figs"),
    save = TRUE) {
  layout <- "
  AAAABB
  CCCCDD
  EEEEFF
  "

  fig_hub_validation <- plot_nowcasts_over_time +
    horiz_bar_chart_sum_scores_ag +
    plot_wis_comp_over_time_ag +
    bar_chart_scores_by_age_group +
    plot_mean_delay_over_time_by_age +
    plot_mean_cdf_delay_by_age +
    plot_layout(
      design = layout,
      axes = "collect"
    ) & theme(
    legend.position = "top",
    legend.justification = "left"
  )

  dir_create(fig_file_dir)

  ggsave(
    fig_hub_validation,
    filename = file.path(
      fig_file_dir,
      glue("{fig_filename}.png")
    ),
    width = 10,
    height = 8
  )
  return(fig_hub_validation)
}
