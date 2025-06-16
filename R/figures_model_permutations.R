#' Get a plot of the nowcasts over time faceted by model variation, colored
#' by the type of permutation
#'
#' Hub validation figure A
#'
#' @param combined_nowcasts Dataframe of the combined quantiles across
#'    horizons and nowcast dates for all the model permutations.
#' @param horizon_to_plot Integer indicating which horizon to plot
#' @param age_group_to_plot Character string indicating which age group to
#'    show in the plot. Default is `"00+"`for all age groups.
#' @importFrom glue glue
#' @importFrom ggplot2 aes ggplot ggtitle xlab ylab geom_line geom_ribbon
#'    facet_wrap scale_color_manual scale_fill_manual
#' @importFrom dplyr filter
#' @returns ggplot object
#' @autoglobal
get_plot_nowcasts_over_time_mp <- function(combined_nowcasts,
                                           horizon_to_plot,
                                           age_group_to_plot = "00+") {
  nc <- filter(
    combined_nowcasts,
    horizon == horizon_to_plot,
    age_group == age_group_to_plot
  )

  nc_base <- filter(nc, model_variation == "Baseline validation")

  nc_perms <- filter(nc, model_variation != "Baseline validation") |>
    bind_rows(mutate(nc_base, model_variation = "Borrow for delay and uncertainty estimation")) |> # nolint
    bind_rows(mutate(nc_base, model_variation = "Reporting triangle completeness")) |> # nolint
    bind_rows(mutate(nc_base, model_variation = "Training volume"))

  plot_colors <- plot_components()
  p <- ggplot(nc_perms) +
    geom_line(aes(
      x = reference_date, y = `q_0.5`,
      color = model_variation_string
    ), show.legend = FALSE) +
    geom_ribbon(
      aes(
        x = reference_date,
        ymin = `q_0.25`,
        ymax = `q_0.75`,
        fill = model_variation_string
      ),
      alpha = 0.3
    ) +
    geom_ribbon(
      aes(
        x = reference_date,
        ymin = `q_0.025`,
        ymax = `q_0.975`, fill = model_variation_string
      ),
      alpha = 0.3
    ) +
    geom_line(aes(x = reference_date, y = observed),
      color = "red"
    ) +
    geom_line(aes(x = reference_date, y = data_as_of),
      color = "gray", linetype = "dashed"
    ) +
    facet_wrap(~model_variation, nrow = 3) +
    get_plot_theme() +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(values = plot_colors$permutation_colors) +
    scale_fill_manual(values = plot_colors$permutation_colors) +
    labs(fill = "Model permutation") +
    ggtitle(glue("Horizon: {-horizon_to_plot} days, strata: {age_group_to_plot} age group")) + # nolint
    xlab("") +
    ylab("7-day hospitalisation incidence")
  return(p)
}
#' Get horizontal bar chart of summarised scores by model permutation and score
#'    component
#'
#' @param scores Data.frame of model permutations for all nowcast dates,
#'    horizons, and age groups.
#' @param strata Character string indicating whether to summarise across the
#'    different age strata (`"age groups"`) or across the nation as a whole
#'    (`"national"`). Default is `"age groups"`.
#' @autoglobal
#' @returns ggplot object
#' @importFrom dplyr filter select mutate
#' @importFrom glue glue
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 aes ggplot labs theme coord_flip geom_bar
#'    scale_alpha_manual facet_grid scale_fill_manual xlab ylab
get_plot_bar_chart_scores_mp <- function(scores,
                                         strata = "age groups") {
  if (strata == "age groups") {
    scores <- filter(scores, age_group != "00+")
  } else if (strata == "national") {
    scores <- filter(scores, age_group == "00+")
  }
  plot_colors <- plot_components()

  scores_summary <- scores |>
    summarise_scores(by = c("model_variation_string", "model_variation")) |>
    select(
      model_variation,
      model_variation_string, overprediction,
      underprediction, dispersion
    ) |>
    pivot_longer(cols = c("overprediction", "underprediction", "dispersion")) |>
    mutate(name = factor(name, levels = c(
      "overprediction",
      "dispersion",
      "underprediction"
    )))

  p <- ggplot(
    scores_summary,
    aes(
      x = model_variation_string,
      y = value, fill = model_variation_string, alpha = name
    )
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
      color = "Model permutation"
    ) +
    facet_grid(. ~ model_variation, switch = "x") +
    scale_fill_manual(values = plot_colors$permutation_colors) +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_colors$score_alpha
    ) +
    ggtitle(glue("Overall WIS by model permutation: {strata}"))
  return(p)
}

#' Get a line plot of the relative wis over time for each model permutation
#'
#' @inheritParams get_plot_bar_chart_scores_mp
#' @autoglobal
#' @returns ggplot object
#' @importFrom dplyr filter mutate left_join rename select
#' @importFrom glue glue
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 aes ggplot labs theme_bw coord_flip geom_line
#'    scale_color_manual geom_hline xlab ylab ggtitle scale_x_date
get_plot_rel_wis_over_time_mp <- function(scores,
                                          strata) {
  plot_colors <- plot_components()
  if (strata == "age groups") {
    scores <- filter(scores, age_group != "00+")
  } else if (strata == "national") {
    scores <- filter(scores, age_group == "00+")
  }

  summarised_scores <- scores |>
    summarise_scores(by = c(
      "model_variation_string",
      "model_variation",
      "nowcast_date"
    )) |>
    select(wis, model_variation_string, model_variation, nowcast_date)

  baseline_score <- summarised_scores |>
    filter(model_variation_string == "Baseline validation approach") |>
    rename(baseline_wis = wis) |>
    select(baseline_wis, nowcast_date)
  rel_wis <- summarised_scores |>
    filter(model_variation_string != "Baseline validation approach") |>
    left_join(baseline_score, by = "nowcast_date") |>
    mutate(rel_wis = wis / pmax(baseline_wis, .Machine$double.eps))

  p <- ggplot(rel_wis) +
    geom_line(aes(
      x = nowcast_date, y = rel_wis,
      color = model_variation_string,
      linetype = model_variation
    )) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    get_plot_theme() +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(values = plot_colors$permutation_colors) +
    labs(color = "Model permutation", linetype = "Permutation grouping") +
    ggtitle(glue("Relative WIS over time by model permutation across all horizons: {strata}")) + # nolint
    xlab("") +
    ylab("Relative WIS compared\nto baseline validation approach")
  return(p)
}

#' Get a plot of coverage by model permutation for each model
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
get_plot_coverage_by_mp <- function(all_coverage,
                                    intervals = c(50, 95)) {
  coverage <- filter(
    all_coverage, age_group != "00+",
    interval_range %in% c(intervals)
  )

  coverage_summarised <- coverage |>
    group_by(model, interval_range, model_variation, model_variation_string) |>
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
        x = model_variation_string, y = empirical_coverage,
        alpha = interval_range,
        fill = model_variation_string
      ),
      stat = "identity", position = "stack"
    ) +
    facet_grid(. ~ model_variation, switch = "x") +
    get_plot_theme() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.placement = "outside",
      strip.background = element_rect(color = NA, fill = NA),
      legend.title = element_blank()
    ) +
    scale_fill_manual(
      name = "",
      values = plot_comps$permutation_colors
    ) +
    scale_alpha_manual(
      name = "Empirical coverage",
      values = plot_comps$coverage_alpha
    ) +
    geom_hline(aes(yintercept = 0.50), linetype = "dashed") +
    geom_hline(aes(yintercept = 0.95), linetype = "dashed") +
    labs(
      y = "Empirical coverage", x = "",
      fill = ""
    ) +
    ggtitle("Empirical coverage by model permutation across age groups")
  return(p)
}

#' Get a plot of relative WIS by horizon for all model permutations
#'
#' @param scores Dataframe of all the scores by individual reference and
#'    nowcast dates and model permutation
#' @inheritParams get_plot_bar_chart_scores_mp
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_line aes labs scale_color_manual
#' @importFrom dplyr select filter
#' @importFrom scoringutils summarise_scores
#' @importFrom glue glue
#' @returns ggplot object
get_plot_rel_wis_by_horizon_mp <- function(scores,
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
    scoringutils::summarise_scores(by = c(
      "model_variation",
      "model_variation_string",
      "horizon"
    )) |>
    select(model_variation, model_variation_string, horizon, wis)
  baseline_scores <- scores_sum |>
    filter(model_variation_string == "Baseline validation approach") |>
    rename(baseline_wis = wis) |>
    select(baseline_wis, horizon)
  rel_wis <- scores_sum |>
    filter(model_variation_string != "Baseline validation approach") |>
    left_join(baseline_scores, by = "horizon") |>
    mutate(relative_wis = wis / pmax(baseline_wis, .Machine$double.eps))

  plot_comps <- plot_components()
  p <- ggplot(rel_wis, aes(
    x = horizon, y = relative_wis,
    color = model_variation_string,
    linetype = model_variation
  )) +
    geom_line() +
    get_plot_theme() +
    scale_color_manual(
      name = "Model permutation",
      values = plot_comps$permutation_colors
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    scale_y_continuous(trans = "log10", limits = c(0.6, 2.5)) +
    labs(
      x = "Horizon (days)",
      y = "Relative WIS compared\nto baseline validation approach",
      linetype = "Permutation grouping"
    ) +
    ggtitle(glue::glue("Relative WIS by horizon for all model permutations: {strata}")) # nolint

  return(p)
}

#' Get a plot of relative decomposed WIS by age group
#'
#' @param scores Dataframe of all the scores by individual reference and
#'    nowcast dates and model permutation
#' @param facet Boolean indicating whether or not to create separate facets for
#'    each WIS component. Default is `FALSE`
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_line aes labs scale_color_manual
#' @importFrom dplyr select filter
#' @importFrom scoringutils summarise_scores
#' @importFrom glue glue
#' @returns ggplot object
get_plot_rel_decomposed_wis <- function(scores,
                                        facet = FALSE) {
  scores_sum <- scores |>
    scoringutils::summarise_scores(by = c(
      "model_variation",
      "model_variation_string",
      "age_group"
    )) |>
    select(
      model_variation, model_variation_string, age_group, wis,
      underprediction, overprediction, dispersion
    )
  baseline_scores <- scores_sum |>
    filter(model_variation_string == "Baseline validation approach") |>
    rename(
      baseline_wis = wis,
      baseline_underprediction = underprediction,
      baseline_overprediction = overprediction,
      baseline_dispersion = dispersion
    ) |>
    select(
      age_group, baseline_wis, baseline_underprediction,
      baseline_overprediction, baseline_dispersion
    )
  rel_wis <- scores_sum |>
    filter(model_variation_string != "Baseline validation approach") |>
    left_join(baseline_scores, by = "age_group") |>
    mutate(
      relative_wis = wis / pmax(baseline_wis, .Machine$double.eps),
      relative_underprediction = underprediction / pmax(
        baseline_underprediction, .Machine$double.eps
      ),
      relative_overprediction = overprediction / pmax(
        baseline_overprediction, .Machine$double.eps
      ),
      relative_dispersion = dispersion / pmax(
        baseline_dispersion, .Machine$double.eps
      )
    ) |>
    select(
      age_group, model_variation_string,
      relative_underprediction, relative_overprediction,
      relative_dispersion
    ) |>
    pivot_longer(
      cols = starts_with("relative_"),
      names_prefix = "relative_",
      names_to = "component",
      values_to = "rel_score"
    )

  plot_comps <- plot_components()
  p <- ggplot(rel_wis, aes(
    x = age_group, y = rel_score,
    fill = model_variation_string,
    alpha = component
  )) +
    geom_bar(stat = "identity", position = "dodge") +
    get_plot_theme() +
    scale_fill_manual(
      name = "Model permutation",
      values = plot_comps$permutation_colors
    ) +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_comps$score_alpha
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    scale_y_continuous(trans = "log10") +
    labs(
      x = "",
      y = "Relative WIS compared\nto baseline validation approach"
    ) +
    ggtitle(glue::glue("Relative WIS components by age group for all model permutations")) # nolint

  if (isTRUE(facet)) {
    p <- p + facet_wrap(~component, nrow = 3)
  }

  return(p)
}




#' Get a plot of decomposed WIS by age group for each model permutation
#'
#' @param scores_by_age_group Dataframe of the scores by age group
#'    and model
#' @inheritParams get_plot_rel_wis_by_age_group
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_bar aes labs scale_fill_manual
#'    geom_hline scale_y_continuous
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom fs dir_create
#' @returns ggplot object
get_plot_wis_by_age_group_mp <- function(
    scores_by_age_group,
    fig_file_name = NULL,
    fig_file_dir = file.path("output", "figs", "supp"),
    save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }
  scores_sum <- scores_by_age_group |>
    scoringutils::summarise_scores(by = c(
      "model_variation",
      "model_variation_string",
      "age_group"
    )) |>
    select(
      model_variation, model_variation_string,
      age_group, overprediction, underprediction, dispersion
    ) |>
    pivot_longer(cols = c(
      "overprediction",
      "underprediction",
      "dispersion"
    )) |>
    mutate(name = factor(name, levels = c(
      "overprediction",
      "dispersion",
      "underprediction"
    )))

  plot_comps <- plot_components()
  p <- ggplot(
    scores_sum,
    aes(
      x = model_variation_string, y = value,
      alpha = name,
      fill = model_variation_string
    )
  ) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(
      name = "Model permutation",
      values = plot_comps$permutation_colors
    ) +
    get_plot_theme() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.placement = "outside",
      strip.background = element_rect(color = NA, fill = NA),
      legend.position = "bottom"
    ) +
    facet_grid(. ~ age_group, switch = "x") +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_comps$score_alpha
    ) +
    labs(x = "", y = "WIS") +
    ggtitle("WIS by age group for all model permutations")
  if (isTRUE(save)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 16,
      height = 8
    )
  }
  return(p)
}

#' Get a plot of decomposed WIS by horizon for each model permutation
#'
#' @param scores Dataframe of the scores by age group, horizon, and model
#' @param strata Character string indicating whether to summarise across the
#'    different age strata (`"age groups"`) or across the nation as a whole
#'    (`"national"`). Default is `"age groups"`.
#' @inheritParams get_plot_rel_wis_by_age_group
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_bar aes labs scale_fill_manual
#'    geom_hline scale_y_continuous
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom glue glue
#' @importFrom fs dir_create
#' @returns ggplot object
get_plot_wis_by_horizon_mp <- function(
    scores,
    strata = "age groups",
    fig_file_name = NULL,
    fig_file_dir = file.path("output", "figs", "supp"),
    save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }
  if (strata == "age groups") {
    scores_filtered <- filter(scores, age_group != "00+")
  } else if (strata == "national") {
    scores_filtered <- filter(scores, age_group == "00+")
  }
  scores_sum <- scores_filtered |>
    scoringutils::summarise_scores(by = c(
      "model_variation",
      "model_variation_string",
      "horizon"
    )) |>
    select(
      model_variation, model_variation_string,
      horizon, overprediction, underprediction, dispersion
    ) |>
    pivot_longer(cols = c(
      "overprediction",
      "underprediction",
      "dispersion"
    )) |>
    mutate(name = factor(name, levels = c(
      "overprediction",
      "dispersion",
      "underprediction"
    )))

  plot_comps <- plot_components()
  p <- ggplot(
    scores_sum,
    aes(
      x = model_variation_string, y = value,
      fill = model_variation_string,
      alpha = name
    )
  ) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(
      name = "Model permutation",
      values = plot_comps$permutation_colors
    ) +
    get_plot_theme() +
    cowplot::background_grid(
      minor = "none",
      major = "none"
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.placement = "outside",
      strip.background = element_rect(color = NA, fill = NA),
      legend.position = "bottom"
    ) +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_comps$score_alpha
    ) +
    facet_grid(. ~ horizon, switch = "x") +
    labs(x = "Horizon (days)", y = "WIS breakdown") +
    ggtitle(glue::glue("WIS breakdown by horizon for all model permutations: {strata}")) # nolint
  if (isTRUE(save)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 16,
      height = 8
    )
  }
  return(p)
}


#' Get a plot of decomposed WIS by week for each model permutation
#'
#' @param scores Dataframe of the scores by age group, horizon, and model
#' @param strata Character string indicating whether to summarise across the
#'    different age strata (`"age groups"`) or across the nation as a whole
#'    (`"national"`). Default is `"age groups"`.
#' @inheritParams get_plot_rel_wis_by_age_group
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_bar aes labs scale_fill_manual
#'    geom_hline scale_y_continuous
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate isoweek
#' @importFrom glue glue
#' @importFrom fs dir_create
#' @returns ggplot object
get_plot_wis_by_week_mp <- function(
    scores,
    strata = "age groups",
    fig_file_name = NULL,
    fig_file_dir = file.path("output", "figs", "supp"),
    save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }
  if (strata == "age groups") {
    scores_filtered <- filter(scores, age_group != "00+")
  } else if (strata == "national") {
    scores_filtered <- filter(scores, age_group == "00+")
  }
  metrics_attr <- attr(scores_filtered, "metrics")
  scores_sum <- scores_filtered |>
    mutate(week = isoweek(nowcast_date)) |>
    group_by(week) |>
    mutate(week_end_date = max(nowcast_date)) |>
    ungroup() |>
    # nolint start
    # Restore the metrics attribute before summarising
    {
      \(x) {
        attr(x, "metrics") <- metrics_attr
        return(x)
      }
    }() |>
    # nolint end
    scoringutils::summarise_scores(by = c(
      "model_variation",
      "model_variation_string",
      "week_end_date"
    )) |>
    select(
      model_variation, model_variation_string,
      week_end_date, overprediction, underprediction, dispersion
    ) |>
    pivot_longer(cols = c(
      "overprediction",
      "underprediction",
      "dispersion"
    )) |>
    mutate(name = factor(name, levels = c(
      "overprediction",
      "dispersion",
      "underprediction"
    )))

  plot_comps <- plot_components()
  p <- ggplot(
    scores_sum,
    aes(
      x = model_variation_string, y = value,
      fill = model_variation_string,
      alpha = name
    )
  ) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(
      name = "Model permutation",
      values = plot_comps$permutation_colors
    ) +
    get_plot_theme() +
    cowplot::background_grid(
      minor = "none",
      major = "none"
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.placement = "outside",
      strip.text.x = element_text(angle = 45),
      strip.background = element_rect(color = NA, fill = NA),
      legend.position = "bottom"
    ) +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_comps$score_alpha
    ) +
    facet_grid(. ~ week_end_date, switch = "x") +
    labs(x = "Nowcast date", y = "WIS breakdown") +
    ggtitle(glue::glue("WIS breakdown by nowcast date for all model permutations: {strata}")) # nolint
  if (isTRUE(save)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 16,
      height = 8
    )
  }
  return(p)
}

#' Title
#'
#' @param plot_nowcasts_over_time_mp A
#' @param bar_chart_wis_by_mp B
#' @param rel_wis_over_time_mp C
#' @param bar_chart_coverage_mp D
#' @param rel_wis_by_horizon_mp E
#' @param rel_decomp_wis_by_age_group F
#' @param fig_file_name Character string indicating name of the figure to be
#'    saved as the file name
#' @param fig_file_dir Path to save figure. Default is
#'    `file.path("output", "figs")`.
#' @param save Boolean indicating whether or not to save the figure to disk.
#'    Default is `TRUE`.
#' @autoglobal
#' @importFrom glue glue
#' @importFrom patchwork plot_layout
#' @importFrom ggplot2 ggsave theme
#' @importFrom fs dir_create
#' @returns ggplot object as a gridded panel
make_fig_model_perms <- function(
    plot_nowcasts_over_time_mp,
    bar_chart_wis_by_mp,
    rel_wis_over_time_mp,
    bar_chart_coverage_mp,
    rel_wis_by_horizon_mp,
    rel_decomp_wis_by_age_group,
    fig_file_name,
    fig_file_dir = file.path("output", "figs"),
    save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }
  fig_layout <- "
  AAABB
  AAADD
  AAAEE
  CCCFF
  "

  fig_model_perm <- plot_nowcasts_over_time_mp +
    bar_chart_wis_by_mp +
    rel_wis_over_time_mp +
    bar_chart_coverage_mp +
    rel_wis_by_horizon_mp +
    rel_decomp_wis_by_age_group +
    plot_layout(
      design = fig_layout,
      axes = "collect",
      guides = "collect"
    ) & theme(
    legend.position = "top",
    legend.justification = "left"
  )

  dir_create(fig_file_dir)

  if (isTRUE(save)) {
    ggsave(
      plot = fig_model_perm,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 24,
      height = 16
    )
  }

  return(fig_model_perm)
}
