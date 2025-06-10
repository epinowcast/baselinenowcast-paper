#' Get a plot of the nowcasts over time faceted by model variation, colored
#' by the type of permutation
#'
#' Hub validation figure A
#'
#' @param combined_nowcasts Dataframe of the combined quantiles across
#'    horizons and nowcast dates for all the model permutations.
#' @param horizon_to_plot Integer indicating which horizon to plot
#' @param age_group_to_plot Character string indicating which age group to
#'    show in the plot, default is "00+" for all age groups.
#' @importFrom glue glue
#' @importFrom ggplot2 aes ggplot ggtitle xlab ylab geom_line geom_ribbon
#'    facet_wrap scale_color_manual scale_fill_manual
#' @importFrom dplyr filter
#' @returns ggplot object
#' @autoglobal
get_plot_nowcast_perms_over_time <- function(combined_nowcasts,
                                             horizon_to_plot,
                                             age_group_to_plot = "00+") {
  nc <- filter(
    combined_nowcasts,
    horizon == horizon_to_plot,
    age_group == age_group_to_plot
  )

  nc_base <- nc |> filter(model_variation == "Baseline validation")

  nc_perms <- nc |>
    filter(model_variation != "Baseline validation") |>
    bind_rows(nc_base |> mutate(model_variation = "Borrow for delay and uncertainty estimation")) |>
    bind_rows(nc_base |> mutate(model_variation = "Reporting triangle completeness")) |>
    bind_rows(nc_base |> mutate(model_variation = "Training volume"))

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
get_plot_bar_chart_sum_scores_mp <- function(scores,
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
#' @inheritParams get_plot_bar_chart_sum_scores_mp
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

  rel_wis <- summarised_scores |>
    filter(model_variation_string != "Baseline validation approach") |>
    left_join(summarised_scores |>
      filter(model_variation_string == "Baseline validation approach") |> # nolint
      rename(baseline_wis = wis) |>
      select(baseline_wis, nowcast_date), by = "nowcast_date") |>
    mutate(rel_wis = wis / baseline_wis)

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
    ggtitle(glue("Relative WIS over time by model permutation across all horizons: {strata}")) +
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
}
