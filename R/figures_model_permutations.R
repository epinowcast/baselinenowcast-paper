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
#' @param permutation_grouping Character string indicating which model
#'    permutation grouping to plot. Default is `NULL` for all of them.
#' @inheritParams get_plot_rel_wis_by_age_group
#' @importFrom glue glue
#' @importFrom ggplot2 aes ggplot ggtitle xlab ylab geom_line geom_ribbon
#'    facet_wrap scale_color_manual scale_fill_manual guide_legend
#'    scale_linetype_manual
#' @importFrom dplyr filter
#' @returns ggplot object
#' @autoglobal
get_plot_nowcasts_over_time_mp <- function(combined_nowcasts,
                                           horizon_to_plot,
                                           age_group_to_plot = "00+",
                                           permutation_grouping = NULL,
                                           fig_file_name = NULL,
                                           fig_file_dir = file.path(
                                             "output",
                                             "figs",
                                             "supp"
                                           ),
                                           save = TRUE) {
  if (isTRUE(save) && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }
  nc <- filter(
    combined_nowcasts,
    horizon == horizon_to_plot,
    age_group == age_group_to_plot
  )


  nc_base <- filter(nc, model_variation == "Default")

  if (is.null(permutation_grouping)) {
    nc_perms <- filter(nc, model_variation != "Default") |>
      bind_rows(mutate(nc_base, model_variation = "Borrow for delay and uncertainty estimation")) |> # nolint
      bind_rows(mutate(nc_base, model_variation = "Reporting triangle completeness")) |> # nolint
      bind_rows(mutate(nc_base, model_variation = "Training volume"))
  } else {
    nc_perms <- filter(nc, model_variation == {{ permutation_grouping }}) |>
      bind_rows(mutate(nc_base, model_variation = {{ permutation_grouping }}))
  }

  nc_perms <- nc_perms |>
    mutate(facet_title = glue::glue("{age_group}: {model_variation}"))

  plot_colors <- plot_components()
  p <- ggplot(nc_perms) +
    geom_ribbon(
      aes(
        x = reference_date,
        ymin = `q_0.25`,
        ymax = `q_0.75`,
        fill = model_variation_string,
        alpha = "50%"
      )
    ) +
    geom_ribbon(
      aes(
        x = reference_date,
        ymin = `q_0.025`,
        ymax = `q_0.975`,
        fill = model_variation_string,
        alpha = "95%"
      )
    ) +
    geom_line(
      aes(
        x = reference_date, y = `q_0.5`,
        color = model_variation_string
      ),
      show.legend = FALSE,
      linewidth = 1
    ) +
    geom_line(
      aes(
        x = reference_date, y = observed,
        linetype = "Final evaluation data"
      ),
      color = "red",
      linewidth = 1
    ) +
    geom_line(
      aes(
        x = reference_date, y = data_as_of,
        linetype = "Data as of nowcast date"
      ),
      color = "gray",
      linewidth = 1
    ) +
    scale_alpha_manual(
      name = "Prediction intervals",
      values = c(
        "95%" = 0.2,
        "50%" = 0.4
      ),
      guide = guide_legend(
        nrow = 2,
        title.position = "top",
        override.aes = list(
          alpha = c(
            "95%" = 0.2,
            "50%" = 0.4
          )
        )
      )
    ) +
    facet_wrap(~facet_title, nrow = 3) +
    get_plot_theme() +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(values = plot_colors$permutation_colors) +
    scale_fill_manual(values = plot_colors$permutation_colors) +
    scale_linetype_manual(
      name = "Observed data",
      values = c(
        "Final evaluation data" = "solid",
        "Data as of nowcast date" = "solid"
      ),
      guide = guide_legend(
        title.position = "top",
        nrow = 3,
        override.aes = list(
          color = c(
            "Final evaluation data" = "red",
            "Data as of nowcast date" = "gray"
          ),
          linewidth = 1
        )
      )
    ) +
    xlab("") +
    ylab("7-day hospitalisation\nincidence") +
    guides(
      color = "none",
      fill = "none"
    ) +
    theme(
      strip.placement = "outside",
      strip.background = element_rect(color = NA, fill = NA)
    )

  if (isTRUE(save)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 20,
      height = 8
    )
  }

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
#'    scale_alpha_manual facet_grid scale_fill_manual xlab ylab guide_legend
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
    pivot_longer(cols = c(
      "overprediction", "underprediction",
      "dispersion"
    )) |>
    mutate(
      name = factor(name, levels = c(
        "overprediction",
        "dispersion",
        "underprediction"
      )),
      model_variation =
        case_when(
          model_variation == "Borrow for delay and uncertainty estimation" ~
            "Borrow for delay and\nuncertainty estimation",
          model_variation == "Reporting triangle completeness" ~
            "Reporting triangle\ncompleteness",
          model_variation == "Weekday filter" ~
            "Weekday\nfilter",
          TRUE ~ model_variation
        )
    )
  scores_base <- filter(
    scores_summary,
    model_variation == "Default"
  )
  # Make Default be alongside all the others
  scores_perms <- filter(
    scores_summary,
    model_variation != "Default"
  ) |>
    bind_rows(mutate(scores_base,
      model_variation = "Borrow for delay and\nuncertainty estimation"
    )) |> # nolint
    bind_rows(mutate(scores_base,
      model_variation = "Reporting triangle\ncompleteness"
    )) |> # nolint
    bind_rows(mutate(scores_base,
      model_variation = "Training volume"
    )) |> # nolint
    bind_rows(mutate(scores_base,
      model_variation = "Weekday\nfilter"
    ))

  p <- ggplot(
    scores_perms,
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
      strip.text = element_text(size = 18)
    ) +
    labs(
      y = "WIS", x = ""
    ) +
    facet_grid(. ~ model_variation,
      space = "free_x",
      scales = "free_x"
    ) +
    scale_fill_manual(
      name = "Method specification",
      values = plot_colors$permutation_colors
    ) +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_colors$score_alpha
    ) +
    guides(
      # Can used fill = "none" if we want to remove color
      alpha = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 3
      ),
      fill = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 3
      )
    )
  return(p)
}

#' Get a line plot of the relative wis over time for each model permutation
#'
#' @inheritParams get_plot_bar_chart_scores_mp
#' @param age_group_to_plot Character string indicating which age group to
#'    show in the plot. Default is `"00+"`for all age groups.
#' @param permutation_grouping Character string indicating which model
#'    permutation grouping to plot. Default is `NULL` for all of them.
#' @autoglobal
#' @returns ggplot object
#' @importFrom dplyr filter mutate left_join rename select
#' @importFrom glue glue
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 aes ggplot labs theme_bw coord_flip geom_line
#'    scale_color_manual geom_hline xlab ylab ggtitle scale_x_date
get_plot_rel_wis_over_time_mp <- function(scores,
                                          strata,
                                          age_group_to_plot = "00+",
                                          permutation_grouping = NULL) {
  plot_colors <- plot_components()
  if (strata == "age groups") {
    scores <- filter(scores, age_group != "00+")
  } else if (strata == "national") {
    scores <- filter(scores, age_group == "00+")
  }
  if (!is.null(permutation_grouping)) {
    scores <- filter(scores, model_variation %in%
      c(
        {{ permutation_grouping }},
        "Default"
      ))
  }
  if (!is.null(age_group_to_plot)) {
    scores <- filter(scores, age_group == {{ age_group_to_plot }})
  }

  summarised_scores <- scores |>
    summarise_scores(by = c(
      "model_variation_string",
      "model_variation",
      "nowcast_date"
    )) |>
    select(wis, model_variation_string, model_variation, nowcast_date)

  baseline_score <- summarised_scores |>
    filter(model_variation_string == "Default") |>
    rename(baseline_wis = wis) |>
    select(baseline_wis, nowcast_date)
  rel_wis <- summarised_scores |>
    filter(model_variation_string != "Default") |>
    left_join(baseline_score, by = "nowcast_date") |>
    mutate(
      rel_wis = wis / pmax(baseline_wis, .Machine$double.eps),
      model_variation =
        case_when(
          model_variation == "Default" ~ "Baseline\nvalidation",
          model_variation == "Borrow for delay and uncertainty estimation" ~
            "Borrow for delay and\nuncertainty estimation",
          model_variation == "Reporting triangle completeness" ~
            "Reporting triangle\ncompleteness",
          TRUE ~ model_variation
        )
    )

  p <- ggplot(rel_wis) +
    geom_line(aes(
      x = nowcast_date, y = rel_wis,
      color = model_variation_string
    )) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    get_plot_theme() +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(values = plot_colors$permutation_colors) +
    xlab("") +
    guides(color = "none") +
    scale_y_continuous(trans = "log10") +
    # theme( axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("rWIS")

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
    mutate(
      interval_range = factor(interval_range, levels = c("95", "50")),
      model_variation =
        case_when(
          model_variation == "Borrow for delay and uncertainty estimation" ~
            "Borrow for delay and\nuncertainty estimation",
          model_variation == "Reporting triangle completeness" ~
            "Reporting triangle\ncompleteness",
          model_variation == "Weekday filter" ~
            "Weekday\nfilter",
          TRUE ~ model_variation
        )
    )
  coverage_base <- filter(
    coverage_summarised,
    model_variation == "Default"
  )
  # Make Default be alongside all the others
  coverage_perms <- filter(
    coverage_summarised,
    model_variation != "Default"
  ) |>
    bind_rows(mutate(coverage_base,
      model_variation = "Borrow for delay and\nuncertainty estimation"
    )) |> # nolint
    bind_rows(mutate(coverage_base,
      model_variation = "Reporting triangle\ncompleteness"
    )) |> # nolint
    bind_rows(mutate(coverage_base, model_variation = "Training volume")) |>
    bind_rows(mutate(coverage_base, model_variation = "Weekday\nfilter"))

  plot_comps <- plot_components()
  p <- ggplot(coverage_perms) +
    geom_bar(
      aes(
        x = model_variation_string, y = empirical_coverage,
        alpha = interval_range,
        fill = model_variation_string
      ),
      stat = "identity", position = "stack"
    ) +
    facet_grid(. ~ model_variation,
      space = "free_x", scales = "free_x"
    ) +
    get_plot_theme() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.placement = "outside",
      strip.background = element_rect(color = NA, fill = NA),
      strip.text = element_text(size = 18)
    ) +
    scale_fill_manual(
      name = "Method specification",
      values = plot_comps$permutation_colors
    ) +
    scale_alpha_manual(
      name = "Interval coverage",
      values = plot_comps$coverage_alpha,
      labels = c("50" = "50%", "95" = "95%")
    ) +
    geom_hline(aes(yintercept = 0.50), linetype = "dashed") +
    geom_hline(aes(yintercept = 0.95), linetype = "dashed") +
    labs(
      y = "Empirical coverage", x = ""
    ) +
    guides(
      # Can used fill = "none" if we want to remove color
      alpha = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 3
      ),
      fill = "none"
    )

  return(p)
}

#' Get a plot of relative WIS by horizon for all model permutations
#'
#' @param scores Dataframe of all the scores by individual reference and
#'    nowcast dates and model permutation
#' @inheritParams get_plot_bar_chart_scores_mp
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_line aes labs scale_color_manual
#'    guide_legend
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
    filter(model_variation_string == "Default") |>
    rename(baseline_wis = wis) |>
    select(baseline_wis, horizon)
  rel_wis <- scores_sum |>
    filter(model_variation_string != "Default") |>
    left_join(baseline_scores, by = "horizon") |>
    mutate(
      relative_wis = wis / pmax(baseline_wis, .Machine$double.eps),
      model_variation =
        case_when(
          model_variation == "Default" ~ "Baseline\nvalidation",
          model_variation == "Borrow for delay and uncertainty estimation" ~
            "Borrow for delay and\nuncertainty estimation",
          model_variation == "Reporting triangle completeness" ~
            "Reporting triangle\ncompleteness",
          TRUE ~ model_variation
        )
    )

  plot_comps <- plot_components()
  p <- ggplot(rel_wis, aes(
    x = horizon, y = relative_wis,
    color = model_variation_string
  )) +
    geom_line(linewidth = 1) +
    facet_grid(. ~ model_variation,
      space = "free_x", scales = "free_x"
    ) +
    get_plot_theme() +
    theme(
      strip.placement = "outside",
      strip.background = element_rect(color = NA, fill = NA)
    ) +
    scale_color_manual(
      name = "Permutation grouping",
      values = plot_comps$permutation_colors
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    scale_y_continuous(trans = "log10", limits = c(0.6, 2.5)) +
    labs(
      x = "Horizon (days)",
      y = "rWIS"
    ) +
    guides(
      color = "none",
      linetype = guide_legend(title.position = "top", title.hjust = 0.5)
    )
  return(p)
}

#' Get a plot of relative decomposed WIS by age group
#'
#' @param scores Dataframe of all the scores by individual reference and
#'    nowcast dates and model permutation
#' @param facet Boolean indicating whether or not to create separate facets for
#'    each WIS component. Default is `FALSE`
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_line aes labs scale_color_manual vars
#'    facet_grid scale_shape_manual
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
    filter(model_variation_string == "Default") |>
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
    filter(model_variation_string != "Default") |>
    left_join(baseline_scores, by = "age_group") |>
    mutate(
      overall_rel_wis = wis / pmax(baseline_wis, .Machine$double.eps),
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
      age_group, model_variation_string, model_variation,
      overall_rel_wis,
      relative_underprediction, relative_overprediction,
      relative_dispersion
    ) |>
    pivot_longer(
      cols = starts_with("relative_"),
      names_prefix = "relative_",
      names_to = "component",
      values_to = "rel_score"
    ) |>
    mutate(
      model_variation =
        case_when(
          model_variation == "Default" ~ "Baseline\nvalidation",
          model_variation == "Borrow for delay and uncertainty estimation" ~
            "Borrow for delay and\nuncertainty estimation",
          model_variation == "Reporting triangle completeness" ~
            "Reporting triangle\ncompleteness",
          TRUE ~ model_variation
        ),
      component = factor(component, levels = c(
        "overprediction",
        "dispersion",
        "underprediction"
      ))
    )

  plot_comps <- plot_components()
  p <- ggplot(rel_wis) +
    geom_point(aes(
      x = component, y = rel_score,
      color = model_variation_string,
      shape = component
    ), size = 4) +
    geom_hline(
      aes(
        yintercept = overall_rel_wis,
        color = model_variation_string
      ),
      linewidth = 1
    ) +
    facet_grid(
      rows = vars(age_group),
      cols = vars(model_variation),
      scales = "free_y"
    ) +
    get_plot_theme() +
    theme(
      strip.placement = "outside",
      strip.background = element_rect(color = NA, fill = NA),
      axis.text.x = element_blank()
    ) +
    scale_color_manual(
      name = "Method specificaition",
      values = plot_comps$permutation_colors
    ) +
    # nolint start
    scale_shape_manual(
      name = "WIS breakdown",
      values = c(
        "dispersion" = 16,
        "overprediction" = 17,
        "underprediction" = 15
      )
    ) +
    # nolint end
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    scale_y_continuous(trans = "log10") +
    labs(
      x = "",
      y = "rWIS"
    ) +
    guides(
      # Can used fill = "none" if we want to remove color
      alpha = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 3
      ),
      shape = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 3
      ),
      fill = "none",
      color = "none"
    )


  if (isTRUE(facet)) {
    p <- p + facet_wrap(~component, nrow = 3, scales = "free_y")
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
      name = "Method specification",
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
    facet_grid(. ~ age_group, switch = "x", scales = "free_y") +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_comps$score_alpha
    ) +
    labs(x = "", y = "WIS") +
    guides(
      fill = guide_legend(
        nrow = 3,
        title.position = "top"
      ),
      alpha = guide_legend(
        nrow = 3,
        title.position = "top"
      )
    )
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
      name = "Method specification",
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
    guides(
      fill = guide_legend(
        nrow = 3,
        title.position = "top"
      ),
      alpha = guide_legend(
        nrow = 3,
        title.position = "top"
      )
    )
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
      name = "Method specification",
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
    guides(
      fill = guide_legend(
        nrow = 3,
        title.position = "top"
      ),
      alpha = guide_legend(
        nrow = 3,
        title.position = "top"
      )
    )
  if (isTRUE(save)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 25,
      height = 8
    )
  }
  return(p)
}

#' Make a patchwork plot for panel A combining facets and underlays
#'
#' @param plot_nowcasts_t_mp_borrow larger nowcasts over time
#' @param rel_wis_over_time_mp_borrow underlay relative WIS
#' @param plot_nowcasts_t_mp_rep_tri larger nowcasts over time
#' @param rel_wis_over_time_mp_rep_tri underlay relative WIS
#' @param plot_nowcasts_t_mp_volume larger nowcasts over time
#' @param rel_wis_over_time_mp_volume underlay relative WIS
#' @param fig_file_name Character string indicating name of the figure to be
#'    saved as the file name
#' @param fig_file_dir Path to save figure. Default is
#'    `file.path("output", "figs")`.
#' @param save Boolean indicating whether or not to save the figure to disk.
#'    Default is `FALSE`.
#'
#' @returns patchwork ggplot object
make_panel_A_model_perms <- function(
    plot_nowcasts_t_mp_borrow,
    rel_wis_over_time_mp_borrow,
    plot_nowcasts_t_mp_rep_tri,
    rel_wis_over_time_mp_rep_tri,
    plot_nowcasts_t_mp_volume,
    rel_wis_over_time_mp_volume,
    fig_file_name,
    fig_file_dir = file.path("output", "figs"),
    save = FALSE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }
  fig_layout <- "
  AAAA
  AAAA
  BBBB
  CCCC
  CCCC
  DDDD
  EEEE
  EEEE
  FFFF
  "

  fig_panel_A <- (plot_nowcasts_t_mp_borrow +
    theme(plot.tag.position = c(0, 0.75))) +
    rel_wis_over_time_mp_borrow +
    plot_nowcasts_t_mp_rep_tri +
    rel_wis_over_time_mp_rep_tri +
    plot_nowcasts_t_mp_volume +
    rel_wis_over_time_mp_volume +
    plot_layout(
      design = fig_layout,
      axes = "collect"
    )

  if (isTRUE(save)) {
    # Add tags and fill legend if saving as stand-alone
    fig_panel_A <- (plot_nowcasts_t_mp_borrow +
      theme(plot.tag.position = c(0, 0.75)) +
      guides(fill = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 4
      ))) +
      rel_wis_over_time_mp_borrow +
      plot_nowcasts_t_mp_rep_tri +
      rel_wis_over_time_mp_rep_tri +
      plot_nowcasts_t_mp_volume +
      rel_wis_over_time_mp_volume +
      plot_layout(
        design = fig_layout,
        axes = "collect",
        guides = "collect"
      ) +
      plot_annotation(
        tag_levels = "A",
        tag_sep = "" # no separator between tag levels
      )
    ggsave(
      plot = fig_panel_A,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 16,
      height = 20
    )
  }
  return(fig_panel_A)
}

#' Make a patchwork plot for panel A combining facets and underlays with two
#'    age groups side by side
#'
#' @param plot_nowcasts_t_mp_borrow1 larger nowcasts over time
#' @param rel_wis_over_time_mp_borrow1 underlay relative WIS
#' @param plot_nowcasts_t_mp_rep_tri1 larger nowcasts over time
#' @param rel_wis_over_time_mp_rep_tri1 underlay relative WIS
#' @param plot_nowcasts_t_mp_volume1 larger nowcasts over time
#' @param rel_wis_over_time_mp_volume1 underlay relative WIS
#' @param plot_nowcasts_t_mp_wday1 larger nowcasts over time
#' @param rel_wis_over_time_mp_wday1 underlay relative WIS
#' @param plot_nowcasts_t_mp_borrow2 larger nowcasts over time
#' @param rel_wis_over_time_mp_borrow2 underlay relative WIS
#' @param plot_nowcasts_t_mp_rep_tri2 larger nowcasts over time
#' @param rel_wis_over_time_mp_rep_tri2 underlay relative WIS
#' @param plot_nowcasts_t_mp_volume2 larger nowcasts over time
#' @param rel_wis_over_time_mp_volume2 underlay relative WIS
#' @param plot_nowcasts_t_mp_wday2 larger nowcasts over time
#' @param rel_wis_over_time_mp_wday2 underlay relative WIS
#'
#' @returns patchwork ggplot object
make_panel_A_mps_2_ags <- function(
    plot_nowcasts_t_mp_borrow1,
    rel_wis_over_time_mp_borrow1,
    plot_nowcasts_t_mp_rep_tri1,
    rel_wis_over_time_mp_rep_tri1,
    plot_nowcasts_t_mp_volume1,
    rel_wis_over_time_mp_volume1,
    plot_nowcasts_t_mp_wday1,
    rel_wis_over_time_mp_wday1,
    plot_nowcasts_t_mp_borrow2,
    rel_wis_over_time_mp_borrow2,
    plot_nowcasts_t_mp_rep_tri2,
    rel_wis_over_time_mp_rep_tri2,
    plot_nowcasts_t_mp_volume2,
    rel_wis_over_time_mp_volume2,
    plot_nowcasts_t_mp_wday2,
    rel_wis_over_time_mp_wday2) {
  fig_layout <- "
  AAII
  AAII
  BBJJ
  CCKK
  CCKK
  DDLL
  EEMM
  EEMM
  FFNN
  GGOO
  GGOO
  HHPP
  "

  fig_panel_A <- (plot_nowcasts_t_mp_borrow1 + labs(tag = " A i") +
    theme(plot.tag.position = c(0, 0.7))) +
    (rel_wis_over_time_mp_borrow1 + labs(tag = "ii") +
      theme(plot.tag.position = c(-0.01, 1.01))) +
    (plot_nowcasts_t_mp_rep_tri1 + labs(tag = "iii")) +
    (rel_wis_over_time_mp_rep_tri1 + labs(tag = "iv") +
      theme(plot.tag.position = c(-0.01, 1.01))) +
    (plot_nowcasts_t_mp_volume1 + labs(tag = "v")) +
    (rel_wis_over_time_mp_volume1 + labs(tag = "vi") +
      theme(plot.tag.position = c(-0.01, 1.01))) +
    (plot_nowcasts_t_mp_wday1 + labs(tag = "vii")) +
    (rel_wis_over_time_mp_wday1 + labs(tag = "viii") +
      theme(plot.tag.position = c(-0.01, 1.01))) +
    (plot_nowcasts_t_mp_borrow2 + labs(tag = "ix") +
      theme(plot.tag.position = c(-0.01, 0.7))) +
    (rel_wis_over_time_mp_borrow2 + labs(tag = "x")) +
    (plot_nowcasts_t_mp_rep_tri2 + labs(tag = "xi")) +
    (rel_wis_over_time_mp_rep_tri2 + labs(tag = "xii") +
      theme(plot.tag.position = c(-0.01, 1.01))) +
    (plot_nowcasts_t_mp_volume2 + labs(tag = "xiii")) +
    (rel_wis_over_time_mp_volume2 + labs(tag = "xiv") +
      theme(plot.tag.position = c(-0.01, 1.01))) +
    (plot_nowcasts_t_mp_wday2 + labs(tag = "xv")) +
    (rel_wis_over_time_mp_wday2 + labs(tag = "xvi") +
      theme(plot.tag.position = c(-0.01, 1.01))) +
    plot_layout(
      design = fig_layout,
      axes = "collect"
    ) +
    plot_annotation(
      tag_levels = "I",
      tag_sep = "" # no separator between tag levels
    )
  return(fig_panel_A)
}


#' Make panel for main model permutation figure
#'
#' @param panel_A_nowcasts_over_time A
#' @param bar_chart_wis_by_mp B
#' @param bar_chart_coverage_mp C
#' @param rel_wis_by_horizon_mp D
#' @param rel_decomp_wis_by_age_group E
#' @param fig_file_name Character string indicating name of the figure to be
#'    saved as the file name
#' @param fig_file_dir Path to save figure. Default is
#'    `file.path("output", "figs")`.
#' @param save Boolean indicating whether or not to save the figure to disk.
#'    Default is `TRUE`.
#' @autoglobal
#' @importFrom glue glue
#' @importFrom patchwork plot_layout plot_annotation wrap_plots
#' @importFrom ggplot2 ggsave theme
#' @importFrom fs dir_create
#' @returns ggplot object as a gridded panel
make_fig_model_perms <- function(
    panel_A_nowcasts_over_time,
    bar_chart_wis_by_mp,
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
  AAABB
  AAACC
  AAACC
  AAADD
  AAAEE
  AAAEE
  AAAEE
  AAAEE
  AAAEE
  AAAEE
  "


  wrapped_panel_A <- wrap_plots(panel_A_nowcasts_over_time)
  # theme(plot.tag.position = c(0, 0.70)))
  fig_model_perm <- (wrapped_panel_A + labs(tag = "A")) + # A
    (bar_chart_wis_by_mp + labs(tag = "B") +
      theme(plot.tag.position = c(0, 0.6))) + # B
    (bar_chart_coverage_mp + labs(tag = "C")) + # C
    (rel_wis_by_horizon_mp + labs(tag = "D")) + # D
    (rel_decomp_wis_by_age_group + labs(tag = "E")) + # E
    plot_layout(
      design = fig_layout,
      axes = "collect",
      guides = "collect"
    ) & theme(
    legend.position = "top",
    legend.title = element_text(hjust = 0.5),
    legend.justification = "center",
    plot.tag = element_text(size = 18),
    plot.title.position = "plot", # Position title relative to plot area
    plot.title = element_text(
      hjust = 0.5, # Center the title
      vjust = -4 # Move title down (negative moves down)
    )
  )

  dir_create(fig_file_dir)

  if (isTRUE(save)) {
    ggsave(
      plot = fig_model_perm,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 32,
      height = 22,
      dpi = 300
    )
  }

  return(fig_model_perm)
}

#' Get a line plot of the relative wis over time for each model permutation
#'
#' @inheritParams get_plot_bar_chart_scores_mp
#' @inheritParams make_fig_model_perms
#' @autoglobal
#' @returns ggplot object
#' @importFrom dplyr filter mutate left_join rename select
#' @importFrom glue glue
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 aes ggplot labs theme_bw coord_flip geom_line
#'    scale_color_manual geom_hline xlab ylab ggtitle scale_x_date
get_plot_rel_wis_over_time_all <- function(scores,
                                           strata,
                                           fig_file_name,
                                           fig_file_dir = file.path(
                                             "output",
                                             "figs",
                                             "supp"
                                           ),
                                           save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }
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
    filter(model_variation_string == "Default") |>
    rename(baseline_wis = wis) |>
    select(baseline_wis, nowcast_date)
  rel_wis <- summarised_scores |>
    filter(model_variation_string != "Default") |>
    left_join(baseline_score, by = "nowcast_date") |>
    mutate(
      rel_wis = wis / pmax(baseline_wis, .Machine$double.eps),
      model_variation =
        case_when(
          model_variation == "Default" ~ "Baseline\nvalidation",
          model_variation == "Borrow for delay and uncertainty estimation" ~
            "Borrow for delay and\nuncertainty estimation",
          model_variation == "Reporting triangle completeness" ~
            "Reporting triangle\ncompleteness",
          TRUE ~ model_variation
        )
    )

  p <- ggplot(rel_wis) +
    geom_line(aes(
      x = nowcast_date, y = rel_wis,
      color = model_variation_string
    ), linewidth = 1) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    get_plot_theme() +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    facet_wrap(~model_variation, nrow = 3) +
    scale_color_manual(
      name = "Method specification",
      values = plot_colors$permutation_colors
    ) +
    xlab("") +
    guides(color = guide_legend(
      nrow = 3,
      title.position = "top"
    )) +
    scale_y_continuous(trans = "log10") +
    # theme( axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Relative WIS") +
    theme(
      strip.placement = "outside",
      strip.background = element_rect(color = NA, fill = NA),
      legend.position = "bottom"
    )

  dir_create(fig_file_dir)

  if (isTRUE(save)) {
    ggsave(
      plot = p,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 15,
      height = 10
    )
  }

  return(p)
}
