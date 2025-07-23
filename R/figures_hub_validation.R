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
#'    scale_alpha_manual guide_legend
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
    scale_fill_manual(
      name = "Model",
      values = plot_colors$model_colors
    ) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    labs(
      x = "Model", y = "WIS"
    ) +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_colors$score_alpha
    ) +
    guides(
      fill = "none",
      alpha = guide_legend(
        title.position = "top", title.hjust = 0.5,
        nrow = 3
      )
    )
  return(p)
}

#' Get a plot illustrating nowcasts at certain dates
#'
#' Hub validation figure A
#'
#' @param combined_nowcasts Dataframe of the combined quantiles across
#'    horizons and nowcast dates
#' @param max_horizon Integer indicating the maximum horizon to plot.
#' @param nowcast_dates_to_plot Vector of character strings of the dates you
#'   wish to plot, default is `NULL` which will plot all of them
#' @param age_group_to_plot Character string indicating which age group to
#'    show in the plot, default is "00+" for all age groups.
#' @param facet Boolean indicating whether or not to make separate facets
#'    of each model
#' @importFrom glue glue
#' @importFrom ggplot2 aes ggplot ggtitle xlab ylab geom_line geom_ribbon
#'    facet_wrap scale_color_manual scale_fill_manual guide_legend
#'     scale_linetype_manual
#' @importFrom dplyr filter
#' @returns ggplot object
#' @autoglobal
get_plot_nowcast_illustration <- function(combined_nowcasts,
                                          max_horizon = 28,
                                          nowcast_dates_to_plot = NULL,
                                          age_group_to_plot = "00+",
                                          facet = FALSE) {
  nc <- combined_nowcasts |>
    filter(
      horizon <= max_horizon,
      age_group == age_group_to_plot,
      nowcast_date %in% c(nowcast_dates_to_plot)
    ) |>
    mutate(nowcast_date_model = glue("{nowcast_date}-{model}"))
  data_only <- filter(
    combined_nowcasts,
    horizon <= max_horizon,
    age_group == age_group_to_plot,
    horizon == 0
  )
  plot_colors <- plot_components()
  p <- ggplot() +
    geom_line(
      data = nc,
      aes(
        x = reference_date, y = `q_0.5`,
        color = model, group = nowcast_date_model
      )
    ) +
    geom_line(
      data = nc,
      aes(
        x = reference_date, y = data_as_of,
        group = nowcast_date,
        linetype = "Data as of nowcast date"
      ),
      color = "gray",
      linewidth = 1
    ) +
    geom_vline(
      data = nc,
      aes(
        xintercept = nowcast_date,
        linetype = "Date of nowcast"
      ),
      color = "black"
    ) +
    geom_ribbon(
      data = nc,
      aes(
        x = reference_date,
        ymin = `q_0.25`,
        ymax = `q_0.75`, fill = model,
        group = nowcast_date_model,
        alpha = "50%"
      )
    ) +
    geom_ribbon(
      data = nc,
      aes(
        x = reference_date,
        ymin = `q_0.025`,
        ymax = `q_0.975`, fill = model,
        group = nowcast_date_model,
        alpha = "95%"
      )
    ) +
    geom_line(
      data = data_only,
      aes(
        x = reference_date, y = observed,
        linetype = "Final evaluation data"
      ),
      color = "red", linewidth = 1
    ) +
    get_plot_theme() +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(
      name = "Model",
      values = plot_colors$model_colors
    ) +
    # Add scale for the reference lines
    scale_linetype_manual(
      name = "Observed data",
      values = c(
        "Final evaluation data" = "solid",
        "Data as of nowcast date" = "solid",
        "Date of nowcast" = "dashed"
      ),
      breaks = c(
        "Final evaluation data",
        "Data as of nowcast date",
        "Date of nowcast"
      ),
      guide = guide_legend(
        override.aes = list(
          color = c(
            "Final evaluation data" = "red",
            "Data as of nowcast date" = "gray",
            "Date of nowcast" = "black"
          ),
          linewidth = 1
        )
      )
    ) +
    scale_fill_manual(
      name = "Model",
      values = plot_colors$model_colors
    ) +
    scale_alpha_manual(
      name = "Prediction intervals",
      values = c(
        "95%" = 0.2,
        "50%" = 0.4
      ),
      guide = guide_legend(
        override.aes = list(
          alpha = c(
            "95%" = 0.2,
            "50%" = 0.4
          )
        )
      )
    ) +
    xlab("") +
    ylab("7-day hospitalisation incidence") +
    guides(
      color = guide_legend(title.position = "top"),
      fill = guide_legend(title.position = "top"),
      linetype = guide_legend(
        title.position = "top",
        nrow = 3
      ),
      alpha = guide_legend(title.position = "top")
    )

  if (isTRUE(facet)) {
    p <- p + facet_wrap(~model)
  }


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
#'    facet_wrap scale_color_manual scale_fill_manual guide_legend
#'     scale_linetype_manual
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
    geom_line(
      aes(
        x = reference_date, y = observed,
        linetype = "Final evaluation data"
      ),
      color = "red", linewidth = 1
    ) +
    geom_line(
      aes(
        x = reference_date, y = data_as_of,
        linetype = "Data as of nowcast date"
      ),
      color = "gray", linewidth = 1
    ) +
    get_plot_theme() +
    scale_x_date(
      limits = as.Date(c("2021-11-08", "2022-04-29")),
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(
      name = "Model",
      values = plot_colors$model_colors
    ) +
    # Add scale for the reference lines
    scale_linetype_manual(
      name = "Observed data",
      values = c(
        "Final evaluation data" = "solid",
        "Data as of nowcast date" = "solid"
      ),
      guide = guide_legend(
        override.aes = list(
          color = c(
            "Final evaluation data" = "red",
            "Data as of nowcast date" = "gray"
          ),
          linewidth = 1
        )
      )
    ) +
    scale_fill_manual(
      name = "Model",
      values = plot_colors$model_colors
    ) +
    xlab("") +
    ylab("7-day hospitalisation incidence") +
    guides(
      color = guide_legend(title.position = "top", title.hjust = 0.5),
      fill = guide_legend(title.position = "top", title.hjust = 0.5),
      linetype = guide_legend(
        title.position = "top", title.hjust = 0.5,
        nrow = 2
      )
    )

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
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(
      name = "Model",
      values = plot_colors$model_colors
    ) +
    guides(color = "none") +
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
      strip.background = element_rect(color = NA, fill = NA)
    ) +
    labs(
      y = "WIS", x = "", fill = ""
    ) +
    facet_grid(. ~ age_group, switch = "x") +
    scale_fill_manual(
      name = "Model",
      values = plot_colors$model_colors
    ) +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_colors$score_alpha
    ) +
    guides(
      fill = "none",
      alpha = "none"
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
#'    guides guide_legend
get_plot_mean_delay_over_time <- function(delays_over_time) {
  plot_comps <- plot_components()
  p <- ggplot(data = delays_over_time) +
    geom_line(aes(
      x = ymd(nowcast_date), y = mean_delay,
      color = age_group,
      linewidth = age_group
    )) +
    get_plot_theme() +
    scale_x_date(
      date_breaks = "1 month",
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
    ylab("Mean delay over time") +
    guides(
      linewidth = "none",
      color = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        override.aes = list(linewidth = 2)
      )
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
#' @importFrom ggplot2 ggplot geom_line aes labs guides guide_legend
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
    get_plot_theme() +
    guides(
      color = "none"
    )



  return(p)
}

#' Get a horizontal bar chart of coverage
#'
#' @param all_coverage Dataframe of the combined individual level coverage for
#'    days and intervals.
#' @param strata Character string indicating which strata to summarize over.
#' @param intervals Vector of integers to plot coverage of.
#' @inheritParams get_plot_rel_wis_by_age_group
#' @importFrom ggplot2 ggplot geom_bar aes labs scale_alpha_manual
#'    scale_fill_manual geom_hline
#' @importFrom dplyr filter group_by summarise mutate n
#' @importFrom tidyr pivot_wider pivot_longer
#' @autoglobal
#' @returns bar chart
get_plot_bar_chart_coverage <- function(
    all_coverage,
    strata,
    intervals = c(50, 95),
    fig_file_name = NULL,
    fig_file_dir = file.path("output", "figs", "supp"),
    save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }
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
      name = "Model",
      values = plot_comps$model_colors
    ) +
    scale_alpha_manual(
      name = "Interval coverage",
      values = plot_comps$coverage_alpha,
      labels = c("50" = "50%", "95" = "95%")
    ) +
    geom_hline(aes(yintercept = 0.50), linetype = "dashed") +
    geom_hline(aes(yintercept = 0.95), linetype = "dashed") +
    labs(
      x = "Empirical coverage", y = "",
      fill = ""
    ) +
    coord_flip() +
    get_plot_theme()
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

#' Get a plot of relative WIS by age group
#'
#' @param scores_by_age_group Dataframe of the summarised scores by age group
#'    and model
#' @param KIT_comparison_model Character string indicating which of the KIT
#'     models to compare to. Default is `"KIT simple nowcast revised"`.
#' @param fig_file_dir Path to save figure. Default is
#'    `file.path("output", "figs", "supp")`.
#' @inheritParams make_fig_hub_validation
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_bar aes labs scale_fill_manual
#'    geom_hline scale_y_continuous
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom fs dir_create
#' @importFrom glue glue
#' @returns ggplot object
get_plot_rel_wis_by_age_group <- function(
    scores_by_age_group,
    KIT_comparison_model = "KIT simple nowcast revised",
    fig_file_name = NULL,
    fig_file_dir = file.path("output", "figs", "supp"),
    save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }
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
    ), show.legend = FALSE) +
    scale_fill_manual(
      name = "",
      values = plot_comps$model_colors
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    scale_y_continuous(trans = "log", limits = c(0.66, 1.5)) +
    coord_flip() +
    get_plot_theme() +
    labs(x = "", y = "Relative WIS")

  if (isTRUE(save)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 10,
      height = 8
    )
  }


  return(p)
}
#' Get a plot of mean WIS by horizon
#'
#' @param scores Dataframe of all the scores by individual reference and
#'    nowcast dates and model and age groups
#' @inheritParams get_plot_bar_chart_sum_scores
#' @inheritParams get_plot_rel_wis_by_age_group
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_line aes labs scale_color_manual
#' @importFrom dplyr select filter
#' @importFrom scoringutils summarise_scores
#' @importFrom fs dir_create
#' @importFrom glue glue
#' @returns ggplot object
get_plot_mean_wis_by_horizon <- function(
    scores,
    strata,
    fig_file_name = NULL,
    fig_file_dir = file.path("output", "figs", "supp"),
    save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }
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
      name = "Model",
      values = plot_comps$model_colors
    ) +
    get_plot_theme() +
    theme(legend.position = "bottom")
  labs(x = "Horizon (days)", y = "Mean WIS", color = "Model")
  if (isTRUE(save)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 10,
      height = 8
    )
  }

  return(p)
}

#' Get a plot of relative WIS by horizon
#'
#' @param scores Dataframe of all the scores by individual reference and
#'    nowcast dates and model and age groups
#' @param KIT_comparison_model Character string indicating which model to
#'   compare to.
#' @inheritParams get_plot_bar_chart_sum_scores
#' @inheritParams get_plot_rel_wis_by_age_group
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_line aes labs scale_color_manual
#' @importFrom dplyr select filter
#' @importFrom scoringutils summarise_scores
#' @importFrom fs dir_create
#' @importFrom glue glue
#' @returns ggplot object
get_plot_rel_wis_by_horizon <- function(
    scores,
    strata,
    KIT_comparison_model = "KIT simple nowcast revised",
    fig_file_name = NULL,
    fig_file_dir = file.path("output", "figs", "supp"),
    save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }
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
    geom_line(aes(x = horizon, y = rel_wis, color = model),
      show.legend = FALSE
    ) +
    get_plot_theme() +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    scale_y_continuous(trans = "log") +
    scale_color_manual(
      name = "",
      values = plot_comps$model_colors
    ) +
    labs(x = "Horizon (days)", y = "Relative WIS")

  if (isTRUE(save)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 10,
      height = 8
    )
  }


  return(p)
}

#' Get a plot of coverage by horizon for each model
#'
#' @param all_coverage Dataframe of the combined individual level coverage for
#'    days and intervals.
#' @param strata Character string indicating which strata to summarize over.
#' @param intervals Vector of integers to plot coverage of.
#' @inheritParams get_plot_rel_wis_by_age_group
#' @importFrom ggplot2 ggplot geom_bar aes labs scale_alpha_manual
#'    scale_fill_manual geom_hline
#' @importFrom dplyr filter group_by summarise mutate n
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom fs dir_create
#' @autoglobal
#' @returns bar chart
get_plot_coverage_by_horizon <- function(
    all_coverage,
    strata,
    intervals = c(50, 95),
    fig_file_name = NULL,
    fig_file_dir = file.path("output", "figs", "supp"),
    save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }

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
      name = "Model",
      values = plot_comps$model_colors
    ) +
    facet_wrap(~interval_range, scales = "free_y") +
    labs(
      x = "Horizon(days)", y = "Empirical coverage",
      color = "Model"
    ) +
    get_plot_theme() +
    theme(legend.position = "bottom")

  if (isTRUE(save)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 10,
      height = 8
    )
  }

  return(p)
}

#' Get a plot of coverage by age_group for each model
#'
#' @param all_coverage Dataframe of the combined individual level coverage for
#'    days and intervals.
#' @param intervals Vector of integers to plot coverage of.
#' @inheritParams get_plot_rel_wis_by_age_group
#' @importFrom ggplot2 ggplot geom_bar aes labs scale_alpha_manual
#'    scale_fill_manual geom_hline
#' @importFrom dplyr filter group_by summarise mutate n
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom fs dir_create
#' @autoglobal
#' @returns bar chart
get_plot_coverage_by_age_group <- function(
    all_coverage,
    intervals = c(50, 95),
    fig_file_name = NULL,
    fig_file_dir = file.path("output", "figs", "supp"),
    save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }

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
      name = "Model",
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
      name = "Interval coverage",
      values = plot_comps$coverage_alpha,
      labels = c("50" = "50%", "95" = "95%")
    ) +
    geom_hline(aes(yintercept = 0.50), linetype = "dashed") +
    geom_hline(aes(yintercept = 0.95), linetype = "dashed") +
    labs(
      x = "Empirical coverage", y = "",
      fill = ""
    ) +
    coord_flip() +
    get_plot_theme()
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
#' @param plot_nowcasts_over_time A
#' @param horiz_bar_chart_sum_scores_ag B
#' @param plot_wis_comp_over_time_ag C
#' @param bar_chart_scores_by_age_group D
#' @param plot_mean_delay_over_time_by_age E
#' @param plot_mean_cdf_delay_by_age F
#' @param fig_file_name Character string indicating name of the figure to be
#'    saved as the file name. Default is `NULL`.
#' @param fig_file_dir Path to save figure. Default is
#'    `file.path("output", "figs")`.
#' @param save Boolean indicating whether or not to save the figure to disk.
#'    Default is `TRUE`.
#' @autoglobal
#' @importFrom glue glue
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom ggplot2 ggsave theme
#' @importFrom fs dir_create
#' @returns ggplot object as a gridded panel
make_fig_hub_validation <- function(
    plot_nowcasts_over_time,
    horiz_bar_chart_sum_scores_ag,
    plot_wis_comp_over_time_ag,
    bar_chart_scores_by_age_group,
    plot_mean_delay_over_time_by_age, # nolint
    plot_mean_cdf_delay_by_age,
    fig_file_name = NULL,
    fig_file_dir = file.path("output", "figs"),
    save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }

  fig_layout <- "
  AAAABB
  CCCCDD
  EEEEFF
  "

  fig_hub_validation <- (plot_nowcasts_over_time +
    theme(plot.tag.position = c(0, 0.75))) +
    (horiz_bar_chart_sum_scores_ag + theme(plot.tag.position = c(0, 0.75))) +
    plot_wis_comp_over_time_ag +
    bar_chart_scores_by_age_group +
    plot_mean_delay_over_time_by_age +
    plot_mean_cdf_delay_by_age +
    plot_layout(
      design = fig_layout,
      axes = "collect",
      guides = "collect"
    ) +
    plot_annotation(
      tag_levels = "A",
      tag_sep = ""
    ) & theme(
    legend.position = "top",
    legend.title = element_text(hjust = 0.5),
    legend.justification = "center",
    plot.tag = element_text(size = 18)
  )

  dir_create(fig_file_dir)
  if (isTRUE(save)) {
    ggsave(
      plot = fig_hub_validation,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 24,
      height = 16,
      dpi = 300
    )
  }

  return(fig_hub_validation)
}
