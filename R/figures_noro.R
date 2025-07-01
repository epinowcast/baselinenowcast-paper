#' Get a plot of the nowcasts string together for all dates or a set of dates
#'
#' @param all_nowcasts Dataframe of the quantiled nowcasts (in wide format)
#' @param nowcast_dates_to_plot Vector of character strings of the dates you
#'   wish to plot, default is `NULL` which will plot all of them
#' @param pathogen Character sting of the pathogen being plotted
#' @param title Character string indicating the title
#'
#' @autoglobal
#' @importFrom ggplot2 aes geom_line ggplot ggtitle xlab ylab theme_bw
#'    theme geom_ribbon geom_point scale_x_date element_text coord_cartesian
#'    geom_vline scale_color_manual scale_fill_manual
#' @importFrom glue glue
#' @importFrom dplyr filter
#' @importFrom lubridate ymd
#' @returns ggplot object
get_plot_mult_nowcasts_noro <- function(all_nowcasts,
                                        nowcast_dates_to_plot = NULL,
                                        pathogen = "",
                                        title = "") {
  if (!is.null(nowcast_dates_to_plot)) {
    all_nowcasts <- all_nowcasts |>
      filter(nowcast_date %in% c(nowcast_dates_to_plot))
  }
  all_nowcasts <- all_nowcasts |>
    mutate(nowcast_date_model = glue("{nowcast_date}-{model}"))
  plot_colors <- plot_components()

  p <- ggplot(all_nowcasts) +
    geom_ribbon(
      aes(
        x = reference_date, ymin = `q_0.05`, ymax = `q_0.95`,
        group = nowcast_date_model, fill = model
      ),
      alpha = 0.2
    ) +
    geom_ribbon(
      aes(
        x = reference_date, ymin = `q_0.25`, ymax = `q_0.75`,
        group = nowcast_date_model, fill = model
      ),
      alpha = 0.2
    ) +
    geom_line(aes(
      x = reference_date, y = `q_0.5`,
      group = nowcast_date_model, color = model
    )) +
    geom_line(
      aes(
        x = reference_date, y = observed,
        linetype = "Final evaluation data",
        group = nowcast_date
      ),
      color = "red", linewidth = 1
    ) +
    geom_line(
      aes(
        x = reference_date, y = data_as_of,
        linetype = "Data as of nowcast date",
        group = nowcast_date
      ),
      color = "gray", linewidth = 1
    ) +
    theme_bw() +
    facet_wrap(~model_type, nrow = 2) +
    scale_x_date(
      limits = c(as.Date("2023-10-30"), as.Date("2024-03-10")),
      date_breaks = "1 week", # Break every month
      date_labels = "%b %Y"
    ) +
    theme(
      axis.text.x = element_text(
        vjust = 1,
        hjust = 1,
        angle = 45
      ),
      legend.position = "bottom"
    ) +
    # nolint start
    scale_color_manual(
      values = plot_colors$model_colors,
      name = "Model"
    ) +
    scale_fill_manual(
      values = plot_colors$model_colors,
      name = "Model"
    ) +
    coord_cartesian(ylim = c(0, 110)) +
    scale_linetype_manual(
      name = "Observed data",
      values = c("Final evaluation data" = "solid", "Data as of nowcast date" = "solid"),
      guide = guide_legend(
        override.aes = list(
          color = c("Final evaluation data" = "red", "Data as of nowcast date" = "gray"),
          linewidth = 1
        )
      )
    ) +
    # nolint end
    xlab("") +
    ylab(glue("{pathogen} cases")) +
    ggtitle(glue("{title}")) +
    guides(
      color = guide_legend(title.position = "top", title.hjust = 0.5),
      fill = guide_legend(title.position = "top", title.hjust = 0.5),
      linetype = guide_legend(
        title.position = "top", title.hjust = 0.5,
        nrow = 2
      )
    )

  return(p)
}
#' Plot stacked bar chart of scores by model type colored by model and
#'    with alpha by over/under prediction and dispersion
#'
#' @param scores Dataframe containing scores across
#'    nowcast dates and reference dates, stratified by model and and model type.
#'
#' @returns ggplot object
#' @autoglobal
#' @importFrom ggplot2 ggplot aes labs
#'    facet_grid theme scale_fill_manual
#'    ggtitle element_blank scale_alpha_manual geom_bar
get_bar_chart_sum_scores_noro <- function(scores) {
  scores_summary <- scores |>
    summarise_scores(by = c("model", "model_type")) |>
    select(model, model_type, overprediction, underprediction, dispersion) |>
    pivot_longer(cols = c("overprediction", "underprediction", "dispersion")) |>
    mutate(name = factor(name, levels = c(
      "overprediction",
      "dispersion",
      "underprediction"
    )))
  plot_colors <- plot_components()
  p <- ggplot(
    scores_summary,
    aes(x = as.factor(model), y = value, fill = as.factor(model), alpha = name)
  ) +
    geom_bar(stat = "identity", position = "stack") +
    get_plot_theme() +
    labs(
      x = "", y = "WIS",
      color = "Model"
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.placement = "outside",
      strip.background = element_rect(color = NA, fill = NA)
    ) +
    facet_grid(. ~ model_type,
      switch = "x",
      space = "free_x", scales = "free_x"
    ) +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_colors$score_alpha
    ) +
    scale_fill_manual(
      name = "Model",
      values = plot_colors$model_colors
    ) +
    ggtitle("Overall WIS") +
    guides(
      fill = "none",
      alpha = guide_legend(
        title.position = "top", title.hjust = 0.5,
        nrow = 3
      )
    )
  return(p)
}

#' Get a line plot of relative WIS by nowcast date
#'
#' @inheritParams get_bar_chart_sum_scores_noro
#'
#' @returns ggplot object
#' @importFrom ggplot2 ggplot aes labs
#'    facet_grid theme scale_fill_manual
#'    ggtitle element_blank scale_alpha_manual geom_bar
#' @autoglobal
#' @importFrom dplyr select filter rename mutate
get_plot_rel_wis_over_time <- function(scores) {
  scores_sum <- scores |>
    scoringutils::summarise_scores(by = c("model", "nowcast_date")) |>
    select(model, nowcast_date, wis)

  baseline_comparison <- scores_sum |>
    filter(model == "base") |>
    rename(comparison_wis = wis) |>
    select(comparison_wis, nowcast_date)

  relative_wis <- scores_sum |>
    filter(model != "base") |>
    left_join(baseline_comparison, by = "nowcast_date") |>
    mutate(rel_wis = wis / pmax(comparison_wis, .Machine$double.eps))
  plot_comps <- plot_components()
  p <- ggplot(relative_wis) +
    geom_line(aes(
      x = as.Date(nowcast_date), y = rel_wis,
      color = model, group = model
    )) +
    get_plot_theme() +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    scale_y_continuous(trans = "log10") +
    scale_color_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    scale_x_date( # Jan 2023, Feb 2023, etc.
      limits = c(as.Date("2023-10-30"), as.Date("2024-03-10")),
      date_breaks = "1 week", # Break every month
      date_labels = "%b %Y"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    labs(x = "", y = "Relative WIS") +
    ggtitle("Relative WIS over time relative to baselinenowcast default model configuration") + # nolint
    guides(color = "none")

  return(p)
}

#' Get plot relative wis by weekday
#'
#' @inheritParams get_bar_chart_sum_scores_noro
#'
#' @returns ggplot object
#' @importFrom ggplot2 ggplot aes labs
#'    facet_grid theme scale_fill_manual
#'    ggtitle element_blank scale_alpha_manual geom_bar
#' @autoglobal
#' @importFrom dplyr select filter rename mutate
get_plot_rel_wis_by_weekday <- function(scores) {
  scores_sum <- scores |>
    mutate(
      weekday_name = wday(reference_date, label = TRUE),
      weekday = wday(reference_date, label = FALSE)
    ) |>
    scoringutils::summarise_scores(by = c(
      "model",
      "weekday",
      "weekday_name"
    )) |>
    select(model, weekday, weekday_name, wis)

  baseline_comparison <- scores_sum |>
    filter(model == "base") |>
    rename(comparison_wis = wis) |>
    select(comparison_wis, weekday)

  relative_wis <- scores_sum |>
    filter(model != "base") |>
    left_join(baseline_comparison, by = "weekday") |>
    mutate(rel_wis = wis / pmax(comparison_wis, .Machine$double.eps))
  plot_comps <- plot_components()
  p <- ggplot(relative_wis) +
    geom_line(aes(
      x = weekday_name, y = rel_wis,
      color = model, group = model
    )) +
    get_plot_theme() +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    scale_y_continuous(trans = "log10") +
    scale_color_manual(
      name = "",
      values = plot_comps$model_colors
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    labs(x = "", y = "Relative WIS") +
    ggtitle("Relative WIS by weekday relative to baselinenowcast default model configuration") + # nolint
    guides(color = "none")
  return(p)
}

#' Get plot mean delay over time by weekday
#'
#' @param delay_dfs Data.frame of delay estimates both separately by weekday
#'    and jointly, indexed by the weekday of the reference date.
#' @param n_history_delay_filter Integer indicating how much historical data
#'    to use to estimate the mean delay at each nowcast time. Default is `28`.
#'
#' @returns ggplot object
#' @importFrom ggplot2 ggplot aes labs
#'    facet_grid theme scale_fill_manual
#'    ggtitle element_blank scale_alpha_manual geom_bar
#' @autoglobal
#' @importFrom dplyr select filter rename mutate
get_plot_mean_delay_t_by_wday <- function(delay_dfs,
                                          n_history_delay_filter = 28) {
  mean_delay_by_weekday_and_date <- delay_dfs |>
    filter(
      n_history_delay == n_history_delay_filter,
      filter_ref_dates
    ) |>
    group_by(nowcast_date, weekday, weekday_name) |>
    summarise(
      mean_delay = sum(delay * delay_time)
    )

  mean_delay_all_weekdays <- delay_dfs |>
    filter(
      n_history_delay == 28,
      !filter_ref_dates
    ) |>
    group_by(nowcast_date) |>
    summarise(
      mean_delay = sum(delay * delay_time)
    ) |>
    mutate(
      weekday = NA,
      weekday_name = "All"
    )

  mean_delay_df <- bind_rows(
    mean_delay_all_weekdays,
    mean_delay_by_weekday_and_date
  )
  plot_comps <- plot_components()
  p <- ggplot(data = mean_delay_df) +
    geom_line(aes(
      x = ymd(nowcast_date), y = mean_delay,
      color = weekday_name,
      linewidth = weekday_name
    )) +
    guides(linewidth = "none") +
    get_plot_theme() +
    scale_x_date( # Jan 2023, Feb 2023, etc.
      limits = c(as.Date("2023-10-30"), as.Date("2024-03-10")),
      date_breaks = "1 week", # Break every month
      date_labels = "%b %Y"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    scale_color_manual(
      name = "Weekday",
      values = plot_comps$weekday_colors
    ) +
    scale_linewidth_manual(
      values = plot_comps$weekday_linewidth,
      labels = NULL
    ) +
    guides(
      color = guide_legend(title.position = "top", title.hjust = 0.5),
    ) +
    xlab("") +
    ylab("Mean delay over time")
  return(p)
}

#' Get a plot of the cdf colored by weekday
#'
#' @inheritParams get_plot_mean_delay_t_by_wday
#' @importFrom ggplot2 ggplot aes labs
#'    facet_grid theme scale_fill_manual
#'    ggtitle element_blank scale_alpha_manual geom_bar
#' @importFrom dplyr select filter rename mutate
#' @autoglobal
#' @returns ggplot object
get_plot_cdf_by_weekday <- function(delay_dfs) {
  avg_cdf_by_weekday <- delay_dfs |>
    filter(
      n_history_delay == 28,
      filter_ref_dates
    ) |>
    group_by(weekday_name, delay_time) |>
    summarise(mean_delay = mean(delay)) |>
    mutate(cdf = cumsum(mean_delay))

  avg_cdf_overall <- delay_dfs |>
    filter(
      n_history_delay == 28,
      !filter_ref_dates
    ) |>
    group_by(delay_time) |>
    summarise(mean_delay = mean(delay)) |>
    mutate(
      cdf = cumsum(mean_delay),
      weekday_name = "All"
    ) |>
    select(colnames(avg_cdf_by_weekday))

  cdf_df <- bind_rows(
    avg_cdf_by_weekday,
    avg_cdf_overall
  )

  plot_comps <- plot_components()
  p <- ggplot(cdf_df) +
    geom_line(
      aes(
        x = delay_time, y = cdf, color = weekday_name,
        linewidth = weekday_name
      )
    ) +
    guides(linewidth = "none") +
    scale_color_manual(
      name = "Weekday",
      values = plot_comps$weekday_colors
    ) +
    scale_linewidth_manual(
      values = plot_comps$weekday_linewidth,
      labels = NULL
    ) +
    xlab("Delay (days)") +
    ylab("Cumulative delay distribution") +
    get_plot_theme() +
    guides(
      color = "none",
      linewidth = "none"
    )

  return(p)
}

#' Make panel for main norovirus figure
#'
#' @param plot_noro_nowcasts A
#' @param bar_chart_wis_noro B
#' @param rel_wis_by_week_noro C
#' @param rel_wis_by_weekday D
#' @param plot_mean_delay_t_by_wday E
#' @param plot_cdf_by_weekday F
#' @inheritParams  make_fig_model_perms
#'
#' @returns ggplot
#' @autoglobal
#' @importFrom glue glue
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom ggplot2 ggsave theme
#' @importFrom fs dir_create
#' @returns ggplot object as a gridded panel
make_fig_noro <- function(plot_noro_nowcasts,
                          bar_chart_wis_noro,
                          rel_wis_by_week_noro,
                          rel_wis_by_weekday,
                          plot_mean_delay_t_by_wday,
                          plot_cdf_by_weekday,
                          fig_file_name,
                          fig_file_dir = file.path("output", "figs"),
                          save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }
  fig_layout <- "
  AAABB
  AAABB
  CCCDD
  EEEFF
  "

  fig_noro <- plot_noro_nowcasts +
    bar_chart_wis_noro +
    rel_wis_by_week_noro +
    rel_wis_by_weekday +
    plot_mean_delay_t_by_wday +
    plot_cdf_by_weekday +
    plot_layout(
      design = fig_layout,
      axes = "collect",
      guides = "collect"
    ) +
    plot_annotation(
      tag_levels = "A",
      tag_suffix = ".", # adds a period after each letter
      tag_sep = "" # no separator between tag levels
    ) & theme(
    legend.position = "top",
    legend.title = element_text(hjust = 0.5),
    legend.justification = "left"
  )

  dir_create(fig_file_dir)

  if (isTRUE(save)) {
    ggsave(
      plot = fig_noro,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 24,
      height = 18
    )
  }
  return(fig_noro)
}

#' Get a plot of decomposed WIS by week for each model
#'
#' @param scores Dataframe of the scores by age group, horizon, and model
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
get_plot_wis_over_time_noro <- function(
    scores,
    fig_file_name = NULL,
    fig_file_dir = file.path("output", "figs", "supp"),
    save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }

  metrics_attr <- attr(scores, "metrics")
  scores_sum <- scores |>
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
      "model",
      "model_type",
      "week_end_date"
    )) |>
    select(
      model, model_type,
      week_end_date, overprediction, underprediction, dispersion
    ) |>
    pivot_longer(cols = c(
      "overprediction",
      "underprediction",
      "dispersion"
    )) |>
    mutate(
      name = factor(name, levels = c(
        "overprediction",
        "dispersion",
        "underprediction"
      )),
      model = factor(model, levels = c(
        "base",
        "filter weekday small training volume",
        "filter weekday large training volume",
        "GAM",
        "epinowcast",
        "baseline Mellor et al"
      ))
    )

  plot_comps <- plot_components()
  p <- ggplot(
    scores_sum,
    aes(
      x = model, y = value,
      fill = model,
      alpha = name
    )
  ) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(
      name = "Model",
      values = plot_comps$model_colors
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
    ggtitle(glue::glue("WIS breakdown by nowcast date for all models")) # nolint
  if (isTRUE(save)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 28,
      height = 8
    )
  }
  return(p)
}

#' Get a plot of decomposed WIS by weekday for each model
#'
#' @param scores Dataframe of the scores by age group, horizon, and model
#' @inheritParams get_plot_rel_wis_by_age_group
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_bar aes labs scale_fill_manual
#'    geom_hline scale_y_continuous
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate wday
#' @importFrom glue glue
#' @importFrom fs dir_create
#' @returns ggplot object
get_plot_wis_by_weekday <- function(
    scores,
    fig_file_name = NULL,
    fig_file_dir = file.path("output", "figs", "supp"),
    save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }

  scores_sum <- scores |>
    mutate(
      weekday = wday(reference_date),
      weekday_name = wday(reference_date, label = TRUE)
    ) |>
    scoringutils::summarise_scores(by = c(
      "model",
      "model_type",
      "weekday",
      "weekday_name"
    )) |>
    select(
      model, model_type,
      weekday, weekday_name, overprediction, underprediction, dispersion
    ) |>
    pivot_longer(cols = c(
      "overprediction",
      "underprediction",
      "dispersion"
    )) |>
    mutate(
      name = factor(name, levels = c(
        "overprediction",
        "dispersion",
        "underprediction"
      )),
      model = factor(model, levels = c(
        "base",
        "filter weekday small training volume",
        "filter weekday large training volume",
        "GAM",
        "epinowcast",
        "baseline Mellor et al"
      ))
    )

  plot_comps <- plot_components()
  p <- ggplot(
    scores_sum,
    aes(
      x = model, y = value,
      fill = model,
      alpha = name
    )
  ) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(
      name = "Model",
      values = plot_comps$model_colors
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
    facet_grid(. ~ weekday_name, switch = "x") +
    labs(x = "Weekday", y = "WIS breakdown") +
    ggtitle(glue::glue("WIS breakdown by weekday for all models")) # nolint
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

#' Get a plot of coverage by model
#'
#' @param all_coverage Dataframe of the combined individual level coverage for
#'    days and intervals.
#' @param intervals Vector of integers to plot coverage of.
#' @inheritParams get_plot_rel_wis_by_age_group
#' @importFrom ggplot2 ggplot geom_bar aes labs scale_alpha_manual
#'    scale_fill_manual geom_hline
#' @importFrom dplyr filter group_by summarise mutate n
#' @importFrom tidyr pivot_wider pivot_longer
#' @autoglobal
#' @returns bar chart
get_plot_cov_by_model_noro <- function(all_coverage,
                                       intervals = c(50, 90),
                                       fig_file_name = NULL,
                                       fig_file_dir = file.path(
                                         "output",
                                         "figs",
                                         "supp"
                                       ),
                                       save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }
  coverage <- filter(
    all_coverage,
    interval_range %in% c(intervals)
  )

  coverage_summarised <- coverage |>
    group_by(model, interval_range) |>
    summarise(empirical_coverage = sum(interval_coverage) / n()) |>
    pivot_wider(
      names_from = interval_range,
      values_from = empirical_coverage
    ) |>
    mutate(`90` = `90` - `50`) |>
    pivot_longer(
      cols = c(`50`, `90`),
      names_to = "interval_range",
      values_to = "empirical_coverage"
    ) |>
    mutate(
      interval_range = factor(interval_range, levels = c("90", "50")),
      model = factor(model, levels = c(
        "base",
        "filter weekday small training volume",
        "filter weekday large training volume",
        "GAM",
        "epinowcast",
        "baseline Mellor et al"
      ))
    )


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
    get_plot_theme() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.placement = "outside",
      strip.background = element_rect(color = NA, fill = NA)
    ) +
    scale_fill_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    scale_alpha_manual(
      name = "Empirical coverage",
      values = plot_comps$coverage_alpha
    ) +
    geom_hline(aes(yintercept = 0.50), linetype = "dashed") +
    geom_hline(aes(yintercept = 0.90), linetype = "dashed") +
    labs(
      y = "Empirical coverage", x = "",
      fill = "Model", alpha = "Interval"
    ) +
    ggtitle("Empirical coverage by model")
  if (isTRUE(save)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 12,
      height = 8
    )
  }
  return(p)
}

#' Get a plot of coverage by model and weekday
#'
#' @param all_coverage Dataframe of the combined individual level coverage for
#'    days and intervals.
#' @param intervals Vector of integers to plot coverage of.
#' @inheritParams get_plot_rel_wis_by_age_group
#' @importFrom ggplot2 ggplot geom_bar aes labs scale_alpha_manual
#'    scale_fill_manual geom_hline
#' @importFrom dplyr filter group_by summarise mutate n
#' @importFrom tidyr pivot_wider pivot_longer
#' @autoglobal
#' @returns bar chart
get_plot_cov_by_mod_wday_noro <- function(all_coverage,
                                          intervals = c(50, 90),
                                          fig_file_name = NULL,
                                          fig_file_dir = file.path(
                                            "output",
                                            "figs",
                                            "supp"
                                          ),
                                          save = TRUE) {
  if (save && is.null(fig_file_name)) {
    stop("When `save = TRUE`, `fig_file_name` must be supplied.", call. = FALSE)
  }
  coverage <- filter(
    all_coverage,
    interval_range %in% c(intervals)
  )

  coverage_summarised <- coverage |>
    mutate(
      weekday = wday(reference_date),
      weekday_name = wday(reference_date, label = TRUE)
    ) |>
    group_by(model, weekday, weekday_name, interval_range) |>
    summarise(empirical_coverage = sum(interval_coverage) / n()) |>
    pivot_wider(
      names_from = interval_range,
      values_from = empirical_coverage
    ) |>
    mutate(`90` = `90` - `50`) |>
    pivot_longer(
      cols = c(`50`, `90`),
      names_to = "interval_range",
      values_to = "empirical_coverage"
    ) |>
    mutate(
      interval_range = factor(interval_range, levels = c("90", "50")),
      model = factor(model, levels = c(
        "base",
        "filter weekday small training volume",
        "filter weekday large training volume",
        "GAM",
        "epinowcast",
        "baseline Mellor et al"
      ))
    )


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
    facet_grid(. ~ weekday_name, switch = "x") +
    get_plot_theme() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.placement = "outside",
      strip.background = element_rect(color = NA, fill = NA)
    ) +
    scale_fill_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    scale_alpha_manual(
      name = "Empirical coverage",
      values = plot_comps$coverage_alpha
    ) +
    geom_hline(aes(yintercept = 0.50), linetype = "dashed") +
    geom_hline(aes(yintercept = 0.90), linetype = "dashed") +
    labs(
      y = "Empirical coverage", x = "",
      fill = "Model", alpha = "Interval"
    ) +
    ggtitle("Empirical coverage by model and weekday")
  if (isTRUE(save)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 12,
      height = 8
    )
  }
  return(p)
}
