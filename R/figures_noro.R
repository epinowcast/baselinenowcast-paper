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
      alpha = 0.3
    ) +
    geom_ribbon(
      aes(
        x = reference_date, ymin = `q_0.25`, ymax = `q_0.75`,
        group = nowcast_date_model, fill = model
      ),
      alpha = 0.3
    ) +
    geom_line(aes(
      x = reference_date, y = `q_0.5`,
      group = nowcast_date_model, color = model
    )) +
    geom_line(
      aes(
        x = reference_date,
        y = data_as_of
      ),
      color = "gray"
    ) +
    geom_line(
      aes(x = reference_date, y = observed),
      color = "red"
    ) +
    theme_bw() +
    facet_wrap(~model_type, nrow = 2) +
    scale_x_date(
      date_breaks = "1 week",
      date_labels = "%Y-%m-%d"
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
    # nolint end
    xlab("") +
    ylab(glue("{pathogen} cases")) +
    coord_cartesian(ylim = c(0, 150)) + # nolint
    ggtitle(glue("{title}"))

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
      strip.background = element_rect(color = NA, fill = NA),
      legend.title = element_blank()
    ) +
    facet_grid(. ~ model_type, switch = "x") +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_colors$score_alpha
    ) +
    scale_fill_manual(
      name = "Model",
      values = plot_colors$model_colors
    ) +
    ggtitle("Overall WIS")
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
      name = "",
      values = plot_comps$model_colors
    ) +
    scale_x_date( # Jan 2023, Feb 2023, etc.
      date_breaks = "2 weeks" # Break every month
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    labs(x = "", y = "Relative WIS") +
    ggtitle("Relative WIS over timeelative to baselinenowcast default model configuration") # nolint
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
#' @importFrom dplyr select filter rename mutate
get_plot_rel_wis_by_weekday <- function(scores) {
  scores_sum <- scores |>
    mutate(
      weekday_name = wday(reference_date, label = TRUE),
      weekday = wday(reference_date, label = FALSE)
    ) |>
    scoringutils::summarise_scores(by = c("model", "weekday", "weekday_name")) |>
    select(model, weekday, weekday_name, wis)

  baseline_comparison <- scores_sum |>
    filter(model == "base") |>
    rename(comparison_wis = wis) |>
    select(comparison_wis, weekday)

  relative_wis <- scores_sum |>
    filter(model != "base") |>
    left_join(baseline_comparison, by = c("weekday")) |>
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
    ggtitle("Relative WIS over timeelative to baselinenowcast default model configuration") # nolint
  return(p)
}

#' Get plot mean delay over time by weekday
#'
#' @param delay_dfs Data.frame of delay estimates both separately by weekday
#'    and jointly, indexed by the weekday of the reference date.
#'
#' @returns ggplot object
#' @importFrom ggplot2 ggplot aes labs
#'    facet_grid theme scale_fill_manual
#'    ggtitle element_blank scale_alpha_manual geom_bar
#' @importFrom dplyr select filter rename mutate
get_plot_mean_delay_over_time_by_weekday <- function(delay_dfs) {
  mean_delay_by_weekday_and_date <- delay_dfs |>
    filter(
      n_history_delay == 28,
      filter_ref_dates == TRUE
    ) |>
    group_by(nowcast_date, weekday, weekday_name) |>
    summarise(
      mean_delay = sum(delay * delay_time)
    )

  mean_delay_all_weekdays <- delay_dfs |>
    filter(
      n_history_delay == 28,
      filter_ref_dates == FALSE
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
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(
      name = "Weekday",
      values = plot_comps$weekday_colors
    ) +
    scale_linewidth_manual(
      values = plot_comps$weekday_linewidth,
      labels = NULL
    ) +
    xlab("") +
    ylab("Mean delay over time")
}

#' Get a plot of the cdf colored by weekday
#'
#' @inheritParams get_plot_mean_delay_over_time_by_weekday
#' @importFrom ggplot2 ggplot aes labs
#'    facet_grid theme scale_fill_manual
#'    ggtitle element_blank scale_alpha_manual geom_bar
#' @importFrom dplyr select filter rename mutate
#' @returns ggplot object
get_plot_cdf_by_weekday <- function(delay_dfs) {
  avg_cdf_by_weekday <- delay_dfs |>
    filter(
      n_history_delay == 28,
      filter_ref_dates == TRUE
    ) |>
    group_by(weekday_name, delay_time) |>
    summarise(mean_delay = mean(delay)) |>
    mutate(cdf = cumsum(mean_delay))

  avg_cdf_overall <- delay_dfs |>
    filter(
      n_history_delay == 28,
      filter_ref_dates == FALSE
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
      aes(x = delay_time, y = cdf, color = weekday_name, linewidth = weekday_name)
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
    get_plot_theme()

  return(p)
}
