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
      values = plot_colors$model_colors
    ) +
    scale_fill_manual(
      values = plot_colors$model_colors
    ) +
    # nolint end
    xlab("") +
    ylab(glue("{pathogen} cases")) +
    coord_cartesian(ylim = c(0, 150)) + # nolint
    ggtitle(glue("{title}"))

  return(p)
}
