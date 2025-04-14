#' Get a plot of the data as of different dates
#'
#' @param final_df Dataframe of the latest data by reference and report date
#' @param as_of_dates Vector of character strings of the dates you wish to plot
#' @param pathogen Character sting of the pathogen being plotted
#'
#' @autoglobal
#' @importFrom ggplot2 aes geom_line ggplot ggtitle xlab ylab theme_bw
#' @importFrom glue glue
#' @importFrom dplyr group_by summarise ungroup bind_rows
#' @returns ggplot object
get_plot_data_as_of <- function(final_df,
                                as_of_dates,
                                pathogen = "") {
  sum_df <- final_df |>
    group_by(reference_date) |>
    summarise(
      observed = sum(count, na.rm = TRUE)
    ) |>
    ungroup()

  as_of_dfs <- data.frame()
  for (i in seq_along(as_of_dates)) {
    as_of_df <- get_eval_data_from_long_df(
      final_df,
      as_of_dates[i]
    )
    as_of_dfs <- bind_rows(as_of_dfs, as_of_df)
  }

  plot_df <- ggplot(as_of_dfs) +
    geom_line(aes(
      x = reference_date, y = observed, color =
        as.factor(as_of_date)
    )) +
    geom_line(
      data = sum_df,
      aes(x = reference_date, y = observed),
      color = "black"
    ) +
    xlab("") +
    ylab("Cases") +
    theme_bw() +
    ggtitle(glue("{pathogen} cases as of different nowcasting days"))

  return(plot_df)
}

#' Get a plot of the nowcasts string together for all dates or a set of dates
#'
#' @param all_nowcasts Dataframe of the quantiled nowcasts (in wide format)
#' @param final_summed_data Dtaaframe of the sum of the counts by reference
#'    date as of the final reference data + the eval time frame.
#' @param nowcast_dates_to_plot Vector of character strings of the dates you
#'   wish to plot, default is `NULL` which will plot all of them
#' @param pathogen Character sting of the pathogen being plotted
#'
#' @autoglobal
#' @importFrom ggplot2 aes geom_line ggplot ggtitle xlab ylab theme_bw
#'    theme geom_ribbon geom_point scale_x_date element_text
#' @importFrom glue glue
#' @importFrom dplyr filter
#' @returns ggplot object
get_plot_mult_nowcasts <- function(all_nowcasts,
                                   final_summed_data,
                                   nowcast_dates_to_plot = NULL,
                                   pathogen = "") {
  final_df <- final_summed_data |>
    filter(
      reference_date >= min(all_nowcasts$reference_date),
      reference_date <= max(all_nowcasts$reference_date)
    )

  if (!is.null(nowcast_dates_to_plot)) {
    all_nowcasts <- all_nowcasts |>
      filter(nowcast_date %in% c(nowcast_dates_to_plot))
  }

  p <- ggplot(all_nowcasts) +
    geom_ribbon(
      aes(
        x = reference_date, ymin = `q_0.025`, ymax = `q_0.975`,
        group = nowcast_date
      ),
      alpha = 0.3, fill = "darkgreen"
    ) +
    geom_ribbon(
      aes(
        x = reference_date, ymin = `q_0.25`, ymax = `q_0.75`,
        group = nowcast_date
      ),
      alpha = 0.3, fill = "darkgreen"
    ) +
    geom_line(aes(
      x = reference_date, y = `q_0.5`,
      group = nowcast_date
    ), color = "darkgreen") +
    geom_line(
      aes(
        x = reference_date,
        y = data_as_of, group = nowcast_date
      ),
      color = "magenta4"
    ) +
    geom_line(
      aes(
        x = reference_date,
        y = data_as_of, group = nowcast_date
      ),
      color = "magenta4"
    ) +
    geom_point(
      aes(x = reference_date, y = observed),
      color = "darkblue"
    ) +
    geom_line(
      data = final_df,
      aes(x = reference_date, y = observed), color = "black"
    ) +
    geom_vline(aes(xintercept = ymd(nowcast_date)), linetype = "dashed") +
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
      )
    ) +
    xlab("Reference date") +
    ylab(glue("{pathogen} cases")) +
    coord_cartesian(ylim = c(0, 1.1 * max(final_df$observed)))

  return(p)
}
