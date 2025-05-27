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
get_plot_mult_nowcasts <- function(all_nowcasts,
                                   final_summed_data,
                                   nowcast_dates_to_plot = NULL,
                                   pathogen = "",
                                   title = "") {
  final_df <- final_summed_data |>
    filter(
      reference_date >= min(all_nowcasts$reference_date),
      reference_date <= max(all_nowcasts$reference_date)
    )

  if (!is.null(nowcast_dates_to_plot)) {
    all_nowcasts <- all_nowcasts |>
      filter(nowcast_date %in% c(nowcast_dates_to_plot))
  }
  all_nowcasts <- all_nowcasts |>
    mutate(nowcast_date_model = glue("{nowcast_date}-{model}"))

  p <- ggplot(all_nowcasts) +
    geom_ribbon(
      aes(
        x = reference_date, ymin = `q_0.025`, ymax = `q_0.975`,
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
    # nolint start
    scale_color_manual(
      values = c(
        "base" = "darkgreen",
        "KIT simple nowcast" = "orange4"
      )
    ) +
    scale_fill_manual(
      values = c(
        "base" = "darkgreen",
        "KIT simple nowcast" = "orange4"
      )
    ) +
    # nolint end
    xlab("Reference date") +
    ylab(glue("{pathogen} cases")) +
    coord_cartesian(ylim = c(0, 1.01 * max(all_nowcasts$`q_0.975`))) + # nolint
    ggtitle(glue("{title}"))

  return(p)
}


#' Get a plot comparing just the point nowcasts
#'
#' @param pt_nowcasts_combined Dataframe of the point nowcasts from  multiple
#'    models.
#' @param final_summed_data Dataframe of the sum of the counts by reference
#'    date as of the final reference data + the eval time frame.
#' @param nowcast_dates_to_plot Vector of character strings of the dates you
#'   wish to plot, default is `NULL` which will plot all of them
#' @param pathogen Character sting of the pathogen being plotted
#' @param title Character string indicating the title
#'
#' @autoglobal
#' @importFrom ggplot2 aes geom_line ggplot ggtitle xlab ylab theme_bw
#'    theme geom_ribbon geom_point scale_x_date element_text coord_cartesian
#'    geom_vline
#' @importFrom glue glue
#' @importFrom dplyr filter
#' @importFrom lubridate ymd
#' @returns ggplot object
get_plot_pt_nowcasts <- function(pt_nowcasts_combined,
                                 final_summed_data,
                                 nowcast_dates_to_plot = NULL,
                                 pathogen = "",
                                 title = "") {
  final_df <- final_summed_data |>
    filter(
      reference_date >= min(pt_nowcasts_combined$reference_date),
      reference_date <= max(pt_nowcasts_combined$reference_date)
    )

  if (!is.null(nowcast_dates_to_plot)) {
    all_nowcasts <- pt_nowcasts_combined |>
      filter(nowcast_date %in% c(nowcast_dates_to_plot))
  } else {
    all_nowcasts <- pt_nowcasts_combined
  }

  p <- ggplot(all_nowcasts) +
    geom_line(aes(
      x = reference_date, y = predicted,
      group = nowcast_date,
      color = model
    )) +
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
      color = "darkblue", linewidth = 0.7
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
    coord_cartesian(ylim = c(0, 1.1 * max(all_nowcasts$predicted,
      na.rm = TRUE
    ))) +
    ggtitle(glue("{title}"))

  return(p)
}
#' Get a plot of the draws of an individual nowcast
#'
#' @param nowcast_draws Dataframe of the draws of the nowcast with data
#' @param nowcast_target Character string indicating the quantity being
#'    nowcasted
#'
#' @autoglobal
#' @importFrom ggplot2 aes geom_line ggplot ggtitle xlab ylab theme_bw
#'    theme scale_x_date element_text coord_cartesian
#'    geom_vline
#' @importFrom glue glue
#' @returns ggplot object
get_plot_ind_nowcast_draws <- function(nowcast_draws,
                                       nowcast_target = "7-day rolling sum COVID-19 admissions") { # nolint

  p <- ggplot(nowcast_draws) +
    geom_line(aes(x = reference_date, y = total_count, group = draw),
      alpha = 0.2, color = "gray"
    ) +
    geom_line(aes(x = reference_date, y = data_as_of),
      color = "black"
    ) +
    geom_line(aes(x = reference_date, y = observed),
      color = "magenta4"
    ) +
    geom_line(aes(x = reference_date, y = pt_nowcast),
      color = "green"
    ) +
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
    coord_cartesian(ylim = c(0, 1.1 * max(nowcast_draws$total_count,
      na.rm = TRUE
    ))) +
    theme_bw() +
    xlab("Reference date") +
    ylab(glue("{nowcast_target}")) +
    ggtitle("Individual nowcast")


  return(p)
}

#' Get a plot of the quantiles from individual nowcast nowcast draws
#'
#' @param nowcast_draws Dataframe of the draws of the nowcast with data
#' @param nowcast_target Character string indicating the quantity being
#'    nowcasted
#'
#' @autoglobal
#' @importFrom ggplot2 aes geom_line ggplot ggtitle xlab ylab theme_bw
#'    theme scale_x_date element_text coord_cartesian
#'    geom_vline
#' @importFrom glue glue
#' @importFrom dplyr group_by left_join summarise distinct
#' @returns ggplot object
get_plot_ind_nowcast_quantiles <- function(nowcast_draws,
                                           nowcast_target = "7-day rolling sum COVID-19 admissions") { # nolint
  nowcast_quantiles <- nowcast_draws |>
    group_by(reference_date) |>
    summarise(
      q_50 = quantile(total_count, 0.5, na.rm = TRUE),
      q_lb_50th = quantile(total_count, 0.25, na.rm = TRUE),
      q_ub_50th = quantile(total_count, 0.75, na.rm = TRUE),
      q_lb_95th = quantile(total_count, 0.025, na.rm = TRUE),
      q_ub_95th = quantile(total_count, 0.975, na.rm = TRUE)
    ) |>
    left_join(
      distinct(
        nowcast_draws,
        reference_date, observed, data_as_of,
        pt_nowcast
      ),
      by = "reference_date"
    )



  p <- ggplot(nowcast_quantiles) +
    geom_line(aes(x = reference_date, y = q_50),
      color = "darkgreen"
    ) +
    geom_ribbon(
      aes(
        x = reference_date, ymin = q_lb_50th,
        ymax = q_ub_50th
      ),
      fill = "darkgreen", alpha = 0.3
    ) +
    geom_ribbon(
      aes(
        x = reference_date, ymin = q_lb_95th,
        ymax = q_ub_95th
      ),
      fill = "darkgreen", alpha = 0.3
    ) +
    geom_line(aes(x = reference_date, y = pt_nowcast),
      color = "green"
    ) +
    geom_line(aes(x = reference_date, y = data_as_of),
      color = "magenta4"
    ) +
    geom_line(aes(x = reference_date, y = observed),
      color = "black"
    ) +
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
    coord_cartesian(ylim = c(0, 1.1 * max(nowcast_draws$total_count))) +
    theme_bw() +
    xlab("Reference date") +
    ylab(glue("{nowcast_target}")) +
    ggtitle("Individual nowcast")


  return(p)
}
