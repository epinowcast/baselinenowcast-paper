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
#' @returns ggplot obejct
get_plot_data_as_of <- function(final_df,
                                as_of_dates,
                                pathogen = "") {
  sum_df <- final_df |>
    group_by(reference_date) |>
    summarise(
      observed = sum(confirm, na.rm = TRUE)
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
