get_plot_data_as_of <- function(final_df,
                                as_of_dates) {
  sum_df <- final_df |>
    group_by(reference_date) |>
    summarise(
      observed = sum(confirm)
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

  plot <- ggplot(as_of_dfs) +
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
    ggtitle("Measles cases as of different nowcasting days")

  return(plot)
}
