get_plot_data_as_of <- function(as_of_dfs) {
  final_df <- as_of_dfs |>
    filter(as_of_date == max(as_of_date))

  plot <- ggplot(as_of_dfs) +
    geom_line(aes(
      x = reference_date, y = observed, color =
        as.factor(as_of_date)
    )) +
    geom_line(
      data = final_df,
      aes(x = reference_date, y = observed),
      color = "black"
    ) +
    xlab("") +
    ylab("Cases") +
    theme_bw() +
    ggtitle("Measles cases as of different nowcasting days")

  return(plot)
}
