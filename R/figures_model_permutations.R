#' Get a plot of the nowcasts over time faceted by model variation, colored
#' by the type of permutation
#'
#' Hub validation figure A
#'
#' @param combined_nowcasts Dataframe of the combined quantiles across
#'    horizons and nowcast dates for all the model permutations.
#' @param horizon_to_plot Integer indicating which horizon to plot
#' @param age_group_to_plot Character string indicating which age group to
#'    show in the plot, default is "00+" for all age groups.
#' @importFrom glue glue
#' @importFrom ggplot2 aes ggplot ggtitle xlab ylab geom_line geom_ribbon
#'    facet_wrap scale_color_manual scale_fill_manual
#' @importFrom dplyr filter
#' @returns ggplot object
#' @autoglobal
get_plot_nowcast_perms_over_time <- function(combined_nowcasts,
                                             horizon_to_plot,
                                             age_group_to_plot = "00+") {
  nc <- filter(
    combined_nowcasts,
    horizon == horizon_to_plot,
    age_group == age_group_to_plot
  )

  nc_base <- nc |> filter(model_variation == "Baseline validation")

  nc_perms <- nc |>
    filter(model_variation != "Baseline validation") |>
    bind_rows(nc_base |> mutate(model_variation = "Borrow for delay and uncertainty estimation")) |>
    bind_rows(nc_base |> mutate(model_variation = "Reporting triangle completeness")) |>
    bind_rows(nc_base |> mutate(model_variation = "Training volume"))

  plot_colors <- plot_components()
  p <- ggplot(nc_perms) +
    geom_line(aes(
      x = reference_date, y = `q_0.5`,
      color = model_variation_string
    ), show.legend = FALSE) +
    geom_ribbon(
      aes(
        x = reference_date,
        ymin = `q_0.25`,
        ymax = `q_0.75`,
        fill = model_variation_string
      ),
      alpha = 0.3
    ) +
    geom_ribbon(
      aes(
        x = reference_date,
        ymin = `q_0.025`,
        ymax = `q_0.975`, fill = model_variation_string
      ),
      alpha = 0.3
    ) +
    geom_line(aes(x = reference_date, y = observed),
      color = "red"
    ) +
    geom_line(aes(x = reference_date, y = data_as_of),
      color = "gray", linetype = "dashed"
    ) +
    facet_wrap(~model_variation, nrow = 3) +
    get_plot_theme() +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(values = plot_colors$permutation_colors) +
    scale_fill_manual(values = plot_colors$permutation_colors) +
    labs(fill = "Model permutation") +
    ggtitle(glue("Horizon: {-horizon_to_plot} days, strata: {age_group_to_plot} age group")) + # nolint
    xlab("") +
    ylab("7-day hospitalisation incidence")



  return(p)
}
