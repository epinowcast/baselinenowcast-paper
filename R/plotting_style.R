#' Get standardized plot theme to add to figures
#'
#' @returns a theme object to add to a [ggplot2::ggplot()] object
#' @autoglobal
#' @importFrom ggplot2 theme element_rect
#' @importFrom cowplot theme_half_open background_grid
get_plot_theme <- function() {
  plot_theme <- cowplot::theme_half_open() +
    cowplot::background_grid() +
    theme(
      plot.background = element_rect(fill = "white")
    )

  return(plot_theme)
}

#' Get plot components (colors and shapes)
#'
#' @returns a list of the model colors to be passed to `scale_fill_manual` and
#'    `scale_color_manual`
#' @autoglobal
plot_components <- function() {
  # nolint start
  model_colors <- c(
    "KIT simple nowcast" = "darkgreen",
    "baselinenowcast" = "orange4"
  )
  # nolint end

  plot_comp_list <-
    list(model_colors = model_colors)
  return(plot_comp_list)
}
