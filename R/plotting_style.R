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
#' @importFrom RColorBrewer brewer.pal
plot_components <- function() {
  pal_age_groups <- brewer.pal(6, "Spectral")
  # nolint start
  model_colors <- c(
    "KIT simple nowcast" = "darkgreen",
    "baselinenowcast" = "purple4"
  )
  age_colors <- c(
    "00+" = "black",
    "00-04" = pal_age_groups[1],
    "05-14" = pal_age_groups[2],
    "15-34" = pal_age_groups[3],
    "35-59" = pal_age_groups[4],
    "60-79" = pal_age_groups[5],
    "80+" = pal_age_groups[6]
  )
  age_linewidth <- c(
    "00+" = 2,
    "00-04" = 1,
    "05-14" = 1,
    "15-34" = 1,
    "35-59" = 1,
    "60-79" = 1,
    "80+" = 1
  )
  # nolint end

  plot_comp_list <-
    list(
      model_colors = model_colors,
      age_colors = age_colors,
      age_linewidth = age_linewidth
    )
  return(plot_comp_list)
}
