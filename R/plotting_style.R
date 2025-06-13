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
  pal_mps <- brewer.pal(6, "Set2")
  # nolint start
  model_colors <- c(
    "KIT simple nowcast" = "darkgreen",
    "KIT simple nowcast revised" = "darkorange",
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
  permutation_colors <- c(
    "Baseline validation approach" = "black",
    "Borrowed estimates from all age groups" = pal_mps[1],
    "Complete reporting triangle" = pal_mps[2],
    "Delay:180,\nUncertainty:60" = pal_mps[3],
    "Delay:41,\nUncertainty:19" = pal_mps[4],
    "Delay:50,\nUncertainty:10" = pal_mps[5],
    "Delay:60,\nUncertainty:180" = pal_mps[6]
  )

  permutation_linetype <- c(
    "Baseline validation" = "solid",
    "Training volume" = "dotted",
    "Reporting triangle completeness" = "dotdash",
    "Borrow for delay and uncertainty estimation" = "longdash"
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
  coverage_alpha <- c(
    "95" = 0.2,
    "50" = 0.8
  )
  score_alpha <- c(
    "overprediction" = 0.5,
    "underprediction" = 0.8,
    "dispersion" = 0.2
  )
  # nolint end

  plot_comp_list <-
    list(
      model_colors = model_colors,
      age_colors = age_colors,
      age_linewidth = age_linewidth,
      score_alpha = score_alpha,
      coverage_alpha = coverage_alpha,
      permutation_colors = permutation_colors,
      permutation_linetype = permutation_linetype
    )
  return(plot_comp_list)
}
