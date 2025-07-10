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
      plot.background = element_rect(fill = "white"),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 18)
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
  pal_weekday <- brewer.pal(7, "Set1")
  pal_mps <- brewer.pal(12, "Paired")
  # nolint start
  model_colors <- c(
    "KIT simple nowcast" = "darkorange",
    "KIT simple nowcast revised" = "darkgreen",
    "baselinenowcast" = "purple4",
    "Default" = "purple4",
    "base" = "purple4",
    "filter weekday small training volume" = "blue3",
    "filter weekday large training volume" = "maroon",
    "GAM" = "orange3",
    "epinowcast" = "green4",
    "baseline Mellor et al" = "navy",
    "baselinenowcast_model1" = "purple4",
    "baselinenowcast_model2" = "blue3",
    "baselinenowcast_model3" = "maroon",
    "baselinenowcast base" = "purple4",
    "baselinenowcast weekday\nfilter small training volume" = "blue3",
    "baselinenowcast weekday\nfilter large training volume" = "maroon"
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
  weekday_colors <- c(
    "Mon" = pal_weekday[1],
    "Tue" = pal_weekday[5],
    "Wed" = pal_weekday[6],
    "Thu" = pal_weekday[3],
    "Fri" =  pal_weekday[2],
    "Sat" = pal_weekday[4],
    "Sun" = pal_weekday[7],
    "All" = "black"
  )
  weekday_linewidth <- c(
    "Mon" = 1,
    "Tue" = 1,
    "Wed" = 1,
    "Thu" = 1,
    "Fri" = 1,
    "Sat" = 1,
    "Sun" = 1,
    "All" = 2
  )
  permutation_colors <- c(
    "Baseline validation approach" = "purple4",
    "Default" = "purple4",
    "Borrowed estimates from all age groups" = pal_mps[4],
    "Complete reporting triangle" = pal_mps[8],
    "Increased delay estimate" = pal_mps[1],
    "Increased uncertainty estimate" = pal_mps[2],
    "Reduced delay estimate" = "maroon",
    "Reduced uncertainty estimate" = "palevioletred",
    "Delay:180,\nUncertainty:60" = pal_mps[1],
    "Delay:41,\nUncertainty:19" = pal_mps[5],
    "Delay:50,\nUncertainty:10" = pal_mps[6],
    "Delay:60,\nUncertainty:180" = pal_mps[2]
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
    "90" = 0.2,
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
      weekday_colors = weekday_colors,
      weekday_linewidth = weekday_linewidth
    )
  return(plot_comp_list)
}
