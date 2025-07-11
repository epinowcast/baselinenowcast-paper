% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures_eda.R
\name{get_plot_pt_nowcasts}
\alias{get_plot_pt_nowcasts}
\title{Get a plot comparing just the point nowcasts}
\usage{
get_plot_pt_nowcasts(
  pt_nowcasts_combined,
  final_summed_data,
  nowcast_dates_to_plot = NULL,
  pathogen = "",
  title = "",
  facet = FALSE
)
}
\arguments{
\item{pt_nowcasts_combined}{Dataframe of the point nowcasts from  multiple
models.}

\item{final_summed_data}{Dataframe of the sum of the counts by reference
date as of the final reference data + the eval time frame.}

\item{nowcast_dates_to_plot}{Vector of character strings of the dates you
wish to plot, default is \code{NULL} which will plot all of them}

\item{pathogen}{Character sting of the pathogen being plotted}

\item{title}{Character string indicating the title}

\item{facet}{Boolean indicating whether or not to make separate facets
of each model}
}
\value{
ggplot object
}
\description{
Get a plot comparing just the point nowcasts
}
