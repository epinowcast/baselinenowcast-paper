% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures_noro.R
\name{get_plot_mult_nowcasts_noro}
\alias{get_plot_mult_nowcasts_noro}
\title{Get a plot of the nowcasts string together for all dates or a set of dates}
\usage{
get_plot_mult_nowcasts_noro(
  all_nowcasts,
  nowcast_dates_to_plot = NULL,
  facet_title = "",
  pathogen = "Norovirus"
)
}
\arguments{
\item{all_nowcasts}{Dataframe of the quantiled nowcasts (in wide format)}

\item{nowcast_dates_to_plot}{Vector of character strings of the dates you
wish to plot, default is \code{NULL} which will plot all of them}

\item{facet_title}{Character string indicating facet title.}

\item{pathogen}{Character sting of the pathogen being plotted}
}
\value{
ggplot object
}
\description{
Get a plot of the nowcasts string together for all dates or a set of dates
}
