% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures_hub_validation.R
\name{get_plot_mean_wis_by_horizon}
\alias{get_plot_mean_wis_by_horizon}
\title{Get a plot of mean WIS by horizon}
\usage{
get_plot_mean_wis_by_horizon(scores, strata)
}
\arguments{
\item{scores}{Dataframe of all the scores by individual reference and
nowcast dates and model and age groups}

\item{strata}{Character string indicating whether to summarise across the
different age strata (\code{"age groups"}) or across the nation as a whole
(\code{"national"}). Default is \code{"age groups"}.}
}
\value{
ggplot object
}
\description{
Get a plot of mean WIS by horizon
}
