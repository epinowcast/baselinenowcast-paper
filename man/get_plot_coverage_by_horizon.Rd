% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures_hub_validation.R
\name{get_plot_coverage_by_horizon}
\alias{get_plot_coverage_by_horizon}
\title{Get a plot of coverage by horizon for each model}
\usage{
get_plot_coverage_by_horizon(all_coverage, strata, intervals = c(50, 95))
}
\arguments{
\item{all_coverage}{Dataframe of the combined individual level coverage for
days and intervals.}

\item{strata}{Character string indicating which strata to summarize over.}

\item{intervals}{Vector of integers to plot coverage of.}
}
\value{
bar chart
}
\description{
Get a plot of coverage by horizon for each model
}
