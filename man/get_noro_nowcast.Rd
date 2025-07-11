% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_noro_nowcast_pipeline.R
\name{get_noro_nowcast}
\alias{get_noro_nowcast}
\title{Get norovirus nowcast}
\usage{
get_noro_nowcast(
  long_df,
  nowcast_date,
  max_delay,
  n_draws,
  n_history_delay,
  n_history_uncertainty,
  structure = 1
)
}
\arguments{
\item{long_df}{Dataframe of the latest data by reference and
report date, may or may not contain days for every reference date}

\item{nowcast_date}{Character string in YYYY-MM-DD format indicating the
date of the nowcast.}

\item{max_delay}{Integer indicating the maximum delay.}

\item{n_draws}{Integer indicating number of draws to sample.}

\item{n_history_delay}{Integer indicating number of reference times used for
delay estimate.}

\item{n_history_uncertainty}{Integer indicating number of retrospective
nowcast datasets to use for uncertainty estimation}

\item{structure}{Integer or vector specifying the reporting structure.
If integer, divides columns evenly by that integer (with last possibly
truncated).  If vector, the sum must not be greater than or equal to the
number of columns. Default is 1 (standard triangular structure).}
}
\value{
Data.frame of nowcast draws joined to training data.
}
\description{
Get norovirus nowcast
}
