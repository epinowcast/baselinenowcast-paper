% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_noro_nowcast_pipeline.R
\name{run_noro_nowcast_pipeline}
\alias{run_noro_nowcast_pipeline}
\title{Run noro nowcast pipeline}
\usage{
run_noro_nowcast_pipeline(
  noro_df,
  nowcast_date,
  max_delay,
  filter_ref_dates,
  n_history_delay,
  n_history_uncertainty,
  eval_timeframe,
  days_to_eval,
  n_draws,
  quantiles
)
}
\arguments{
\item{noro_df}{Dataframe of the latest data by reference and
report date}

\item{nowcast_date}{Character string in YYYY-MM-DD format indicating the
date of the nowcast.}

\item{max_delay}{Integer indicating the maximum delay.}

\item{filter_ref_dates}{Boolean indicating whether or not to make separate
delay estimates by weekday.}

\item{n_history_delay}{Integer indicating number of reference times used for
delay estimate.}

\item{n_history_uncertainty}{Integer indicating number of retrospective
nowcast datasets to use for uncertainty estimation}

\item{eval_timeframe}{Integer indicating the number of days after the
nowcast date to evaluate the nowcasts.}

\item{days_to_eval}{Integer indicating the number of days before the nowcast
date to evaluate against.}

\item{n_draws}{Integer indicating number of draws to sample.}

\item{quantiles}{Vector of quantiles to generate.}
}
\value{
Data.frame of nowcast draws with training data and metadata.
}
\description{
Run noro nowcast pipeline
}
