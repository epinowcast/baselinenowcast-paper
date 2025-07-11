% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures_model_permutations.R
\name{make_panel_A_model_perms}
\alias{make_panel_A_model_perms}
\title{Make a patchwork plot for panel A combining facets and underlays}
\usage{
make_panel_A_model_perms(
  plot_nowcasts_t_mp_borrow,
  rel_wis_over_time_mp_borrow,
  plot_nowcasts_t_mp_rep_tri,
  rel_wis_over_time_mp_rep_tri,
  plot_nowcasts_t_mp_volume,
  rel_wis_over_time_mp_volume,
  fig_file_name,
  fig_file_dir = file.path("output", "figs"),
  save = FALSE
)
}
\arguments{
\item{plot_nowcasts_t_mp_borrow}{larger nowcasts over time}

\item{rel_wis_over_time_mp_borrow}{underlay relative WIS}

\item{plot_nowcasts_t_mp_rep_tri}{larger nowcasts over time}

\item{rel_wis_over_time_mp_rep_tri}{underlay relative WIS}

\item{plot_nowcasts_t_mp_volume}{larger nowcasts over time}

\item{rel_wis_over_time_mp_volume}{underlay relative WIS}

\item{fig_file_name}{Character string indicating name of the figure to be
saved as the file name}

\item{fig_file_dir}{Path to save figure. Default is
\code{file.path("output", "figs")}.}

\item{save}{Boolean indicating whether or not to save the figure to disk.
Default is \code{FALSE}.}
}
\value{
patchwork ggplot object
}
\description{
Make a patchwork plot for panel A combining facets and underlays
}
