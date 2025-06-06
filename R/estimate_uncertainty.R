#' Estimate uncertainty using baselinenowcast functions
#'
#' @param triangle_for_uncertainty Matrix of reporting triangle to use for
#'    uncertainty estimation.
#' @param n_history_uncertainty Integer indicating number of retrospective
#'   nowcast dates to use for uncertainty estimation
#' @param n_history_delay Integer indicating number of reference times to use
#'   for delay estimation
#' @param fun_to_aggregate Function for aggregating the target data, default
#'    is `sum`.
#' @param k Integer indicating window size for the aggregate function to
#'   operate over, default is `7`.
#' @importFrom baselinenowcast truncate_triangles generate_triangles
#'   generate_pt_nowcast_mat_list estimate_dispersion
#' @returns Vector of dispersion parameters
estimate_uncertainty <- function(triangle_for_uncertainty,
                                 n_history_uncertainty,
                                 n_history_delay,
                                 fun_to_aggregate = sum,
                                 k = 7) {
  # Input validation
  if (!is.matrix(triangle_for_uncertainty)) {
    stop("triangle_for_uncertainty must be a matrix")
  }
  if (!is.numeric(n_history_uncertainty) || n_history_uncertainty <= 0) {
    stop("n_history_uncertainty must be a positive integer")
  }
  if (!is.numeric(n_history_delay) || n_history_delay <= 0) {
    stop("n_history_delay must be a positive integer")
  }
  if (!is.function(fun_to_aggregate)) {
    stop("fun_to_aggregate must be a function")
  }
  if (!is.numeric(k) || k <= 0) {
    stop("k must be a positive integer")
  }

  truncated_rts <- truncate_triangles(
    reporting_triangle = triangle_for_uncertainty,
    n = n_history_uncertainty
  )

  retro_rts <- generate_triangles(
    trunc_rep_tri_list = truncated_rts
  )

  retro_nowcasts <- generate_pt_nowcast_mat_list(
    reporting_triangle_list = retro_rts,
    n = n_history_delay
  )

  disp_params <- estimate_dispersion(
    pt_nowcast_mat_list = retro_nowcasts,
    trunc_rep_tri_list = truncated_rts,
    reporting_triangle_list = retro_rts,
    fun_to_aggregate = fun_to_aggregate,
    k = k
  )
  return(disp_params)
}
