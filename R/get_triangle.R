#' Get reporting triangle
#'
#' @param long_df Dataframe with all age groups, reference dates, and report
#' @param nowcast_date Character string indicating the date of the nowcast
#' @param max_delay Integer indicating the maximum delay
#' @param age_group Character string indicating the age group for this triangle
#' @param partial_rep_tri Boolean indicating whether to use the partial
#'    reporting triangle or only pull from complete reporting matrix. Default
#'    is `TRUE`.
#'
#' @importFrom dplyr filter select
#' @importFrom lubridate ymd days
#' @returns `triangle` Matrix of reporting triangle
get_triangle <- function(long_df,
                         nowcast_date,
                         max_delay,
                         age_group,
                         partial_rep_tri = TRUE) {
  df_filtered <- long_df |>
    filter(age_group == !!age_group)

  triangle_raw <- get_rep_tri_from_long_df(
    long_df = df_filtered,
    nowcast_date = nowcast_date,
    max_delay = max_delay
  )
  if (!partial_rep_tri) {
    triangle <- triangle_raw |>
      filter(reference_date <= ymd(nowcast_date) - days(max_delay))
  } else {
    triangle <- triangle_raw
  }
  triangle_matrix <- triangle |>
    select(
      -reference_date, -nowcast_date
    ) |>
    as.matrix()

  return(triangle_matrix)
}
