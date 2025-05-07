#' Get the url to load in the KIT nowcast data
#'
#' @param prefix Character string indicating the github url up until the date
#'    appears.
#' @param nowcast_date Character string indicating the nowcast date.
#'
#' @return `kit_url` Character string indicating the url for the nowcast on that nowcast
#'    date.
get_kit_nowcast_url <- function(prefix, nowcast_date) {
  kit_url <- glue::glue("{prefix}/{nowcast_date}-KIT-simple_nowcast.csv")
  return(kit_url)
}
