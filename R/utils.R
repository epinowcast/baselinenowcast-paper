#' Suppress output and messages for code.
#' @param code Code to run quietly.
#' @return The result of running the code.
#' @export
#' @examples
#' result <- quiet(message("This message should be suppressed"))
#' print(result)
quiet <- function(code) {
  sink(nullfile())
  on.exit(sink())
  return(suppressMessages(code))
}

#' Get all the breaks for Sundays
#'
#' @param start_date first date
#' @param end_date last date
#'
#' @returns vector of Sunday dates
get_sunday_breaks <- function(start_date, end_date) {
  # Find first Sunday in range (wday 1 = Sunday when week_start = 7)
  first_sunday <- start_date + (1 - wday(start_date, week_start = 7)) %% 7
  sunday_dates <- seq(first_sunday, end_date, by = "1 week")
  return(sunday_dates)
}
