#' Get COVID data from German Nowcast Hub
#'
#' @param url String of url for raw data
#' @param loc_to_subs String indicating the location to subset the data to,
#'    default is "DE" which will get the national data for all age groups.
#' @return Data.frame of counts of cases by reference date and report date for
#'    only Germany and
get_covid_data <- function(url,
                           loc_to_subset = "DE") {
  raw_data <- readr::read_csv(config$covid$url)

  raw_data_long <- tidyr::pivot_longer(
    raw_data,
    cols = starts_with("value_"),
    names_to = "delay",
    values_to = "count",
    names_prefix = "value_"
  ) |>
    mutate(delay = as.integer(gsub("d.*$", "", delay))) |>
    mutate(report_date = date + days(delay)) |>
    rename(reference_date = date) |>
    filter(location == {{ loc_to_subset }})
  return(raw_data_long)
}
