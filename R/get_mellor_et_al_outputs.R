#' Placeholder function for extracting the outputs from the Norovirus papaer
#'
#' @param data_from_bnc Data.frame of the output from baselinenowcast
#' @param models Vector of character strings indicating which models from
#'    Mellor et al to include. Default is
#'    `c("epinowcast", "GAM", "baseline Mellor et al")`.
#' @autoglobal
#' @importFrom dplyr mutate case_when
#' @returns Data.frame labeled with models from Mellor et al
get_mellor_et_al_outputs <- function(
    data_from_bnc,
    models = c("epinowcast", "GAM", "baseline Mellor et al")) {
  fake_data <- data_from_bnc |>
    filter(model %in% models) |>
    mutate(
      model = case_when(
        model == "base" ~ "baseline Mellor et al",
        model == "filter weekday large training volume" ~ "epinowcast",
        model == "filter weekday small training volume" ~ "GAM"
      )
    )
  return(fake_data)
}
