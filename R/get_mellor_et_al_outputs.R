#' Placeholder function for extracting the outputs from the Norovirus paper
#'
#' @param data_from_bnc Data.frame of the output from baselinenowcast model
#'    permutations run locally
#' @param days_to_eval Integer indicating the number of days before the nowcast
#'    date to evaluate against.
#' @param local_load Boolean indicating whether or not to load data from local
#'    or from GitHUb. Default is FALSE which loads from GitHub.
#' @param synethetic_data Boolean indicating whether to use the synthetic or\
#'    or real data. Default is `synthetic`.
#' @param model_names Vector of character strings indicating the names of the
#'    models to include within the file names. Defaults are the three different
#'    models `c("epinowcast", "gam", "baseline_prev_week")`.
#' @autoglobal
#' @importFrom dplyr mutate case_when
#' @returns Data.frame labeled with models from Mellor et al
get_mellor_et_al_outputs <- function(
    data_from_bnc,
    days_to_eval,
    local_load = FALSE,
    synthetic_data = TRUE,
    model_names = c("epinowcast", "gam", "baseline_prevweek")) {
  if (isTRUE(local_load)) {
    prefix <- ("output/data/noro")
  } else {
    prefix <- "https://raw.githubusercontent.com/jonathonmellor/norovirus-nowcast-baselinenowcast/refs/heads/initial-port/outputs/data/"
  }
  if (isTRUE(synthetic_data)) {
    data_type <- "synthetic"
  } else {
    data_type <- "original"
  }

  just_data <- data_from_bnc |>
    distinct(nowcast_date, reference_date, observed, data_as_of) |>
    mutate(
      nowcast_date = as.Date(nowcast_date),
      reference_date = as.Date(reference_date)
    )

  mellor_data_matched <- data.frame()
  for (i in 1:length(model_names)) {
    model_name <- model_names[i]
    mellor_data <- read_csv(file.path(
      prefix,
      data_type,
      glue("{model_name}_predictions_summary.csv")
    ))

    # Filter to only 7 days per evaluation time frame
    mellor_data <- mellor_data |>
      mutate(horizon = as.integer(prediction_end_date - specimen_date)) |>
      filter(
        horizon < days_to_eval,
        horizon >= 0
      )

    data_to_match <- mellor_data |>
      rename(
        nowcast_date = prediction_end_date,
        reference_date = specimen_date,
        `q_0.5` = pi_50,
        `q_0.05` = pi_5,
        `q_0.25` = pi_25,
        `q_0.75` = pi_75,
        `q_0.95` = pi_95
      ) |>
      filter(
        nowcast_date >= min(just_data$nowcast_date),
        nowcast_date <= max(just_data$nowcast_date),
        t_aggregation == "daily"
      ) |>
      mutate(
        n_history_delay =
          case_when(
            model_name == "baselinenowcast_model1" ~ 28,
            model_name == "baselinenowcast_model2" ~ 11 * 7,
            model_name == "baselinenowcast_model3" ~ 28 * 7,
            TRUE ~ NA
          ),
        n_history_uncertainty =
          case_when(
            model_name == "baselinenowcast_model1" ~ 28,
            model_name == "baselinenowcast_model2" ~ 4,
            model_name == "baselinenowcast_model3" ~ 28 * 7,
            TRUE ~ NA
          )
      ) |>
      left_join(just_data, by = c("reference_date", "nowcast_date")) |>
      select(colnames(data_from_bnc))

    mellor_data_matched <- bind_rows(mellor_data_matched, data_to_match)
  }

  data <- mellor_data_matched |>
    mutate(
      model = ifelse(model == "Baseline: previous week",
        "baseline Mellor et al",
        model
      )
    )
  return(data)
}
