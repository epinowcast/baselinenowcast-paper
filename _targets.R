library(targets)
library(tarchetypes)
library(crew)
library(RcppTOML)
library(readr)
library(here)
library(purrr)

controller <- crew_controller_local(
  workers = 8,
  seconds_idle = 600
)

functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)
rm("functions")

tar_option_set(
  packages = c(
    "tibble", "dplyr", "lubridate",
    "targets", "ggplot2",
    "baselinenowcast",
    "readr", "tidyr",
    "epinowcast"
  ),
  workspace_on_error = TRUE,
  # Run with a pre-specified crew controller
  controller = controller,
  # Setup storage on workers vs on the main node.
  memory = "transient",
  storage = "worker",
  retrieval = "worker",
  format = "parquet", # default storage format
  error = NULL # tells errored targets to return NULL rather than
  # have whole pipeline fail
)

config <- yaml::read_yaml(file.path(
  "input", "config", "config.yaml"
))

# Methods---------------------------------------------------------------------

# We'll start by gathering all of the datasets and model specifications that we
# need for each of the component analyses

## Real-world data from German Nowcast Hub------------------------------------

### Create long tidy dataframe as of final date from German Nowcast Hub------
# This will be used to generate reporting triangles as of nowcast dates
# and to generate evaluation data (triangle as of nowcast date + eval time)
data_targets <- list(

  ### Model specification for German nowcast data------------------------------
  # Specify the maximum delay, number of reference times to use for delay
  # estimation, number of reporting triangles to use for dispersion estimate,
  # and whether or not delay estimates/dispersion parameters are being borrowed

  ## Simulated data, modified from German Nowcast Hub----------------------------
  ### Create simulated long tidy dataframes from data above--------------------
  # This will be for two age strata only, and with a long and short
  # delay, creating 4 distinct datasets. Will be used to generate reporting
  # triangles and evaluation data

  ### Model specification(s) for simulated data--------------------------------
  # Make all combinations of varying:
  # - max delay
  # - borrowing for delay estimation
  # - borrowing for dispersion estimate
  # - number of observations for delay estimation
  # - number of reporting triangles for dispersion estimate

  ## Real-world case data: Measles and norovirus-------------------------------

  ### Load and clean measles data----------------------------------------------
  tar_target(
    name = measles_line_list,
    command = read.delim(config$measles_url, sep = "") |> # nolint
      mutate(
        reference_date = ymd(onset.date),
        report_date = ymd(report.date)
      ) |>
      select(reference_date, report_date)
  ),

  ### Create long tidy dataframe up until final date---------------------------
  # Will be used to create reporting triangles as of nowcast dates +
  # evaluation data. Contains all info in reporting triangles
  tar_target(
    name = measles_long,
    command = measles_line_list |>
      group_by(reference_date, report_date) |>
      summarise(confirm = n()) |>
      ungroup() |>
      complete(
        reference_date = seq(
          from = min(reference_date),
          to = max(reference_date),
          by = "day"
        ),
        report_date = seq(
          from = min(reference_date),
          to = max(report_date),
          by = "days"
        )
      ) |>
      mutate(confirm = ifelse(is.na(confirm), 0, confirm))
  ),

  ### Load and clean norovirus data to create long tidy dataframe--------------
  tar_target(
    name = noro_long,
    command = readr::read_csv(config$norovirus_url) |>
      mutate(
        reference_date = specimen_date,
        report_date = specimen_date + days(days_to_reported)
      ) |>
      rename(confirm = target) |>
      select(reference_date, report_date, confirm)
  ),

  ### Model specification for each data set------------------------------------
  # Based off of the specific dataset, make a single choice about specification
  tar_target(
    name = measles_spec,
    command = list(
      max_delay = 50,
      n_history_delay = 52,
      n_history_dispersion = 50,
      borrow_delay = FALSE,
      borrow_dispersion = FALSE
    ),
    format = "rds"
  )
) # end data_targets


# Results-------------------------------------------------------------------

## Run real-world German Nowcast Hub case study-----------------------------
### Loop over each nowcast date and strata ----------------------------------
# 1. Generate nowcasts and aggregate (baselinenowcast pipeline)
# 2. Save quantiled nowcasts for visualisation

## Run simulated long delays and spare data----------------------------------
### Loop over each nowcast date, strata, data scenario, and model spec-------
# 1. Generate nowcasts and aggregate (baselinenowcast pipeline)
# 2. Generate evaluation data for that nowcast date
# 3. Score nowcasts - X time in days and save with metadata
# 4. Save quantiled nowcasts for visualisation

## Run measles case study----------------------------------------------------
model_run_targets <- list(
  ### Loop over each nowcast date ---------------------------------------------
  # tar_map(
  #   values = list(
  #     nowcast_dates_measles = config$measles_nowcast_dates
  #     )
  #   ),
  #   # 1. Generate nowcasts and aggregate (baselinenowcast pipeline)
  #   tar_target(
  #     name = summary_nowcast_measles,
  #     command = run_baselinenowcast_pipeline(
  #       long_df = measles_long_long,
  #       nowcast_date = nowcast_dates_measles,
  #       max_delay = 30,
  #       n_history_delay = 50,
  #       n_history_uncertainty = 10,
  #       n_draws = 100
  #     )
  #   )
  # )
  # 2. Save quantiled nowcasts for visualisation


  ## Run norovirus case study and score----------------------------------------
  ### Loop over each nowcast date ---------------------------------------------
  tar_map(
    values = list(
      nowcast_dates_noro = config$noro_nowcast_dates
    ),
    # 1. Generate nowcasts and aggregate (baselinenowcast pipeline)
    tar_target(
      name = summary_nowcast_noro,
      command = run_baselinenowcast_pipeline(
        long_df = noro_long,
        nowcast_date = nowcast_dates_noro,
        max_delay = 14,
        n_history_delay = 42,
        n_history_uncertainty = 10,
        n_draws = 100
      )
    )
  )
  # 2. Generate evaluation data for that nowcast date
  # 3. Score for nowcast data - X time in days and save with metadata
  # here use WIS for consistency with norovirus scores
  # 4. Save quantiled nowcasts for visualisation

  ## Gather nowcast scores for other models -------------------------------
  # 1. Combine norovirus model scores by nowcast date and model type
  # 2. Combine German Nowcast Hub model nowcasts and score them by model
  # and strata
)

#### Generate outputs for each model run joined to corresponding metadata

### Figures for real-world case study German Nowcast Hub
plot_targets <- list(
  ### Figures for simulated data case study with different model specifications

  ### Figures for norovirus and measles
  tar_target(
    name = plot_measles_data,
    command = get_plot_data_as_of(
      final_df = measles_long,
      as_of_dates = c(
        "2013-07-01", "2013-10-01",
        "2014-02-25"
      ),
      pathogen = "Measles"
    ),
    format = "rds"
  ),
  tar_target(
    name = plot_noro_data,
    command = get_plot_data_as_of(
      final_df = noro_long,
      as_of_dates = c(
        "2023-12-10", "2024-01-21",
        "2024-02-25"
      ),
      pathogen = "Norovirus"
    ),
    format = "rds"
  )

### Figure comparing performance to German Howcast Hub models

### Figure comparing baselinenowcast performance to other norovirus nowcasts

### Make figures comparing performance of baselinenowcats and norovirus models
) # end plot targets


list(
  data_targets,
  model_run_targets,
  plot_targets
)


