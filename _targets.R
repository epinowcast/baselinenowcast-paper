library(targets)
library(tarchetypes)
library(crew)
library(readr)
library(here)
library(purrr)
library(dplyr)
library(tibble)
library(lubridate)
library(ggplot2)
library(readr)
library(tidyr)
library(glue)
library(epinowcast)
library(baselinenowcast)
library(scoringutils)




controller <- crew_controller_local(
  workers = 8,
  seconds_idle = 600
)

# load functions
functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)
rm("functions")

# load target modules
targets <- list.files(here("targets"), full.names = TRUE)
targets <- grep("*\\.R", targets, value = TRUE)
purrr::walk(targets, source)

tar_option_set(
  packages = c(
    "tibble", "dplyr", "lubridate",
    "targets", "ggplot2",
    "baselinenowcast",
    "readr", "tidyr",
    "baselinenowcastpaper",
    "epinowcast",
    "scoringutils"
  ),
  workspace_on_error = TRUE,
  # Run with a pre-specified crew controller
  controller = controller,
  # Setup storage on workers vs on the main node.
  memory = "transient",
  storage = "worker",
  retrieval = "worker",
  format = "parquet", # default storage format
  error = "continue"
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

  ### Load and clean data----------------------------------------------
  load_data_targets
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
### Loop over each nowcast date ---------------------------------------------
# mapped_measles <- tar_map(
#   values = list(
#     nowcast_dates_measles = config$measles$nowcast_dates
#     ),
#   # 1. Generate nowcasts and aggregate (baselinenowcast pipeline)
#   tar_target(
#     name = summary_nowcast_measles,
#     command = run_baselinenowcast_pipeline(
#       long_df = measles_long_long,
#       nowcast_date = nowcast_dates_measles,
#       max_delay = config$measles$max_delay,
#       n_history_delay = config$measles$n_history_delay,
#       n_history_uncertainty = config$measles$n_history_uncertainty,
#       n_draws = config$n_draws
#     )
#   )
# ),
# 2. Save quantiled nowcasts for visualisation


## Run norovirus case study and score----------------------------------------
### Loop over each nowcast date ---------------------------------------------
mapped_noro <- tar_map(
  unlist = FALSE,
  values = list(
    nowcast_dates_noro = config$norovirus$nowcast_dates
  ),
  # 1. Generate nowcasts  (baselinenowcast pipeline)
  # 2. Generate evaluation data for that nowcast date
  # 3. Score for nowcast data - X time in days and save with metadata
  # here use WIS for consistency with norovirus scores
  # 4. Save quantiled nowcasts for visualisation
  gen_noro_nowcasts_targets
)
# Aggregate the summaried quantiles for visualising
combined_noro_nowcasts <- tar_combine(
  name = all_nowcasts_noro,
  mapped_noro$summary_nowcast_noro,
  command = dplyr::bind_rows(!!!.x)
)
#
#
#
#
# ## Gather nowcast scores for other models -------------------------------
# # 1. Combine norovirus model scores by nowcast date and model type
# # 2. Combine German Nowcast Hub model nowcasts and score them by model
# # and strata
#
#
# #### Generate outputs for each model run joined to corresponding metadata
#
### Figures for real-world case study German Nowcast Hub
plot_targets <- list(
  ### Figures for simulated data case study with different model specifications

  ### EDA figures for norovirus and measles
  EDA_plot_targets

  ### Figure comparing performance to German Howcast Hub models

  ### Figure comparing baselinenowcast performance to other norovirus nowcasts

  ### Make figures comparing performance of baselinenowcats and norovirus models
) # end plot targets


list(
  data_targets,
  mapped_noro,
  combined_noro_nowcasts,
  plot_targets
)
