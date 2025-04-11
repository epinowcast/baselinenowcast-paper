library(targets)
library(tarchetypes)
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
    "epinowcast"
  ),
  workspace_on_error = TRUE,
  # Run with a pre-specified crew controller
  # Setup storage on workers vs on the main node.
  memory = "transient",
  storage = "worker",
  retrieval = "worker",
  format = "parquet", # default storage format
  error = "null"
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

  ### Load and clean data German nowcast data and measles data
  load_data_targets
) # end data_targets


# Results-------------------------------------------------------------------

## Run real-world German Nowcast Hub case study validation --------------------
### Loop over each nowcast date and strata ----------------------------------
mapped_covid <- tar_map(
  unlist = FALSE,
  # Loop over each nowcast date, strata, data scenario, and model spec\
  tar_map(
    # These define the units of model running
    nowcast_dates_covid = config$covid$nowcast_dates,
    age_group_to_nowcast = config$covid$age_groups,
    n_history_delay = config$covid$n_history_delay,
    n_history_uncertainty = config$covid$n_history_uncertainty,
    borrow_delay = config$covid$borrow_delay,
    borrow_uncertainty = config$covid$borrow_uncertainty
  ),
  # 1. Generate nowcasts and aggregate (baselinenowcast pipeline)
  # 2. Save quantiled nowcasts for visualisation
  gen_covid_nowcast_targets
)


## Run multiple model spec on real data
### Loop over each nowcast date, strata, data scenario, and model spec-------
# 1. Generate nowcasts and aggregate (baselinenowcast pipeline)
# 2. Generate evaluation data for that nowcast date
# 3. Score nowcasts - X time in days and save with metadata
# 4. Save quantiled nowcasts for visualisation

# Run norovirus case study and score------------------------------------------
## Loop over each nowcast date and model spec --------------------------------
mapped_noro <- tar_map(
  unlist = FALSE,
  # Loop over all combinations of nowcast dates for each model spec,
  # where model spec is defined by `filter_ref_dates` (FALSE if base, TRUE if
  # filtering by day of week), and by the number of historical observations
  # to use for the delay estimation. (this is either orig_n_history/7 or orig
  # n_history)
  values = list(
    nowcast_dates_noro = config$norovirus$nowcast_dates,
    filter_ref_dates = config$norovirus$filter_ref_dates,
    n_history_delay = config$norovirus$n_history_delays
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
combined_noro_scores <- tar_combine(
  name = all_scores_noro,
  mapped_noro$scores_quantile_noro,
  command = dplyr::bind_rows(!!!.x)
)
combined_noro_coverage <- tar_combine(
  name = all_coverage_noro,
  mapped_noro$coverage_noro,
  command = dplyr::bind_rows(!!!.x)
)
## Gather nowcast scores for other models -------------------------------
# 1. Combine norovirus model scores by nowcast date and model type
# 2. Combine German Nowcast Hub model nowcasts and score them by model
# and strata


#### Generate outputs for each model run joined to corresponding metadata

## Figures for real-world case study German Nowcast Hub
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
  combined_noro_scores,
  combined_noro_coverage
  # plot_targets
)
