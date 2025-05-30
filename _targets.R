library(targets)
library(tarchetypes)
library(readr)
library(here)
library(purrr)
library(dplyr)
library(tibble)
library(lubridate)
library(ggplot2)
library(ggpattern)
library(readr)
library(tidyr)
library(glue)
library(zoo)
library(epinowcast)
library(baselinenowcast)
library(scoringutils)
library(RColorBrewer)

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
    "targets", "ggplot2", "ggpattern",
    "baselinenowcast",
    "readr", "tidyr",
    "zoo",
    "epinowcast",
    "scoringutils",
    "RColorBrewer"
  ),
  workspace_on_error = TRUE,
  storage = "worker",
  retrieval = "worker",
  memory = "transient",
  garbage_collection = TRUE,
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

# Run real-world German Nowcast Hub case study validation --------------------
mapped_kit_nowcasts <- tar_map(
  unlist = FALSE,
  values = list(
    nowcast_dates_covid = unique(config$covid$nowcast_dates)
  ),
  kit_nowcast_targets
)

# Aggregate the summaried quantiles for visualising
combined_kit_nowcasts <- tar_combine(
  name = all_nowcasts_kit,
  mapped_kit_nowcasts$summary_nowcast_kit,
  command = dplyr::bind_rows(!!!.x)
)
combined_kit_scores <- tar_combine(
  name = all_scores_kit,
  mapped_kit_nowcasts$scores_quantile_kit,
  command = dplyr::bind_rows(!!!.x)
)
combined_kit_coverage <- tar_combine(
  name = all_coverage_kit,
  mapped_kit_nowcasts$coverage_kit,
  command = dplyr::bind_rows(!!!.x)
)
combined_kit_pt_nowcast <- tar_combine(
  name = all_pt_nowcasts_kit,
  mapped_kit_nowcasts$summary_pt_nowcast_kit,
  command = dplyr::bind_rows(!!!.x)
)

# ### Loop over each nowcast date and strata ----------------------------------
mapped_covid <- tar_map(
  unlist = FALSE,
  # Loop over each nowcast date, strata, data scenario, and model spec\
  values = list(
    # These define the units of model running
    nowcast_dates_covid = config$covid$nowcast_dates,
    age_group_to_nowcast = config$covid$age_groups,
    n_history_delay = config$covid$n_history_delay,
    n_history_uncertainty = config$covid$n_history_uncertainty,
    borrow = config$covid$borrow,
    partial_rep_tri = config$covid$partial_rep_tri
  ),
  # 1. Generate nowcasts and aggregate (baselinenowcast pipeline)
  # 2. Save quantiled nowcasts for visualisation
  gen_covid_nowcast_targets
)
# Aggregate the summaried quantiles for visualising
combined_covid_nowcasts <- tar_combine(
  name = all_nowcasts_covid,
  mapped_covid$summary_nowcast_covid,
  command = dplyr::bind_rows(!!!.x)
)
combined_covid_scores <- tar_combine(
  name = all_scores_covid,
  mapped_covid$scores_quantile_covid,
  command = dplyr::bind_rows(!!!.x)
)
combined_covid_coverage <- tar_combine(
  name = all_coverage_covid,
  mapped_covid$coverage_covid,
  command = dplyr::bind_rows(!!!.x)
)
combined_pt_nowcast <- tar_combine(
  name = all_pt_nowcasts,
  mapped_covid$pt_nowcast_7d,
  command = dplyr::bind_rows(!!!.x)
)
combined_mean_delay <- tar_combine(
  name = all_mean_delays,
  mapped_covid$mean_delay_df,
  command = dplyr::bind_rows(!!!.x)
)
combined_delay_df <- tar_combine(
  name = all_delay_dfs,
  mapped_covid$delay_df,
  command = dplyr::bind_rows(!!!.x)
)



# Make the combinations needed for the validation study---------------------
nowcast_hub_validation_targets

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
    n_history_training_volume = config$norovirus$n_history_training_volume,
    weekdays_noro = config$norovirus$weekdays_noro
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

# Figures for real-world case study German Nowcast Hub
plot_targets <- list(

  ### EDA figures for norovirus and covid
  EDA_plot_targets,

  ### Figures for German Nowcast Hub validation
  figures_hub_validation_targets

  ### Figures for comparing baselinenowcast model specificaitons

  ### Figure comparing baselinenowcast performance to other norovirus nowcasts
) # end plot targets


list(
  data_targets,
  # Covid targets: validation
  mapped_kit_nowcasts,
  combined_kit_nowcasts,
  combined_kit_scores,
  combined_kit_coverage,
  combined_kit_pt_nowcast,
  nowcast_hub_validation_targets,
  # # Covid targets: model permutations
  mapped_covid,
  combined_covid_nowcasts,
  combined_covid_scores,
  combined_covid_coverage,
  combined_pt_nowcast,
  combined_mean_delay,
  combined_delay_df,
  # Norovirus targets
  mapped_noro,
  combined_noro_nowcasts,
  combined_noro_scores,
  combined_noro_coverage,
  # Plotting
  plot_targets
)
