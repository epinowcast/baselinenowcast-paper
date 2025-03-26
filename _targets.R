# Targets pipeline for `baselinenowcast` analysis paper.
# This script defines the analysis pipeline used to evaluate the performance
# of the `baselinenowcast` R package, which is intended to be used as a
# baseline method for nowcasting right-truncated epidemiological data.

# The performance of the baseline nowcast method will be evaluated as follows:
# 1. Applied to real-world data from the German Nowcast Hub from 1 Dec 2021 to
# 1 April 2022, nationally, by age group, and by state.
# 2. Applied to 4 simulated data scenarios in which we take two age strata, one
# sparse and one rich from the German Nowcast data, and apply a long and short
# delay to the data. For each data scenario, we will assess 24 different
# combinations of model specifications
# 3. Applied to real-world data from 1. 2013-2014 Measles outbreak in the
# Netherlands 2. 2023-2024 Norovirus outbreak in the UK and 3. Comparison of
# performance of the mean/median ensemble in the German Nowcast Hub

library(targets)
library(crew)
library(RcppTOML)
library(readr)

controller <- crew_controller_local(
  workers = 8,
  seconds_idle = 600
)

# Set targets options:
tar_option_set(
  packages = c(
    "tibble", "dplyr", "lubridate",
    "baselinenowcast",
    "readr"
  ),
  workspace_on_error = TRUE,
  # Run with a pre-specified crew controller
  controller = controller,
  # Setup storage on workers vs on the main node.
  memory = "transient",
  storage = "worker",
  retrieval = "worker",
  format = "rds", # default storage format
  error = NULL # tells errored targets to return NULL rather than
  # have whole pipeline fail
)

# Swap in with real config, use example for now
config <- parseTOML(file.path(
  "input", "config", "example_config.toml"
))



# Section 1: Real-world case study German Nowcast Hub -------------------------

## Load data snapshot----------------------------------------------------------
## Create data as of nowcast dates using epinowcast pre-processing-------------
## Create eval data as of nowcast date + eval time frame ---------------------
## Use tar_map to run a series of steps for each nowcasting problem ----------
# 1. apply baselinenowcast pipeline to generate summarized long dataframe
# 2. join corresponding evaluation data
# 3. score from samples, generate quantiled data, score from quantiles
# 4. save to disk for all strata (age groups & states)
# Combine scores, scores from quantiles, and quantiled data -----------------
# Make figures for national data----------------------------------------------
# Supplementary figures applied to other strata--------------------------------

# Section 2: Simulated data scenarios under various model specifications-------

## Use final total counts from Section 1 to create final triangles-------------
## Create data as of nowcast dates from final triangles -----------------------
## Create eval data as of nowcast date + eval time frame ----------------------
## Use tar_map to run steps for each nowcasting problem ----------------------
# Loop over the 4 datasets, specifying each combo of the model specs
# 1. apply baselinenowcast pipeline to generate summarized long dataframe
# 2. join corresponding evaluation data
# 3. score from samples, generate quantiled data, score from quantiles
# 4. save to disk for all strata (age groups & states)
## Combine scores, scores from quantiles, and quantiled data across everything--
## Make figures summarizing scores across data scenarios and model specs--------

# Section 3: Real-world case studies ------------------------------------------

## 3a: Netherlands Measles nowcasts -------------------------------------------
### Load measles data ---------------------------------------------------------
### Create final reporting triangle from line list data -----------------------
### Create data as of nowcast dates--------------------------------------------
### Create eval data as of nowcast date + eval time frame----------------------
### Use tar_map to run steps for each nowcasting problem ----------------------
# 1. apply baselinenowcast pipeline to generate summarized long dataframe
# 2. join corresponding evaluation data
# 3. score from samples, generate quantiled data, score from quantiles
# 4. save to disk for all days
### Combine scores, scores from quantiles, and quantiled data------------------
### Make figures for measles
## 3b: UK Norovirus nowcasts --------------------------------------------------
### Repeat procedure used for measles
## 3c: Comparison to GNH ensembles --------------------------------------------
### Load in scores for mean/median ensemble (or quantiles if no scores)-------
### Join scores to quantiled scores for Section 1
### Summarise by national, states, and age groups and make figures--------------
