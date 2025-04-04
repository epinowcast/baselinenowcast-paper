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

# Methods---------------------------------------------------------------------

# We'll start by gathering all of the datasets and model specifications that we
# need for each of the component analyses

## Real-world data from German Nowcast Hub------------------------------------
  
  ### Create long tidy dataframe as of final date from German Nowcast Hub------
  # This will be used to generate reporting triangles as of nowcast dates 
  # and to generate evaluation data (triangle as of nowcast date + eval time)
  
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
  
  ### Create long tidy dataframe up until final date---------------------------
  # Will be used to create reporting triangles as of nowcast dates + 
  # evaluation data 
  
  ### Load and clean norovirus data--------------------------------------------
  
  ### Create long tidy datframe up until final date----------------------------
  # Will be used to create reporting triangles as of nowcast dates + 
  # evaluation data 
  
  ### Model specification for each data set------------------------------------
  # Based off of the specific dataset, make a single choice about specification
 
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
  # 1. Generate nowcasts and aggregate (baselinenowcast pipeline)
  # 2. Save quantiled nowcasts for visualisation 

  ## Run norovirus case study and score----------------------------------------
  ### Loop over each nowcast date ---------------------------------------------
  # 1. Generate nowcasts and aggregate (baselinenowcast pipeline)
  # 2. Generate evaluation data for that nowcast date 
  # 3. Score for nowcast data - X time in days and save with metadata
    # here use WIS for consistency with norovirus scores
  # 4. Save quantiled nowcasts for visualisation 
  
  ## Gather nowcast scores for other norovirus models ------------------------
  # 1. Combine norovirus model scores by nowcast date and model type


#### Generate outputs for each model run joined to corresponding metadata

### Figures for real-world case study German Nowcast Hub

### Figures for simulated data case study with different model specifications

### Figures for and measles

### Make figures comparing performance of baselinenowcats and norovirus models

