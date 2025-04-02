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

# Methods

# We'll start by gathering all of the datasets and model specifications that we
# need for each of the component analyses

## Real-world data from German Nowcast Hub
data_targets <- list(

  ### Create reporting triangles as of daily nowcast dates

  ### Create evaluation data
  # This will be as of each of the nowcast dates + the evaluation time frame (e.g.
  # 80 days)

  ### Model specification for German nowcast data
  # Specify the maximum delay, number of reference times to use for delay
  # estimation, number of reporting triangles to use for dispersion estimate, and
  # whether or not delay estimates/dispersion parameters are being borrowed

  ## Simulated data, modified from German Nowcast Hub

  ## Create simulated reporting triangles as of nowcast dates from final reports
  # This will be for two age strata only, and with a long and short
  # delay, creating 4 distinct datasets

  ### Create evaluation data for simulated data
  # This will be from simulated data as of nowcast date + eval time frame (e.g. 80
  # days)

  ### Model specification(s) for simulated data
  # Make all combinations of varying:
  # - max delay
  # - borrowing for delay estimation
  # - borrowing for dispersion estimate
  # - number of observations for delay estimation
  # - number of reporting triangles for dispersion estimate

  ## Real-world case data: Measles and norovirus

  ### Load and clean measles data
  tar_target(
    name = measles_line_list,
    command = read.delim("https://raw.githubusercontent.com/kassteele/Nowcasting/refs/heads/master/data/measles_NL_2013_2014.dat", sep = "") |> # nolint
      mutate(
        reference_date = ymd(onset.date),
        report_date = ymd(report.date)
      ) |>
      select(reference_date, report_date)
  ),
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


  ### Create reporting triangles as of final nowcast date
  tar_target(
    name = measles_nowcast_dates,
    command = tibble(nowcast_date = c(
      "2013-07-01", "2013-10-01",
      "2014-02-25"
    )) |>
      group_by(nowcast_date) |>
      tar_group(),
    iteration = "group"
  ),
  # This function will be used to format the data as a reporting triangle
  # from a long dataframe by report and reference date and as of a particular
  # nowcast date
  tar_target(
    name = measles_rt,
    command = do.call(
      get_rep_tri_from_long_df,
      c(list(
        long_df = measles_long,
        nowcast_date = max(measles_long$reference_date),
        max_delay = 50
      ))
    )
  ),

  ### Load in norovirus data

  ### Create reporting triangles as of final nowcast date

  ### Create summarised counts as of all nowcast dates + eval time frames
  tar_target(
    name = measles_nowcasts_df,
    command = do.call(
      get_eval_data_from_long_df,
      c(list(
        long_df = measles_long,
        # replace with nowcast date + eval time frame
        as_of_date = ymd(measles_nowcast_dates$nowcast_date) + days(50)
      ))
    ),
    pattern = map(measles_nowcast_dates)
  ),
  tar_target(
    name = plot_measles_data,
    command = get_plot_data_as_of(measles_nowcasts_df),
    format = "rds"
  ),
  ### Model specification for each data set
  # Based off of the specific dataset, make a single choice about specification
  tar_target(
    name = measles_spec,
    command = list(
      max_delay = 50,
      n_history_delay = 75,
      n_history_dispersion = 50,
      borrow_delay = FALSE,
      borrow_dispersion = FALSE
    ),
    format = "rds"
  ),
  # Results

  ## Run `baselinenowcast` model

  ### Use tar_map or tar_group_by to run the model on all datasets
  tar_map(
    values = list(
      nowcast_date = c(
        "2013-07-01", "2013-10-01",
        "2014-02-25"
      )
    ),
    # Get the reporting triangle as of the nowcast date
    tar_target(
      name = rep_tri_df,
      command = get_rep_tri_from_long_df(
        long_df = measles_long,
        nowcast_date = nowcast_date,
        max_delay = 50
      )
    )
  ) # end tar map
) # end model run targets



#### Generate outputs for each model run joined to corresponding metadata
# - scores (from samples)
# - scores (from quantiles; e.g. calibration but also WIS scores)
# - quantiled nowcasts for visual comparison

### Figures for real-world case study German Nowcast Hub

### Figures for simulated data case study with different model specifications

### Figures for norovirus and measles

### Consolidate evaluation data from mean/median ensemble in GNH & our run

### Make figures comparing performance of baselinenowcats and ensembles

list(
  data_targets
  # model_run_targets
)
