library(targets)
library(tarchetypes)
library(ggplot2)
library(purrr, quietly = TRUE)
library(arrow)
library(here)
functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)
rm("functions")

tar_option_set(
  packages = c(
    "tibble", "dplyr", "lubridate",
    "baselinenowcast",
    "readr", "epinowcast", "tidyr"
  ),
  workspace_on_error = TRUE,
  # Setup storage on workers vs on the main node.
  memory = "transient",
  format = "rds", # default storage format
  error = NULL # tells errored targets to return NULL rather than
  # have whole pipeline fail
)
