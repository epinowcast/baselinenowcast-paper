# Baseline nowcasting methods for handling delays in epidemiological data
This repository contains the code to generate the results of analysing the nowcasts produced by the [`baselinenowcast`](https://github.com/epinowcast/baselinenowcast) R package applied to case studies in German COVID-19 data and norovirus data from UKHSA.
Please see that package's GitHub repository for a mathematical description of the method and details on how to install the package and use it to generate nowcasts.

This README is organized into the following sections:
- [Project structure](#project-structure) describing the contents of this repository
- [Data sources](#data-sources) providing links and a description to external data sources used

## Project structure

| Folder or file | Purpose |
|---|---|
|[`_targets.R`](_targets.R) | The [targets](https://books.ropensci.org/targets/) pipeline used to generate the figures and results in this work. |
|[`targets`](targets) | The folder containing the files of targets lists grouped by their outputs. |
|[`input`](input) | Contains the configuration file to specify the evaluation runs. |
|[`R`](R) | Functions needed to generate targets. |
|[`docs`](docs) | Files needed to generate the supplement plus a rendered version of the latest Supplement. |
|[`report.Rmd`](report.Rmd) | Report containing tables with all the summary metrics needed to write the main text results. |

## Data sources

### Raw data used to produce nowcasts generated in this code-base
The pre-processed reporting triangle used to generate the German COVID-19 nowcasts is available at [`COVID-19_hospitalizations_preprocessed.csv`](https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/11c745322c055cfbd4f0c8f72241642a50aea399/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv).
The synthetic data that can be used to generate norovirus nowcasts for England from UKHSA (United Kingdom Health Security Agency) is available at [`cases_with_noise.csv`](https://raw.githubusercontent.com/jonathonmellor/norovirus-nowcast/refs/heads/main/outputs/data/cases_with_noise.csv).

We note that for the norovirus nowcasts, all outputs were produced by co-authors at UKHSA.
Nowcasts and the data used to evaluate them were run within UKHSA with outputs shared with us.
Results can be approximately reproduced using entirely the synthetic data provided in the UKHSA norovirus GitHub link above, which were used to ensure the method produced reasonable nowcasts.

### Nowcasted quantiles produced outside this code-base

The original and revised implementations of the KIT simple nowcast quantiles on the latest German COVID-19 hospitalization data are available at [`data-processed_retrospective`](https:://github.com/kaitejohnson/hospitalization-nowcast-hub/data-processed_retrospective).
The nowcasted quantiles for norovirus for the GAM, epinowcast, naive baseline, and baselinenowcast specifications are available at [`data/original`](https:://github.com/jonathonmellor/norovirus-nowcast-baselinenowcast/outputs/data/original), with the quantiles fit to the synethic data available at [`data/synthetic`](https:://github.com/jonathonmellor/norovirus-nowcast-baselinenowcast/outputs/data/synthetic)
