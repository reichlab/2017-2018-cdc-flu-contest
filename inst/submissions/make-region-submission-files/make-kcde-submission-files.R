## This code is based on inst/data-processing/create-clean-flu-data.R

library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(MMWRweek)
library(cdcfluview)
library(cdcFlu20172018)
library(forecast)
library(kcde)
library(copula)
library(FluSight)

submissions_save_path <- "inst/submissions/kcde-region"

data <- download_and_preprocess_flu_data()

analysis_time_season <- "2017/2018"

### Do prediction for kcde
## Parameters used in simulating trajectories via kcde
simulate_trajectories_kcde_params <- list(
  n_kcde_sims = 10^5,
  copula_save_path = "inst/estimation/kcde/copula-fits",
  estimation_results_path = "inst/estimation/kcde/fits",
  max_lag = "1",
  seasonality = TRUE,
  bw_parameterization = "diagonal",
  last_analysis_time_season_week = 41,
  first_test_season = "2016/2017" # no fits for 2017/2018 yet...
)

weeks_in_first_season_year <-
  get_num_MMWR_weeks_in_first_season_year(analysis_time_season)

res <- get_submission_via_trajectory_simulation(
  data = data,
  analysis_time_season = analysis_time_season,
  first_analysis_time_season_week = 10, # == week 40 of year
  last_analysis_time_season_week = weeks_in_first_season_year - 11, # analysis for 33-week season
  prediction_target_var = "weighted_ili",
  incidence_bins = data.frame(
    lower = c(0, seq(from = 0.05, to = 12.95, by = 0.1)),
    upper = c(seq(from = 0.05, to = 12.95, by = 0.1), Inf)),
  incidence_bin_names = as.character(seq(from = 0, to = 13, by = 0.1)),
  n_trajectory_sims = 10000,
  simulate_trajectories_function = simulate_trajectories_kcde,
  simulate_trajectories_params = simulate_trajectories_kcde_params)

res_file <- file.path(submissions_save_path,
  paste0(
    "EW", tail(data$week, 1),
    "-", tail(data$year, 1),
    "-ReichLab_kcde",
    ".csv"))
write.csv(res,
  file = res_file,
  row.names = FALSE)

(FluSight::verify_entry_file(res_file))

### Plots for sanity

make_predictions_plots(
  preds_save_file = res_file,
  plots_save_file = paste0(
    submissions_save_path,
    "/plots/",
    tail(data$year, 1),
    "-",
    tail(data$week, 1),
    "-KOTkcde-plots.pdf"),
  data = data
)
