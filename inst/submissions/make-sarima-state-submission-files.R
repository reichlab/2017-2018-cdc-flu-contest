## This code is based on inst/data-processing/create-clean-flu-data.R

library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(MMWRweek)
library(cdcfluview)
library(KoTflu20162017)
library(forecast)
library(kcde)
library(copula)
library(FluSight)


source("R/utils.R") # temporary
source("R/kcde-utils.R") # temporary
source("inst/estimation/sarima/sarima-utils.R") # temporary

submissions_save_path <- "inst/submissions/submissions-KoT-stable"
submissions_save_path_dev <- "inst/submissions/submissions-KoT-dev"

data <- download_and_preprocess_flu_data()

### Do prediction for sarima
## Parameters used in simulating trajectories via kcde
simulate_trajectories_sarima_params <- list(
    fits_filepath = "inst/estimation/sarima/fits-for-competition",
    prediction_target_var = "weighted_ili"
)

sarima_res <- get_submission_via_trajectory_simulation(
    data = data,
    analysis_time_season = "2016/2017",
    first_analysis_time_season_week = 10, # == week 40 of year
    last_analysis_time_season_week = 41, # analysis for 33-week season, consistent with flu competition -- at week 41, we do prediction for a horizon of one week ahead
    prediction_target_var = "weighted_ili",
    incidence_bins = data.frame(
        lower = c(0, seq(from = 0.05, to = 12.95, by = 0.1)),
        upper = c(seq(from = 0.05, to = 12.95, by = 0.1), Inf)),
    incidence_bin_names = as.character(seq(from = 0, to = 13, by = 0.1)),
    n_trajectory_sims = 10000,
    simulate_trajectories_function = sample_predictive_trajectories_arima_wrapper,
    simulate_trajectories_params = simulate_trajectories_sarima_params,
    model_name = "sarima")

res_file <- file.path(submissions_save_path,
  paste0(tail(data$year, 1),
    "-",
    tail(data$week, 1)),
  paste0("sarima-predictions-",
    tail(data$year, 1),
    "-",
    tail(data$week, 1),
    ".csv"))
write.csv(sarima_res,
  file = res_file,
  row.names = FALSE)

(FluSight::verify_entry_file(res_file))

res_file <- file.path(submissions_save_path_dev,
  paste0(tail(data$year, 1),
    "-",
    tail(data$week, 1)),
  paste0("sarima-predictions-",
    tail(data$year, 1),
    "-",
    tail(data$week, 1),
    ".csv"))
write.csv(sarima_res,
  file = res_file,
  row.names = FALSE)

(FluSight::verify_entry_file(res_file))

### Plots for sanity

make_predictions_plots(
  preds_save_file = res_file,
  plots_save_file = paste0(
    submissions_save_path,
    "/",
    tail(data$year, 1),
    "-",
    tail(data$week, 1),
    "/",
    tail(data$week, 1),
    "-KOTsarima-plots.pdf"),
  data = data
)


### Do prediction for kcde
## Parameters used in simulating trajectories via kcde
simulate_trajectories_kcde_params <- list(
    n_kcde_sims = 10^5,
#    n_kcde_sims = 10,
    copula_save_path = "inst/estimation/kcde/copula-fits",
    estimation_results_path = "inst/estimation/kcde/fits",
    max_lag = "1",
    seasonality = TRUE,
    bw_parameterization = "diagonal",
    last_analysis_time_season_week = 41
)

kcde_res <- get_submission_via_trajectory_simulation(
    data = data,
    analysis_time_season = "2016/2017",
    first_analysis_time_season_week = 10, # == week 40 of year
    last_analysis_time_season_week = 41, # analysis for 33-week season, consistent with flu competition -- at week 41, we do prediction for a horizon of one week ahead
    prediction_target_var = "weighted_ili",
    incidence_bins = data.frame(
        lower = c(0, seq(from = 0.05, to = 12.95, by = 0.1)),
        upper = c(seq(from = 0.05, to = 12.95, by = 0.1), Inf)),
    incidence_bin_names = as.character(seq(from = 0, to = 13, by = 0.1)),
    n_trajectory_sims = 10000,
    simulate_trajectories_function = simulate_trajectories_kcde,
    simulate_trajectories_params = simulate_trajectories_kcde_params,
    model_name = "kcde")

res_file <- file.path(submissions_save_path,
  paste0(tail(data$year, 1),
    "-",
    tail(data$week, 1)),
  paste0("kcde-predictions-",
    tail(data$year, 1),
    "-",
    tail(data$week, 1),
    ".csv"))
write.csv(kcde_res,
  file = res_file,
  row.names = FALSE)

(FluSight::verify_entry_file(res_file))

res_file <- file.path(submissions_save_path_dev,
  paste0(tail(data$year, 1),
    "-",
    tail(data$week, 1)),
  paste0("kcde-predictions-",
    tail(data$year, 1),
    "-",
    tail(data$week, 1),
    ".csv"))
write.csv(kcde_res,
  file = res_file,
  row.names = FALSE)

(FluSight::verify_entry_file(res_file))


### Plots for sanity

make_predictions_plots(
  preds_save_file = res_file,
  plots_save_file = paste0(
    submissions_save_path,
    "/",
    tail(data$year, 1),
    "-",
    tail(data$week, 1),
    "/",
    tail(data$week, 1),
    "-KOTkcde-plots.pdf"),
  data = data
)
