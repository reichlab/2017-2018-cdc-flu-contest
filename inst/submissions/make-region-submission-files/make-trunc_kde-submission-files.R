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

source("inst/estimation/kde/kde-utils.R")
source("inst/estimation/trunc-kde/trunc-kde-utils.R")
source("inst/estimation/trunc-kde/make-one-trunc-kde-prediction-file.R")
source("inst/estimation/kde/kde-prediction-utils.R")

kde_fits_save_path <- "inst/estimation/kde/current-fits"
n_sims <- 10000

data <- download_and_preprocess_flu_data()

data$rounded_weighted_ili <- round(data$weighted_ili, digits = 1)


submissions_save_path <- file.path("inst/submissions/submissions-KoT-dev",
  paste0(tail(data$year, 1),
    "-",
    tail(data$week, 1)))

make_one_trunc_kde_prediction_file(
  fits_path = kde_fits_save_path,
  results_path = submissions_save_path,
  season = tail(data$season, 1),
  season_week = tail(data$season_week, 1),
  n_sim = n_sims,
  dat = data,
  first_analysis_time_season_week = 10
)

res_file <- paste0(
  submissions_save_path,
  "/EW",
  tail(data$week, 1),
  "-KOTtrunckde-",
  Sys.Date(),
  ".csv")

(FluSight::verify_entry_file(res_file))



### Plots for sanity

make_predictions_plots(
  preds_save_file = res_file,
  plots_save_file = paste0(
    submissions_save_path,
    "/",
    "KOTtrunckde-plots.pdf"),
  data = data
)
