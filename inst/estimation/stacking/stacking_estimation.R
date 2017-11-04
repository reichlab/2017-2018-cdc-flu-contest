library(plyr)
library(dplyr)
library(tidyr)
library(xgboost)
library(xgbstack)
library(cdcFlu20172018)

#setwd("research/flu/2017-2018-cdc-flu-contest/")

args <- commandArgs(trailingOnly = TRUE)

## For testing purposes:
#args <- c("Season onset", "1")
#source("R/stacking-utils.R")
#source("R/utils.R")

## One of 'Season onset', 'Season peak week', 'Season peak percentage', '1 wk ahead', '2 wk ahead', '3 wk ahead', '4 wk ahead'
prediction_target <- gsub("_", " ", args[1])
cat(prediction_target)

## integer number of threads to use
nthread <- as.integer(args[2])

component.score.df.trim <- read.csv("inst/estimation/stacking/trimmed_scores.csv")

target_loso_pred_res <- component.score.df.trim %>%
  filter(Target == prediction_target) %>%
  select(Model, Year, Epiweek, Season, Model.Week, Location, score_to_optimize) %>%
  spread(Model, score_to_optimize)

target_loso_pred_res$season_week <- year_week_to_season_week(
  year_week = target_loso_pred_res$Epiweek,
  year = target_loso_pred_res$Year)

component_model_names <- c("KCDE", "KDE", "sarima_seasonal_difference_FALSE", "sarima_seasonal_difference_TRUE")
explanatory_variables <- "season_week"

fit_formula <- as.formula(paste0(
  paste0(component_model_names, collapse = " + "),
  " ~ ",
  paste(explanatory_variables, collapse = " + ")))

cv_folds <- lapply(unique(target_loso_pred_res$Season),
  function(season_val) {
    which(target_loso_pred_res$Season == season_val)
  })

xgbstack_fit <- xgbstack(fit_formula,
  data = target_loso_pred_res,
  booster = "gbtree",
  subsample = 1,
  colsample_bytree = 1,
  colsample_bylevel = 1,
  max_depth = 1,
  min_child_weight = -10^10,
  eta = 0.01,
  gamma = 0,
  lambda = 0,
  alpha = 0,
  nrounds = 10,
  cv_params = list(
#      nrounds = 1,
#      nrounds = c(1, 10, 100, 500, 1000), #, 5000, 10000),
      nrounds = c(1, 10, 100, 500, 1000, 5000, 10000),
      max_depth = 1,
      alpha = c(0, 1, 5, 10, 20, 50),
      gamma = c(0, 1, 10, 100, 1000, 10000)
    ),
  cv_folds = cv_folds,
  cv_refit = "ttest",
  update = NULL,
  nthread = nthread,
  verbose = 0)

saveRDS(xgbstack_fit,
  file = file.path("inst/estimation/stacking/fits",
    paste0("xgbstack_fit_", prediction_target, ".rds")))
