library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(xgboost)
library(xgbstack)
library(cdcFlu20172018)

setwd("research/flu/2017-2018-cdc-flu-contest/")

args <- commandArgs(trailingOnly = TRUE)

## For testing purposes:
args <- c("Season onset", "1")
#source("R/stacking-utils.R")
#source("R/utils.R")

## One of 'Season onset', 'Season peak week', 'Season peak percentage', '1 wk ahead', '2 wk ahead', '3 wk ahead', '4 wk ahead'
prediction_target <- args[1]

## integer number of threads to use
nthread <- as.integer(args[2])

component.score.df.trim <- read.csv("inst/estimation/stacking/trimmed_scores.csv")

target_loso_pred_res <- component.score.df.trim %>%
  filter(Target == prediction_target) %>%
  select(c("Model", "Year", "Epiweek", "Season", "Model Week", "Location", "score_to_optimize")) %>%
  spread(Model, score_to_optimize)

target_loso_pred_res$season_week <- 

component_model_names <- c("KCDE", "KDE", "sarima_seasonal_difference_FALSE", "sarima_seasonal_difference_TRUE")

fit_formula <- as.formula(paste0(
  paste0(component_model_names, "_log_score", collapse = " + "),
  " ~ ",
  paste(explanatory_variables, collapse = " + ")))

xgbstack_fit <- xgbstack(fit_formula,
                         data = target_loso_pred_res,
                         booster = booster,
                         subsample = subsample,
                         colsample_bytree = colsample_bytree,
                         colsample_bylevel = colsample_bylevel,
                         max_depth = max_depth,
                         min_child_weight = min_child_weight,
                         eta = eta,
                         gamma = gamma,
                         lambda = lambda,
                         alpha = alpha,
                         nrounds = nrounds,
                         cv_params = cv_params,
                         cv_folds = cv_folds,
                         cv_nfolds = cv_nfolds,
                         cv_refit = cv_refit,
                         update = update,
                         nthread = nthread,
                         verbose = verbose)
