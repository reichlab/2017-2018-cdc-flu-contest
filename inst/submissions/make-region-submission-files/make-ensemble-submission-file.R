library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(MMWRweek)
library(cdcfluview)
library(xgboost)
library(xgbstack)
library(FluSight)
library(cdcFlu20172018)


data <- download_and_preprocess_flu_data()

submissions_save_path <- "inst/submissions"

### get submission files for all component models for current epidemic week
component_model_names <- c("kde", "kcde", "sarima_seasonal_difference_TRUE", "sarima_seasonal_difference_FALSE")
regions <- c("National", paste0("Region", 1:10))

component_model_submissions <- lapply(component_model_names,
  function(name) {
    res_file <- file.path(submissions_save_path,
      paste0(name, "-region"),
      paste0("EW",
        tail(data$week, 1),
        "-",
        tail(data$year, 1),
        "-ReichLab_",
        name,
        ".csv"))
    return(read.csv(file = res_file))
  })



### Template for prediction results
submission <- component_model_submissions[[1]]
submission$Value <- NA

### Update ensemble submission with weighted combination of predictions from
### component models.  Weights depend on region and prediction target
stacking_model_fits_path <- "inst/estimation/stacking/fits"

for(target in as.character(unique(submission$Target))) {
  ## read in xgbstack model fit
  xgbstack_fit_path <- file.path(stacking_model_fits_path,
    paste0("xgbstack_fit_",
      target,
      ".rds"))

  xgbstack_fit <- readRDS(file = xgbstack_fit_path)

  ## get weights for component models
  component_model_weights <-
    compute_model_weights(xgbstack_fit,
      newdata = data.frame(season_week = tail(data$season_week, 1)),
      log = FALSE)
  inds <- grepl("combined", dimnames(component_model_weights)[[2]])
  weight_names <- dimnames(component_model_weights)[[2]][inds]
  weight_names <- substr(weight_names, 1, nchar(weight_names) - 16)
  component_model_weights <- component_model_weights[inds]
  component_model_weights <- component_model_weights[
    sapply(tolower(component_model_names), function(mname) { which(tolower(weight_names) == mname) })]
  component_model_weights <- matrix(component_model_weights, ncol = 1)

  ## set predicted values in submissions data frame as weighted combination of
  ## predicted values in component model data frames
  for(location in as.character(unique(submission$Location))) {
    inds <- (submission$Location == location) & (submission$Target == target)

    component_preds_for_target <- sapply(component_model_submissions,
      function(x) {
        x$Value[inds]
      })

    ## If target is a week of the year, convert to season before averaging
    ## This ensures reasonable predictions for week 51 of 2016 and week 4 of 2017
    if(target %in% c("Season onset", "Season peak week")) {
      pointpred_ind <- which(submission$Type[inds] == "Point")
      component_preds_for_target[pointpred_ind, ] <-
        year_week_to_season_week(
          year_week = component_preds_for_target[pointpred_ind, ],
          year = tail(data$year, 1))
    }

#    if(identical(location, "US National")) { # used for draft submissions before midnight
#      component_preds_for_target <- component_preds_for_target[, 1:2]
#    }

    submission$Value[inds] <-
      component_preds_for_target %*% component_model_weights

    ## If target is a week, convert back to week of year from week of season
    if(target %in% c("Season onset", "Season peak week")) {
      submission$Value[which(inds)[pointpred_ind]] <-
        season_week_to_year_week(
          season_week = submission$Value[which(inds)[pointpred_ind]],
            weeks_in_first_season_year = 52)
    }
  }
}


res_file <- file.path(submissions_save_path,
  "KoT-region",
  paste0(
    "EW", tail(data$week, 1),
    "-", tail(data$year, 1),
    "-ReichLab_KoT",
    ".csv"))
write.csv(submission,
  file = res_file,
  row.names = FALSE)

(FluSight::verify_entry_file(res_file))

### Plots for sanity

make_predictions_plots(
  preds_save_file = res_file,
  plots_save_file = paste0(
    submissions_save_path,
    "/KoT-region-plots/",
    tail(data$year, 1),
    "-",
    tail(data$week, 1),
    "-KoT-plots.pdf"),
  data = data
)
