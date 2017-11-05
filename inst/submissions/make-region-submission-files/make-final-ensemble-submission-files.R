library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(MMWRweek)
library(cdcfluview)
library(xgboost)
library(xgbstack)
library(FluSight)

source("R/utils.R") # temporary -- not actually sure if needed

submissions_save_path <- "inst/submissions/submissions-KoT-dev"

### get submission files for all component models for current epidemic week
all_targets_internal <- c("onset", "peak_week", "peak_inc",
  "ph_1_inc", "ph_2_inc", "ph_3_inc", "ph_4_inc")

onset_component_model_names <- c("trunc_kde", "kcde", "sarima")
peak_week_component_model_names <- c("trunc_kde", "kcde", "sarima")
peak_inc_component_model_names <- c("trunc_kde", "kcde", "sarima")
ph_1_inc_component_model_names <- c("trunc_kde", "kcde", "sarima")
ph_2_inc_component_model_names <- c("trunc_kde", "kcde", "sarima")
ph_3_inc_component_model_names <- c("trunc_kde", "kcde", "sarima")
ph_4_inc_component_model_names <- c("trunc_kde", "kcde", "sarima")
all_component_model_names <- unique(unlist(lapply(all_targets_internal,
  function(target) {
    get(paste0(target, "_component_model_names"))
  })))

regions <- c("National", paste0("Region", 1:10))

year_week_combos <- data.frame(
  year = c(rep(2016, 10), rep(2017, 18)),
  week = c(43:52, 1:18)
)

for(year_week_ind in seq_len(nrow(year_week_combos))) {
  year <- year_week_ind$year[year_week_ind]
  week <- year_week_ind$week[year_week_ind]

  component_model_submissions <- lapply(all_component_model_names,
    function(name) {
      if(name %in% c("kcde", "sarima")) {
        res_file <- file.path(submissions_save_path,
          paste0(year,
            "-",
            week),
          paste0(name,
            "-predictions-",
            year,
            "-",
            week,
            ".csv"))
        return(read.csv(file = res_file))
      } else if(identical(name, "trunc_kde")) {
        res_file <- Sys.glob(file.path(submissions_save_path,
          paste0(year,
            "-",
            week),
          paste0("EW",
            week,
            "-KOTtrunckde-*.csv")))
        return(read.csv(file = res_file))
      } else {
        stop("Unknown component model name")
      }
    })



  ### Template for prediction results
  submission <- component_model_submissions[[1]]


  ### Update ensemble submission with weighted combination of predictions from
  ### component models.  Weights depend on region and prediction target
  stacking_model_fits_path <- "inst/estimation/stacking/stacking-fits-dev"

  for(location in as.character(unique(submission$Location))) {
    # if(identical(location, "US National")) { # used for draft submissions before midnight
    #   component_model_names <- c("kde", "kcde")
    # } else {
    #   component_model_names <- c("kde", "kcde", "sarima")
    # }
    for(target in as.character(unique(submission$Target))) {
      ## name of location as used in stacking model file names
      if(identical(location, "US National")) {
        location_for_lookup <- "National"
      } else {
        space_inds <- which(grepl(" ", strsplit(location, "")[[1]]))
        last_space_ind <- tail(space_inds, 1)
        location_for_lookup <- paste0("Region",
          substr(location, last_space_ind + 1, nchar(location)))
      }

      ## name of target as used in stacking model file names
      if(identical(target, "Season onset")) {
        target_for_lookup <- "onset"
      } else if(identical(target, "Season peak week")) {
        target_for_lookup <- "peak_week"
      } else if(identical(target, "Season peak percentage")) {
        target_for_lookup <- "peak_inc"
      } else {
        target_for_lookup <- paste0("ph_", substr(target, 1, 1), "_inc")
      }

      ## read in xgbstack model fit
      xgbstack_fit_path <- file.path(stacking_model_fits_path,
        paste0("xgbstack-fit-",
          location_for_lookup,
          "-",
          target_for_lookup,
          ".rds"))

      xgbstack_fit <- readRDS(file = xgbstack_fit_path)

      ## get weights for component models
      component_model_weights <-
        compute_model_weights(xgbstack_fit,
          newdata = data.frame(analysis_time_season_week = tail(data$season_week, 1)),
          log = FALSE)

      ## set predicted values in submissions data frame as weighted combination of
      ## predicted values in component model data frames
      inds <- (submission$Location == location) & (submission$Target == target)

      component_model_inds_for_target <- sapply(
        get(paste0(target_for_lookup, "_component_model_names")),
        function(model_name) {
          which(all_component_model_names == model_name)
        }
      )
      component_preds_for_target <- sapply(
        component_model_submissions[component_model_inds_for_target],
        function(x) {
          x$Value[inds]
        })

      submission$Value[inds] <-
        component_preds_for_target %*% t(component_model_weights)
    }
  }

  ## Get point predictions by sampling bins
  point_forecast_rows <- which(submission$Type == "Point")
  submission[point_forecast_rows, "Value"] <- sapply(point_forecast_rows,
    function(forecast_row_ind) {
      submission_subset <- submission[submission$Location == submission$Location[forecast_row_ind] &
          submission$Target == submission$Target[forecast_row_ind] &
          submission$Type == "Bin", ]
      median_ind <- median(sample(seq_len(nrow(submission_subset)),
          size = 10000,
          replace = TRUE,
          prob = submission_subset$Value
      ))
      return(
          as.numeric(as.character(submission_subset$Bin_start_incl[median_ind]))
      )
    })

  dev_submissions_save_path <- "inst/submissions/final-ensemble"
  save_file <- paste0(dev_submissions_save_path,
    "/",
    year,
    "-",
    week,
    "/EW",
    week,
    "-KoTdev-",
    Sys.Date(),
    ".csv")

  write.table(submission,
    file = save_file,
    quote = FALSE,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE)

  (FluSight::verify_entry_file(save_file))


  ### Plots for sanity

  make_predictions_plots(
    preds_save_file = save_file,
    plots_save_file = paste0(
      dev_submissions_save_path,
      "/",
      year,
      week,
      "plots-experimental.pdf"),
    data = data
  )
}
