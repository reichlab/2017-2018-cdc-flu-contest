library(lubridate)
library(plyr)
library(dplyr)
library(reshape)
library(kcde)
library(doMC)
library(forecast)
library(cdcFlu20172018)
library(np)


### Get command line arguments
args <- commandArgs(trailingOnly=TRUE)

## Some other arguments that we're not using for now, but might want to use later
## I have left in code below that depends on these values.
max_seasonal_lag <- 0
bw_parameterization <- "diagonal"


## national or regional (which region?)
## in order to region as an argument, we inserted a - instead of a space
## between "Region" and region number, so now undo that.
region <- gsub("-", " ", args[1])

## Prediction horizon -- integer number of steps ahead to predict
prediction_horizon <- as.integer(args[2])

## Maximum number of non-seasonal lagged observations to explore using for prediction
max_lag <- as.integer(args[3])

## Include terms capturing seasonality?
seasonality <- as.logical(args[4])

## use data up to but not including first test season to do estimation
first_test_season <- args[5]

## Path to save results in
save_path <- args[6]


#region <- "National"
#prediction_horizon <- 1L
#max_lag <- 3L
#seasonality <- TRUE
#first_test_season <- "2010/2011"
#save_path <- ""



## subset data to be only the region of interest
flu_data <- flu_data[flu_data$region == region,]

## Subset data to do estimation using only data up through first test season
first_ind_test_season <- min(which(flu_data$season == first_test_season))
flu_data <- flu_data[seq_len(first_ind_test_season - 1), , drop = FALSE]


prediction_target_var <- "weighted_ili"

lambda <- BoxCox.lambda(flu_data$weighted_ili, method = "loglik")
flu_data$box_cox_trans_weighted_ili <-
  BoxCox(flu_data[[prediction_target_var]], lambda)

flu_data$sin_sq_t <- sin(flu_data$time_index * pi / 365.2425)^2
flu_data$cos_sq_t <- cos(flu_data$time_index * pi / 365.2425)^2

temp <- compute_offset_obs_vecs(data = flu_data,
  filter_control = NULL,
  phi = NULL,
  vars_and_offsets = data.frame(
      var_name = c(rep(prediction_target_var, max_lag + 1 + 1), "sin_sq_t", "cos_sq_t", "season"),
      offset_type = c(rep("lag", max_lag + 1), "lead", "lag", "lag", "lag"),
      offset_value = c(0, seq_len(max_lag), prediction_horizon, 0, 0, 0),
      stringsAsFactors = FALSE
    ) %>%
    mutate(
      combined_name = paste0(var_name, "_", offset_type, offset_value)
    ),
  time_name = NULL,
  leading_rows_to_drop = 0,
  trailing_rows_to_drop = 0,
  additional_rows_to_drop = NULL,
  na.action = "na.omit")


for(season_val in unique(temp$season)) {
  if(max_lag > 0) {
    xvars <- c("weighted_ili_lag0", paste0("weighted_ili_lag", seq_len(max_lag)))
  } else {
    xvars <- c("weighted_ili_lag0")
  }
  if(seasonality) {
    xvars <- c(xvars, c("sin_sq_t_lag0", "cos_sq_t_lag0"))
  }
  est_time <- system.time({
    bw <- npcdensbw(
      xdat = temp[temp$season != season_val, xvars, drop = FALSE],
      ydat = temp[temp$season != season_val, paste0("weighted_ili_lead", prediction_horizon), drop = FALSE],
      nmulti = 2,
      remin = FALSE,
      bwtype = "adaptive_nn",
      bwmethod = "cv.ml")
  })

  eval_time <- system.time({
    fhat <- fitted(npcdens(
      exdat = temp[temp$season == season_val, xvars, drop = FALSE],
      eydat = temp[temp$season == season_val, paste0("weighted_ili_lead", prediction_horizon), drop = FALSE],
      bws = bw))
  })

  ### Save results
  saveRDS(bw,
    file = file.path(save_path,
      paste0("kcde_fit_np_loso_eval-",
        region,
        "-",
        gsub("/", "-", season_val),
	"-",
	max_lag,
        "-",
        "seasonality_", seasonality,
        "-",
        prediction_horizon,
        ".rds")
    )
  )

  saveRDS(fhat,
    file = file.path(save_path,
      paste0("kcde_fhat_np_loso_eval-",
        region,
        "-",
        gsub("/", "-", season_val),
        "-",
	max_lag,
	"-",
        "seasonality_", seasonality,
        "-",
        prediction_horizon,
        ".rds")
    )
  )
}
