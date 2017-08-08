library(plyr)
library(dplyr)
library(reshape)
library(kcde)  ## devtools::install_github("reichlab/kcde")
library(cdcFlu20172018)
library(np)


### Get command line arguments
args <- commandArgs(trailingOnly=TRUE)

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



## subset data to be only the region of interest
flu_data <- flu_data[flu_data$region == region,]

## Subset data to do estimation using only data up through first test season
first_ind_test_season <- min(which(flu_data$season == first_test_season))
flu_data <- flu_data[seq_len(first_ind_test_season - 1), , drop = FALSE]


prediction_target_var <- "weighted_ili"

flu_data$sin_sq_t <- sin(flu_data$time_index * pi / 365.2425)^2
flu_data$cos_sq_t <- cos(flu_data$time_index * pi / 365.2425)^2

temp <- compute_offset_obs_vecs(data = flu_data,
  filter_control = NULL,
  phi = NULL,
  vars_and_offsets = data.frame(
      var_name = c(rep(prediction_target_var, max_lag + 1 + 1), "sin_sq_t", "cos_sq_t"),
      offset_type = c(rep("lag", max_lag + 1), "lead", "lag", "lag"),
      offset_value = c(0, seq_len(max_lag), prediction_horizon, 0, 0),
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

if(max_lag > 0) {
  xvars <- c("weighted_ili_lag0", paste0("weighted_ili_lag", seq_len(max_lag)))
} else {
  xvars <- c("weighted_ili_lag0")
}
if(seasonality) {
  xvars <- c(xvars, c("sin_sq_t_lag0", "cos_sq_t_lag0"))
}
yvars <- paste0("weighted_ili_lead", prediction_horizon)

est_time <- system.time({
  bw <- npcdensbw(
    xdat = temp[, xvars, drop = FALSE],
    ydat = temp[, yvars, drop = FALSE],
    nmulti = 2,
    remin = FALSE,
    bwtype = "generalized_nn",
    bwmethod = "cv.ml")
})



### Save results
case_descriptor <- paste0(
  gsub(" ", "_", region),
  "-",
  "first_test_", gsub("/", "_", first_test_season),
  "-",
  "max_lag_", max_lag,
  "-",
  "horizon_", prediction_horizon,
  ".rds")

saveRDS(bw,
  file = file.path(save_path,
    paste0("kcde_bw_fit-", case_descriptor, ".rds")
  )
)
