library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)
library(kcde)
library(copula)
library(MMWRweek)
library(cdcFlu20172018)
#library(FluSight)

### Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
analysis_time_year <- args[1]
analysis_time_week <- args[2]

#analysis_time_year <- "2010"
#analysis_time_week <- "40"

method <- "kcde"

submissions_save_path <- "inst/prospective-predictions/kcde/prospective_predictions_kcde"

analysis_time_epiweek <- paste0(
  analysis_time_year,
  sprintf("%02d", as.integer(analysis_time_week)))

## set up RNG
get_rng_substream(
  method = method,
  year = analysis_time_year,
  week = analysis_time_week)

all_regions <- c("nat", paste0("hhs", 1:10))
data <- rbind.fill(lapply(all_regions,
  function(region_str) {
    get_partially_revised_ilinet(
      region_str = region_str,
      epiweek_str = analysis_time_epiweek) %>%
      mutate(region = region_str)
  })) %>%
  mutate(
    region.type = ifelse(region == "nat", "National", "HHS Regions"),
    region = ifelse(region == "nat", "National", gsub("hhs", "Region ", region)),
    weighted_ili = wili,
    time = as.POSIXct(MMWRweek2Date(year, week))
  )

## Add time_index column: the number of days since some origin date (1970-1-1 in this case).
## The origin is arbitrary.
data$time_index <- as.integer(data$time -  as.POSIXct(ymd(paste("1970", "01", "01", sep = "-"))))

## Season column: for example, weeks of 2010 up through and including week 30 get season 2009/2010;
## weeks after week 30 get season 2010/2011
## I am not sure where I got that the season as defined as starting on MMWR week 30 from...
data$season <- ifelse(
  data$week <= 30,
  paste0(data$year - 1, "/", data$year),
  paste0(data$year, "/", data$year + 1)
)

## Season week column: week number within season
## weeks after week 30 get season_week = week - 30
## weeks before week 30 get season_week = week + (number of weeks in previous year) - 30
## This computation relies on the start_date function in package MMWRweek,
## which is not exported from that package's namespace!!!
data$season_week <- ifelse(
  data$week <= 30,
  data$week + MMWRweek(MMWRweek:::start_date(data$year) - 1)$MMWRweek - 30,
  data$week - 30
)

data <- as.data.frame(data)


analysis_time_season <- data$season[
  data$region == "National" &
  data$year == as.integer(analysis_time_year) &
  data$week == as.integer(analysis_time_week)
]

## Parameters used in simulating trajectories via kcde
simulate_trajectories_kcde_params <- list(
  n_kcde_sims = 10^5,
  copula_save_path = "inst/estimation/kcde/copula-fits",
  estimation_results_path = "inst/estimation/kcde/fits",
  max_lag = "1",
  seasonality = TRUE,
  bw_parameterization = "diagonal",
  last_analysis_time_season_week = 41,
  first_test_season = analysis_time_season
)

res <- get_submission_via_trajectory_simulation(
  data = data,
  analysis_time_season = analysis_time_season,
  first_analysis_time_season_week = 10, # == week 40 of year
  last_analysis_time_season_week = 41, # analysis for 33-week season, consistent with flu competition -- at week 41, we do prediction for a horizon of one week ahead
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
    "EW", analysis_time_week,
    "-", analysis_time_year,
    "-ReichLab_", method,
    ".csv"))
write.csv(res,
  file = res_file,
  row.names = FALSE)

#file_valid <- FluSight::verify_entry_file(res_file)
#
#if(!file_valid) {
#  stop("invalid predictions file!")
#}
