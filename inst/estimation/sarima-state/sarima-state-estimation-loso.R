## state-level SARIMA fits
## 5 Oct 2016: started for regional level
## 3 Nov 2017 : adapted for state-level
## Nicholas Reich

library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(cdcFlu20172018)

## 5 Oct 2016:
## need to install forecast from GitHub to take advantage of recent bug fix.
## devtools::install_github("robjhyndman/forecast")
library(forecast)

## do all regional fits in parallel
library(doMC)
registerDoMC(3)

load("data/state_flu_data.rdata") ## loads `state_flu` object
state_names <- unique(state_flu$region)
state_names <- state_names[-which(state_names %in% c("Florida", "Louisiana"))]

## Florida and Louisiana: drop completely
## PR: starts in end of 2013
## VI: starts in end of 2015

## workaround to lie to the upcoming function which assumes weighted_ili is outcome
state_flu$weighted_ili <- state_flu$unweighted_ili

## remove zeros since sarima is taking logs
state_flu[which(state_flu$weighted_ili<0.00001),"weighted_ili"] <- NA

region_seasons <- expand.grid(
  region = state_names,
  first_test_season = "2017/2018", #paste0(2011:2016, "/", 2012:2017),
  stringsAsFactors = FALSE
)

## get fits with seasonal differencing before call to auto.arima
## where to store SARIMA fits
path <- paste0("inst/estimation/sarima-state/fits-seasonal-differencing/")

#foreach(i = seq_len(nrow(region_seasons))) %dopar%
for(i in 1:nrow(region_seasons)){
    message(paste(Sys.time(), "starting", region_seasons$region[i]))
  fit_region_sarima(data = state_flu,
    region = region_seasons$region[i],
    first_test_season = region_seasons$first_test_season[i],
    seasonal_difference = TRUE,
    transformation = "log",
    path = path)
}
