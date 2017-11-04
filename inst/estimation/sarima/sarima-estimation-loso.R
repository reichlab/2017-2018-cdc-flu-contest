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
registerDoMC(4)

region_seasons <- expand.grid(
  region = c("National", paste0("Region ", 1:10)),
  first_test_season = paste0(2010:2016, "/", 2011:2017),
  stringsAsFactors = FALSE
)

## get fits with seasonal differencing before call to auto.arima
## where to store SARIMA fits
path <- paste0("inst/estimation/sarima/fits-seasonal-differencing/")

foreach(i = seq_len(nrow(region_seasons))) %dopar%
  fit_region_sarima(data = flu_data,
    region = region_seasons$region[i],
    first_test_season = region_seasons$first_test_season[i],
    seasonal_difference = TRUE,
    transformation = "log",
    path = path)

## get fits without seasonal differencing before call to auto.arima
## where to store SARIMA fits
path <- paste0("inst/estimation/sarima/fits-no-seasonal-differencing/")

foreach(i = seq_len(nrow(region_seasons))) %dopar%
  fit_region_sarima(data = flu_data,
    region = region_seasons$region[i],
    first_test_season = region_seasons$first_test_season[i],
    seasonal_difference = FALSE,
    transformation = "log",
    path = path)
