## download and clean regional US flu data
## create data frame of "final" observed historical indcidence,
## for the purpose of filling in missing partially-revised data
## in the process of making test-phase prospective forecasts for the
## 2010/2011 through 2016/2017 seasons
## July 19 2017 - Evan Ray - created from 2017-2018-cdc-flu-contest/inst/data-processing/create-clean-flu-data.R


library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(MMWRweek)
library(cdcfluview)
library(forcats)

regionflu <- get_flu_data("hhs",
  sub_region = 1:10,
  data_source = "ilinet",
  years = 1997:2017)
usflu <- get_flu_data("national",
  sub_region = NA,
  data_source = "ilinet",
  years = 1997:2017)

## make AGE cols in usflu integer data type
cols <- matches('^AGE', vars=colnames(usflu))
usflu[,cols] <- sapply(usflu[,cols], as.integer)
cols <- matches('^AGE', vars=colnames(regionflu))
regionflu[,cols] <- sapply(regionflu[,cols], as.integer)

flu_data <- bind_rows(regionflu, usflu)

flu_data <- transmute(flu_data,
  release_date = NA,
  region = ifelse(REGION == "X", "nat", gsub("Region ", "hhs", REGION)),
  issue = 201727,
  epiweek = as.integer(paste0(YEAR, sprintf("%02d", WEEK))),
  year = YEAR,
  season_week = WEEK,
  lag = NA,
  num_ili = as.integer(ILITOTAL),
  num_patients = as.integer(`TOTAL PATIENTS`),
  num_providers = as.integer(`NUM. OF PROVIDERS`),
  num_age_0 = NA,
  num_age_1 = NA,
  num_age_2 = NA,
  num_age_3 = NA,
  num_age_4 = NA,
  num_age_5 = NA,
  wili = as.numeric(`% WEIGHTED ILI`),
  ili = as.numeric(`%UNWEIGHTED ILI`)
)

flu_data <- as.data.frame(flu_data)

saveRDS(flu_data, file = "data/flu_data_final_for_prospective_predictions.rds")
