## download and clean regional US flu data
## October 5 2016 - Nicholas Reich - created
## October 7 2016 - Evan Ray - calculate time using MMWRweek package, fix bug in
##   computation of season week
## October 7 2016 - Nicholas Reich - merged US and Regional data into one file.
## November 2 2017 - Nicholas Reich - forked for state-level data


library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(MMWRweek)
library(cdcfluview) ## devtools::install_github("hrbrmstr/cdcfluview")
library(forcats)

flu_data_raw <- get_flu_data(
    region = "state",
    sub_region = "all",
    data_source = "ilinet",
    years = 1997:2017
)

## ggplot(flu_data, aes(x=YEAR+WEEK/53, y=`%UNWEIGHTED ILI`)) + geom_line() + facet_wrap(~REGION)

flu_data <- transmute(flu_data_raw,
    region.type = `REGION TYPE`,
    region = REGION,
    year = YEAR,
    week = WEEK,
    time = as.POSIXct(MMWRweek2Date(YEAR, WEEK)),
    unweighted_ili = as.numeric(`%UNWEIGHTED ILI`),
    ili_count = ILITOTAL,
    patient_count = `TOTAL PATIENTS`,
    provider_count = `NUM. OF PROVIDERS`
)

## set rows with denominator zeroes to NAs
flu_data[which(flu_data$patient_count==0),"weighted_ili"] <- NA

## Add time_index column: the number of days since some origin date
## (1970-1-1 in this case).  The origin is arbitrary.
flu_data$time_index <- as.integer(date(flu_data$time) -  ymd("1970-01-01"))

## Season column: for example, weeks of 2010 up through and including week 30
## get season 2009/2010; weeks after week 30 get season 2010/2011
## Official CDC flu season for the purposes of prediction runs from week 40 of
## one year to week 20 of the next; the season start week we define here is the
## mid-point of the "off-season"
flu_data$season <- ifelse(
  flu_data$week <= 30,
  paste0(flu_data$year - 1, "/", flu_data$year),
  paste0(flu_data$year, "/", flu_data$year + 1)
)

## Season week column: week number within season
## weeks after week 30 get season_week = week - 30
## weeks before week 30 get season_week = week + (number of weeks in previous year) - 30
## This computation relies on the start_date function in package MMWRweek,
## which is not exported from that package's namespace!!!
flu_data$season_week <- ifelse(
  flu_data$week <= 30,
  flu_data$week + MMWRweek(MMWRweek:::start_date(flu_data$year) - 1)$MMWRweek - 30,
  flu_data$week - 30
)

state_flu <- as.data.frame(flu_data)

write.csv(state_flu, file = "data-raw/state_flu_data.csv")
save(state_flu, file = "data/state_flu_data.rdata")
