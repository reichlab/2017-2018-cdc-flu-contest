library(plyr) # for rbind.fill
library(dplyr)
library(MMWRweek)
source("https://raw.githubusercontent.com/cmu-delphi/delphi-epidata/master/src/client/delphi_epidata.R")

# Fetch data
all_obs <- lapply(c("nat", paste0("hhs", 1:10)),
    function(region_val) {
        lapply(0:51,
            function(lag_val) {
                obs_one_lag <- Epidata$fluview(
                    regions = list(region_val),
                    epiweeks = list(Epidata$range(199740, 201740)),
                    lag = list(lag_val))
                
                lapply(obs_one_lag$epidata,
                    function(x) {
                        x[sapply(x, function(comp) is.null(comp))] <- NA
                        return(as.data.frame(x))
                    }) %>%
                    rbind.fill()
            }) %>%
            rbind.fill()
    }) %>%
    rbind.fill()

all_obs$release_date <- as.Date(all_obs$release_date)
all_obs <- all_obs %>% 
    separate(epiweek, c("year", "season_week"), sep=4, remove=FALSE) %>%
    mutate(
        year = as.integer(year),
        season_week = as.integer(season_week))

saveRDS(all_obs, file = "data/flu_data_with_backfill.rds")
