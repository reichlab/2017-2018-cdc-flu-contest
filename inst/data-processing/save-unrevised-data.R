library(plyr) # for rbind.fill
library(dplyr)
library(tidyr)
library(MMWRweek)
source("https://raw.githubusercontent.com/cmu-delphi/delphi-epidata/master/src/client/delphi_epidata.R")

# Fetch data
all_regions <- c("nat", paste0("hhs", 1:10))
all_issues <- expand.grid(
    year = 2010:2017,
    week = sprintf("%02d", c(1:20, 40:53))
  ) %>%
  apply(1, function(x) paste(x, collapse = "")) %>%
  as.integer() %>%
  sort()
all_issues <- all_issues[all_issues >= 201040 & all_issues <= 201720]

all_obs <- lapply(all_regions,
  function(region_val) {
    lapply(all_issues,
      function(issue_val) {
        obs_one_issue <- Epidata$fluview(
          regions = list(region_val),
          epiweeks = list(Epidata$range(199740, 201740)),
          issue = list(issue_val))

        lapply(obs_one_issue$epidata,
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
  separate(epiweek, c("year", "week"), sep=4, remove=FALSE) %>%
  mutate(
    year = as.integer(year),
    week = as.integer(week))

saveRDS(all_obs, file = "data/flu_data_with_backfill.rds")
