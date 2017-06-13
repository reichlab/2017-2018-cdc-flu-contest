## loading all flu data from DELPHI API
## Nicholas Reich

source("https://raw.githubusercontent.com/cmu-delphi/delphi-epidata/master/src/client/delphi_epidata.R")

library(dplyr)
library(MMWRweek)

res <- Epidata$fluview(regions = list('nat', paste0("hhs", 1:10)), 
                       epiweeks = list(Epidata$range(199740, 201653)),
                       issues = list(Epidata$range(199740, 201653)))

df <- do.call(rbind, lapply(res$epidata, rbind)) %>%
    data_frame()

colnames(df) <- names(res$epidata[[1]])[!is.null(res$epidata[[1]])]
df$count <- as.numeric(as.character(df$count))
df$year <- as.numeric(substr(df$epiweek, 0, 4))
df$week <- as.numeric(substr(df$epiweek, 5, 6))
df$date <- MMWRweek2Date(MMWRyear = df$year, MMWRweek = df$week)

