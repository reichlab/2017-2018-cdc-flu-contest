## make a set of prospective prediction files for KDE/GAM model
## Nicholas Reich
## July 2017

library(cdcFlu20172018)
library(doMC)

n_sims <- 10000
prospective_seasons <- paste0(2010:2016, "/", 2011:2017)
season_weeks <- 13:42

## for 2017-2018 season, 

## NUMBERS BELOW HERE NEED TO BE UPDATED FOR 2017-2018 SEASON
## first predictions due on 11/7/2016 (EW45 == SW15)
## using data posted on 11/4/2016 that includes up to EW43 == SW13
## last predictions due on 5/15/2017 (EW20 == SW 42)
## last predictions use data through EW40 == SW18
## first_analysis_time_season_week could be set to 15, but padding at front end
registerDoMC(20)

for(season in prospective_seasons){
    foreach(season_week = season_weeks) %dopar% {
    make_one_kde_prediction_file(save_path = "inst/prospective-predictions/kde/",
                                 fits_path = "inst/estimation/kde/fits/",
                                 season = season,
                                 season_week = season_week,
                                 n_sim = n_sims)
  }
}

