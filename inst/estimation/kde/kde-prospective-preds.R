## make a set of prospective prediction files for KDE/GAM model
## these files used for the collaborative ensemble
## Nicholas Reich
## July 2017

library(cdcFlu20172018)
library(doMC)

n_sims <- 10000
prospective_seasons <- paste0(2010:2016, "/", 2011:2017)
season_weeks <- 10:43

## for 2017-2018 season, 
## first predictions due on 11/6/2017 (EW45 == SW15)
## using data posted on 11/3/2016 that includes up to EW43 == SW13
## last predictions due on 5/14/2017 (EW20 == SW 42)
## last predictions use data through EW18 == SW40
## first_analysis_time_season_week could be set to 15, but padding at front end
registerDoMC(4)

for(season in prospective_seasons){
    foreach(season_week = season_weeks) %dopar% {
    make_one_kde_prediction_file(save_path = "inst/prospective-predictions/kde/",
                                 fits_path = "inst/estimation/kde/fits/",
                                 season = season,
                                 season_week = season_week,
                                 n_sim = n_sims)
  }
}

