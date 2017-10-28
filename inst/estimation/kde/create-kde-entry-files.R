## make a set of prospective prediction files for KDE/GAM model for 2017/2018 season
## Nicholas Reich
## October 2017

library(cdcFlu20172018)
library(doMC)

data(flu_data)

n_sims <- 10000

## for 2017-2018 season, 
## first predictions due on 11/6/2017 (EW45 == SW15)
## using data posted on 11/3/2016 that includes up to EW43 == SW13
## last predictions due on 5/14/2017 (EW20 == SW 42)
## last predictions use data through EW18 == SW40
## first_analysis_time_season_week could be set to 15, but padding at front end to 10

season_weeks <- 10:43
region_strings <- c("National", paste("Region", 1:10))
fit_path <- "inst/estimation/kde/fits/"

registerDoMC(4)

## fit 2017/2018 models

foreach(reg=region_strings) %dopar% {
    
    ## fit models on training seasons, using only prospective data, not LOSO
    ## this function call saves a set of .rds files that contain the list defining a "KDE fit" 
    ## one fit for each (prospective season, region) pair
    
    # reg = region_strings[1]
    fit_region_kdes(flu_data, 
        region=reg,
        first_fit_year = 2017,
        first_fit_week = 20, 
        last_fit_year = 2017,
        path = fit_path)
}

## make entry files
foreach(season_week = season_weeks) %dopar% {
    make_one_kde_prediction_file(save_path = "inst/prospective-predictions/kde/",
        fits_path = fit_path,
        season = "2017/2018",
        season_week = season_week,
        n_sim = n_sims)
}

