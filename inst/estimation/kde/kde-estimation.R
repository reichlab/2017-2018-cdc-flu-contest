## KDE estimation
## 15 June 2017 - copied over from awes repository
## 

library(cdcFlu20172018)
library(rstream)
library(MMWRweek)
library(dplyr)
library(doMC)

## setup
data(flu_data)
region_strings <- c("National", paste("Region", 1:10))
fit_path <- "inst/estimation/kde/fits/"
method <- "kde"
analysis_time_seasons <- paste0(2010:2016, "/", 2011:2017)

## setup for random seeds
rngstream <- get_initial_rng_substream()

registerDoMC(11)
foreach(reg=region_strings) %dopar% {

  ## fit models on training seasons, using only prospective data, not LOSO
  ## this function call saves a set of .rds files that contain the list defining a "KDE fit" 
  ## one fit for each (prospective season, region) pair
  
  # reg = region_strings[1]
  fit_region_kdes(flu_data, 
                  region=reg,
                  first_fit_year = 2010,
                  first_fit_week = 31,
                  last_fit_year = 2016,
                  path = fit_path)
  
  ## make leave-one-season-out predictions for training seasons
  # ## for debugging
  # debug(get_log_scores_via_direct_simulation)
  # region = region_strings[1]
  # analysis_time_season = "2000/2001"
  for(analysis_time_season in analysis_time_seasons) {
    ## set random seed from substream
    get_rng_substream(
      rngstream = rngstream,
      method = method,
      region = reg,
      season = analysis_time_season,
      set_rng = TRUE)
    
    ## get and save log scores
    get_log_scores_via_direct_simulation(
      analysis_time_season = analysis_time_season,
      first_analysis_time_season_week = 10, # == week 40 of year
      last_analysis_time_season_week = 41, # analysis for 33-week season, consistent with flu competition -- at week 41, we do prediction for a horizon of one week ahead
      region = reg,
      prediction_target_var = "weighted_ili",
      incidence_bins = data.frame(
        lower = c(0, seq(from = 0.05, to = 12.95, by = 0.1)),
        upper = c(seq(from = 0.05, to = 12.95, by = 0.1), Inf)),
      incidence_bin_names = as.character(seq(from = 0, to = 13, by = 0.1)),
      n_sims = 100000,
      model_name = "kde",
      fits_path = fit_path,
      prediction_save_path = "inst/estimation/kde/checking/"
    )
  }
}

source("inst/estimation/kde/check-kde-predictions.R")
