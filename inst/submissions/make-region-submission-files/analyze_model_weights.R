library(dplyr)
library(tidyr)
library(xgbstack)
library(xgboost)
library(ggplot2)

### Update ensemble submission with weighted combination of predictions from
### component models.  Weights depend on region and prediction target
stacking_model_fits_path <- "inst/estimation/stacking/stacking-fits"

region_strings <- c("US National", paste("HHS Region", 1:10))
target_strings <- c("Season onset", "Season peak week", "Season peak percentage",
                    paste(1:4, "wk ahead"))
model_strings <- c("kde", "kcde", "sarima") ## ORDER MATTERS - is it right?
season_weeks <- 10:41

model_weights <- expand.grid(region=region_strings,
                             target=target_strings,
                             season_week=season_weeks)
model_weights$kde <- model_weights$kcde <- model_weights$sarima <- NA 

for(location in region_strings) {
    for(target in target_strings) {
        ## name of location as used in stacking model file names
        if(identical(location, "US National")) {
            location_for_lookup <- "National"
        } else {
            space_inds <- which(grepl(" ", strsplit(location, "")[[1]]))
            last_space_ind <- tail(space_inds, 1)
            location_for_lookup <- paste0("Region",
                                          substr(location, last_space_ind + 1, nchar(location)))
        }
        
        ## name of target as used in stacking model file names
        if(identical(target, "Season onset")) {
            target_for_lookup <- "onset"
        } else if(identical(target, "Season peak week")) {
            target_for_lookup <- "peak_week"
        } else if(identical(target, "Season peak percentage")) {
            target_for_lookup <- "peak_inc"
        } else {
            target_for_lookup <- paste0("ph_", substr(target, 1, 1), "_inc")
        }
        
        ## read in xgbstack model fit
        xgbstack_fit_path <- file.path(stacking_model_fits_path,
                                       paste0("xgbstack-fit-",
                                              location_for_lookup,
                                              "-",
                                              target_for_lookup,
                                              ".rds"))
        
        xgbstack_fit <- readRDS(file = xgbstack_fit_path)
        
        ## get weights for component models
        component_model_weights <-
            compute_model_weights(xgbstack_fit,
                                  newdata = data.frame(analysis_time_season_week = 10:41),
                                  log = FALSE)
        idx <- which(model_weights$region==location & 
                         model_weights$target==target)
        model_weights[idx, model_strings] <- component_model_weights 
    }
}

mwts <- gather(model_weights, key = model, value = weight, 
               kde, kcde, sarima)

ggplot(mwts, aes(x=season_week, y=weight, color=model)) +
    geom_line() +
    facet_grid(region ~ target) +
    theme(strip.text.y = element_text(size=5))

