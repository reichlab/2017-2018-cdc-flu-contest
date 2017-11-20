## This code is based on inst/data-processing/create-clean-flu-data.R
## Updated for SARIMA-state submissions: 11/3/2017

library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(MMWRweek)
library(cdcfluview) ## devtools::install_github("hrbrmstr/cdcfluview", ref="8bd99b7")
library(forecast)
library(FluSight)
library(cdcFlu20172018)
library(gridExtra)

submissions_save_path <- "inst/submissions/sarima-state"

data <- download_and_preprocess_state_flu_data()

state_names <- unique(data$region)
state_names <- state_names[-which(state_names %in% c("Florida", "Louisiana"))]

## workaround to lie to the upcoming function which assumes weighted_ili is outcome
data$weighted_ili <- data$unweighted_ili

## make zeroes and NAs very small positive numbers since sarima is taking logs
data[which(data$weighted_ili<0.00001),"weighted_ili"] <- 0.0005
data[which(is.na(data$weighted_ili)),"weighted_ili"] <- 0.0005

### Do prediction for sarima
## Parameters used in simulating trajectories via kcde
simulate_trajectories_sarima_params <- list(
    fits_filepath = "inst/estimation/sarima-state/fits-seasonal-differencing",
    prediction_target_var = "weighted_ili",
    seasonal_difference = TRUE,
    transformation = "log",
    first_test_season = "2017/2018"
)

sarima_res <- get_submission_via_trajectory_simulation(
    data = data,
    analysis_time_season = "2017/2018",
    first_analysis_time_season_week = 10, # == week 40 of year
    last_analysis_time_season_week = 41, # analysis for 33-week season, consistent with flu competition -- at week 41, we do prediction for a horizon of one week ahead
    prediction_target_var = "weighted_ili",
    incidence_bins = data.frame(
        lower = c(0, seq(from = 0.05, to = 12.95, by = 0.1)),
        upper = c(seq(from = 0.05, to = 12.95, by = 0.1), Inf)),
    incidence_bin_names = as.character(seq(from = 0, to = 13, by = 0.1)),
    n_trajectory_sims = 10000,
    simulate_trajectories_function = sample_predictive_trajectories_arima_wrapper,
    simulate_trajectories_params = simulate_trajectories_sarima_params,
    all_regions = state_names)


res_file <- file.path(submissions_save_path,
  paste0(
      "EW",
      tail(data$week, 1),
      "-KoT-StateILI-",
      ymd(Sys.Date()),
      ".csv"
  )
)

plot_file <- file.path(submissions_save_path,
    paste0(
        "plots/EW",
        tail(data$week, 1),
        "-KoT-StateILI-",
        ymd(Sys.Date()),
        "-plots.pdf"
    )
)

write.csv(sarima_res,
  file = res_file,
  row.names = FALSE)

(FluSight::verify_entry_file(res_file, challenge = "state_ili"))

### Plots for sanity

sarima_res <- read_entry(res_file)

pdf(plot_file, width = 12)
for(reg in unique(sarima_res$location)){
    p_peakpct <- plot_peakper(sarima_res, region = reg) + ylim(0,1)
    p_peakwk <- plot_peakweek(sarima_res, region = reg) + ylim(0,1)
    p_1wk <- my_plot_weekahead(sarima_res, region = reg, wk = 1, ilimax=13, years = 2017:2018) + ggtitle(paste(reg, ": 1 wk ahead")) + ylim(0,1)
    p_2wk <- my_plot_weekahead(sarima_res, region = reg, wk = 2, ilimax=13, years = 2017:2018) + ylim(0,1)
    p_3wk <- my_plot_weekahead(sarima_res, region = reg, wk = 3, ilimax=13, years = 2017:2018) + ylim(0,1)
    p_4wk <- my_plot_weekahead(sarima_res, region = reg, wk = 4, ilimax=13, years = 2017:2018) + ylim(0,1)
    grid.arrange(p_1wk, p_2wk, p_3wk, p_4wk, p_peakpct, p_peakwk, ncol=4)
}
dev.off()
