library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(MMWRweek)
library(cdcfluview)
library(xgboost)
library(xgbstack)
library(FluSight)
library(cdcFlu20172018)
library(purrr)
library(ggplot2)


### Update ensemble submission with weighted combination of predictions from
### component models.  Weights depend on region and prediction target
stacking_model_fits_path <- "inst/estimation/stacking/fits"

submissions_save_path <- "inst/submissions"

### get submission files for all component models for current epidemic week
component_model_names <- c("kde", "kcde", "sarima_seasonal_difference_TRUE", "sarima_seasonal_difference_FALSE")
regions <- c("National", paste0("Region", 1:10))

component_model_submissions <- lapply(component_model_names,
  function(name) {
    res_file <- file.path(submissions_save_path,
      paste0(name, "-region"),
      paste0("EW",
        43,
        "-",
        2017,
        "-ReichLab_",
        name,
        ".csv"))
    return(read.csv(file = res_file))
  })



### Template for prediction results
submission <- component_model_submissions[[1]]


pdf("inst/estimation/stacking/plots.pdf")

for(target in as.character(unique(submission$Target))) {
  ## read in xgbstack model fit
  xgbstack_fit_path <- file.path(stacking_model_fits_path,
    paste0("xgbstack_fit_",
      target,
      ".rds"))
  
  xgbstack_fit <- readRDS(file = xgbstack_fit_path)
  
  ## get weights for component models
  component_model_weights <- map_df(c(13:41),
    function(season_week) {
      temp <- compute_model_weights(xgbstack_fit,
        newdata = data.frame(season_week = season_week),
        log = FALSE)
      inds <- grepl("combined", dimnames(temp)[[2]])
      return(
        matrix(temp[inds], nrow = 1) %>%
          as.data.frame() %>%
          `colnames<-`(dimnames(temp)[[2]][inds])
      )
    }
  )

  component_model_weights$season_week <- 13:41
  component_model_weights_long <- component_model_weights %>%
    gather("model", "weight", 1:4)

  p <- ggplot() +
    geom_line(mapping = aes(x = season_week, y = weight, color = model), data = component_model_weights_long) +
    ggtitle(target) +
    theme_bw()

  print(p)

  prediction_target <- gsub("_", " ", target)
  
  component.score.df.trim <- read.csv("inst/estimation/stacking/trimmed_scores.csv")
  
  target_loso_pred_res <- component.score.df.trim %>%
    filter(Target == prediction_target) %>%
    select(Model, Year, Epiweek, Season, Model.Week, Location, score_to_optimize) %>%
    spread(Model, score_to_optimize)
  
  target_loso_pred_res$season_week <- year_week_to_season_week(
    year_week = target_loso_pred_res$Epiweek,
    year = target_loso_pred_res$Year)

  target_loso_pred_res_long <- target_loso_pred_res %>%
    gather("model", "log_score", KCDE, KDE, sarima_seasonal_difference_FALSE, sarima_seasonal_difference_TRUE)
  
  summarized_log_scores <- target_loso_pred_res_long %>%
    group_by(model, season_week) %>%
    summarize(
      mean_log_score = mean(log_score),
      min_log_score = min(log_score),
      max_log_score = max(log_score)) %>%
    mutate(Model = toupper(model))
  summarized_log_scores$smoothed_log_score <- NA
  summarized_log_scores$smoothed_max_log_score <- NA
  summarized_log_scores$smoothed_min_log_score <- NA
  summarized_log_scores$prediction_target <- target

  for(model_val in unique(summarized_log_scores$model)) {
    for(prediction_target_val in unique(summarized_log_scores$prediction_target)) {
      if(identical(prediction_target_val, "Peak Incidence")) {
        loess_span <- 0.5
      } else {
        loess_span <- 0.75
      }
      inds <- (summarized_log_scores$model == model_val &
        summarized_log_scores$prediction_target == prediction_target_val)
      if(sum(inds) == 1 || model_val == "KDE") {
        summarized_log_scores$smoothed_log_score[inds] <-
          summarized_log_scores$mean_log_score[inds]
        summarized_log_scores$smoothed_max_log_score[inds] <-
          summarized_log_scores$max_log_score[inds]
        summarized_log_scores$smoothed_min_log_score[inds] <-
          summarized_log_scores$min_log_score[inds]
      } else {
        summarized_log_scores$smoothed_log_score[inds] <-
          suppressWarnings(predict(
            suppressWarnings(
              loess(log_score ~ season_week, data = target_loso_pred_res_long %>%
                filter(model == model_val),
                span = loess_span
                )
              ),
            newdata = summarized_log_scores[inds, ]
        ))
        summarized_log_scores$smoothed_max_log_score[inds] <-
          suppressWarnings(predict(
            suppressWarnings(
              loess(max_log_score ~ season_week, data = summarized_log_scores[inds, ],
                span = loess_span))
            ))
        summarized_log_scores$smoothed_min_log_score[inds] <-
          suppressWarnings(predict(
            suppressWarnings(
              loess(min_log_score ~ season_week, data = summarized_log_scores[inds, ],
                span = loess_span))
          ))
      }
    }
  }
#summarized_log_scores$prediction_target[summarized_log_scores$prediction_target == "onset"] <- "Onset Timing"
#summarized_log_scores$prediction_target[summarized_log_scores$prediction_target == "peak_inc"] <- "Peak Incidence"
#summarized_log_scores$prediction_target[summarized_log_scores$prediction_target == "peak_week"] <- "Peak Timing"
#summarized_log_scores$prediction_target <- factor(summarized_log_scores$prediction_target,
#  levels = c("Onset Timing", "Peak Timing", "Peak Incidence"))
#
#log_scores_with_confidence_long <-
#  log_scores_with_confidence_long  %>%
#  mutate(Model = toupper(model))
#
#regions_to_plot <- c("National", "Region 1", "Region 7")

StatChull <- ggproto("StatChull", Stat,
  compute_group = function(data, scales) {
    data[chull(data$x, data$y), , drop = FALSE]
  },

  required_aes = c("x", "y")
)

stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


p <- ggplot() +
  stat_chull(
    aes(x = season_week, y = log_score,
      colour = model,
      fill = model,
      linetype = model),
    alpha = 0.2,
    data = target_loso_pred_res_long) +
  geom_line(
    aes(x = season_week,
      y = smoothed_log_score,
      colour = model,
      linetype = model),
    size = 1.5,
    data = summarized_log_scores) +
  geom_point(aes(x = season_week, y = log_score, colour = model, shape = model),
    alpha = 0.25,
    position = position_jitter(),
    data = target_loso_pred_res_long) +
  xlab("Season Week at Analysis Time") +
  ylab("Log Score") +
  ggtitle(target) +
  theme_bw()

print(p)
}

dev.off()


