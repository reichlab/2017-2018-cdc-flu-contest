library("pipeR")
library("dplyr")
library("tidyr")

setwd("research/flu/2017-2018-cdc-flu-contest/")

## set column to use for calculating weights
SCORE_COL <- quo(`Multi bin score`)

## Read in & tweak scores.csv
component.score.df = read.csv("inst/prospective-predictions/scores.csv", check.names=FALSE, stringsAsFactors=FALSE) %>>%
  tibble::as_tibble() %>>%
  dplyr::filter(Model %in% c("ReichLab-KCDE", "ReichLab-KDE", "ReichLab-SARIMA1", "ReichLab-SARIMA2")) %>>% ## Reich Lab Models Only
  dplyr::mutate(score_to_optimize = dplyr::if_else(is.nan(!!SCORE_COL), -10, !!SCORE_COL)) %>>%
  dplyr::mutate(score_to_optimize = dplyr::if_else(score_to_optimize < -10 , -10, score_to_optimize)) %>>%
  dplyr::mutate(Metric = "some log score") %>>%
  {.}

## Create data.frame of boundary weeks of scores to keep for each target/season
source("inst/estimation/stacking/create-scoring-period.R")
all.target.bounds = create_scoring_period()

## Remove scores that fall outside of evaluation period for a given target/season
component.score.df.trim <- component.score.df %>%
  dplyr::left_join(all.target.bounds, by = c("Season", "Target", "Location")) %>%
  dplyr::filter(`Model Week` >= start_week_seq, `Model Week` <= end_week_seq)

component.score.df.trim$Model <- gsub("ReichLab-", "", component.score.df.trim$Model)
component.score.df.trim$Model[component.score.df.trim$Model == "SARIMA1"] <- "sarima_seasonal_difference_FALSE"
component.score.df.trim$Model[component.score.df.trim$Model == "SARIMA2"] <- "sarima_seasonal_difference_TRUE"

write.csv(component.score.df.trim, "inst/estimation/stacking/trimmed_scores.csv")
