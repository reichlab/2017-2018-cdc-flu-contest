## code to check 2017/2018 submissions

# library(FluSight)
# 
# tmp <- read_entry("inst/prospective-predictions/kde/EW01-2018-ReichLab_kde.csv")
# visualize_forecast(tmp, ilimx=10, years=2016, pdfloc="inst/estimation/kde/EW01-2018-check.pdf")
# 
# tmp2 <- read_entry("inst/prospective-predictions/kde/EW43-2017-ReichLab_kde.csv")
# visualize_forecast(tmp2, ilimx=10, years=2016, pdfloc="inst/estimation/kde/EW43-2017-check.pdf")

library(tidyr)
library(ggplot2)
library(dplyr)

region_strings <- c("National", paste0("Region", 1:10))
seasons_to_check <- "2017-2018" #paste0(2010:2015, "-", 2011:2016)

pdf("inst/estimation/kde/check-kde-predictions.pdf", width=10)
for(reg in region_strings) {
    # reg = region_strings[1]
    for(season in seasons_to_check) {
        fname <- paste0("inst/estimation/kde/fits/kde-", reg, "-fit-prospective-", season, ".rds")
        tmp <- readRDS(fname)
        tmp1 <- as_data_frame(tmp) %>% 
            gather(
                key=metric, value=log_score, 
                -c(model, 
                    starts_with("prediction_week_ph"), 
                    starts_with("analysis_time"),
                    ends_with("log_prob"),
                    contains("competition"))
            ) %>%
            ## exclude pandemic season
            filter(analysis_time_season != "2009/2010")
        if(exists("tmp2")) {
            tmp2 <- rbind(tmp2, tmp1)
        } else {
            tmp2 <- tmp1
        }
    }
    p <- ggplot(tmp2, aes(x=analysis_time_season_week, y=log_score)) +
        geom_line(aes(color=factor(analysis_time_season))) +
        facet_grid(.~factor(metric)) +
        geom_smooth(se=FALSE, color="black") +
        ylim(-10, 0) + ggtitle(reg)
    print(p)
    rm(tmp2)
}
dev.off()