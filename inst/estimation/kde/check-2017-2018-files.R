## code to check 2017/2018 submissions

library(FluSight)

tmp <- read_entry("inst/prospective-predictions/kde/EW01-2018-ReichLab_kde.csv")
visualize_forecast(tmp, ilimx=10, years=2016, pdfloc="inst/estimation/kde/EW01-2018-check.pdf")

tmp2 <- read_entry("inst/prospective-predictions/kde/EW43-2017-ReichLab_kde.csv")
visualize_forecast(tmp2, ilimx=10, years=2016, pdfloc="inst/estimation/kde/EW43-2017-check.pdf")
