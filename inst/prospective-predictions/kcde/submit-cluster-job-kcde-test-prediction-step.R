library(dplyr)

cores_req <- "1"
mem_req <- "5000"
time_req <- "4:00"
queue_req <- "short"
year_week_combos <- expand.grid(
  year = as.character(2010:2017),
  week = sprintf("%02d", c(1:19, 40:52)),
  stringsAsFactors = FALSE
) %>%
  mutate(epiweek = as.integer(paste0(year, week))) %>%
  filter(epiweek >= 201040 &
    epiweek <= 201719) %>%
  rbind(
    data.frame(year = "2014",
      week = "53",
      epiweek = 201453,
      stringsAsFactors = FALSE)
    ) %>%
  arrange(epiweek)

for(ind in seq_len(nrow(year_week_combos))) {
  year <- year_week_combos$year[ind]
  week <- year_week_combos$week[ind]

  output_path <- "/home/er71a/2017-2018-cdc-flu-contest/inst/prospective-predictions/kcde/cluster-output"
  lsfoutfilename <- "kcde-test-predict.out"

  case_descriptor <- paste0(year, "-", week)
  filename <- paste0(output_path, "/submit-kcde-prospective-prediction-step-", case_descriptor, ".sh")

  requestCmds <- "#!/bin/bash\n"
  requestCmds <- paste0(requestCmds, "#BSUB -n ", cores_req, " # how many cores we want for our job\n",
    "#BSUB -R span[hosts=1] # ask for all the cores on a single machine\n",
    "#BSUB -R rusage[mem=", mem_req, "] # ask for memory\n",
    "#BSUB -o ", lsfoutfilename, " # log LSF output to a file\n",
    "#BSUB -W ", time_req, " # run time\n",
    "#BSUB -q ", queue_req, " # which queue we want to run in\n")

  cat(requestCmds, file = filename)
  cat("module load R/3.4.0\n", file = filename, append = TRUE)
  cat("module load gsl/1.16\n", file = filename, append = TRUE)
  cat(paste0("R CMD BATCH --vanilla \'--args ",
    year, " ",
    week,
    "\'  /home/er71a/2017-2018-cdc-flu-contest/inst/prospective-predictions/kcde/kcde-prediction-test.R ",
    output_path, "/output-kcde-test-prediction-step-", case_descriptor, ".Rout"),
    file = filename, append = TRUE)

  bsubCmd <- paste0("bsub < ", filename)

  system(bsubCmd)
}
