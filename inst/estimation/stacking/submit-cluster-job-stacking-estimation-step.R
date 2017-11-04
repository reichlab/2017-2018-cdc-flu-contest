options(warn = 2)

all_prediction_targets <- c('Season_onset',
  'Season_peak_week',
  'Season_peak_percentage',
  '1_wk_ahead',
  '2_wk_ahead',
  '3_wk_ahead',
  '4_wk_ahead')
#all_regions <- "National"
#all_prediction_targets <- "onset"

cores_req <- "4"
mem_req <- "15000"
time_req <- "120:00"
#queue_req <- "condo_uma_nicholas_reich"
queue_req <- "long"

for(prediction_target in all_prediction_targets) {
  output_path <- "/home/er71a/2017-2018-cdc-flu-contest/inst/estimation/stacking/cluster-output"
  lsfoutfilename <- "adaptively-weighted-ensemble-xgb-stack-est.out"

  case_descriptor <- paste0(
    "prediction_target_", prediction_target
  )
  filename <- paste0(output_path, "/submit-stacking-estimation-step-", case_descriptor, ".sh")

  requestCmds <- "#!/bin/bash\n"
  requestCmds <- paste0(requestCmds, "#BSUB -n ", cores_req, " # how many cores we want for our job\n",
    "#BSUB -R span[hosts=1] # ask for all the cores on a single machine\n",
    "#BSUB -R rusage[mem=", mem_req, "] # ask for memory\n",
    "#BSUB -o ", lsfoutfilename, " # log LSF output to a file\n",
    "#BSUB -W ", time_req, " # run time\n",
    "#BSUB -q ", queue_req, " # which queue we want to run in\n")

  cat(requestCmds, file = filename)
  cat("module load openmpi/2.0.1\n", file = filename, append = TRUE)
  cat("module load R/3.2.2\n", file = filename, append = TRUE)
  cat(paste0("R CMD BATCH --vanilla \'--args ",
    prediction_target, " ",
    cores_req,
    "\'  /home/er71a/2017-2018-cdc-flu-contest/inst/estimation/stacking/stacking_estimation.R ",
    output_path, "/output-stacking-estimation-", case_descriptor, ".Rout"),
    file = filename, append = TRUE)

  bsubCmd <- paste0("bsub < ", filename)

  system(bsubCmd)
} # prediction_target
