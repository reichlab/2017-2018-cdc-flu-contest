options(warn = 2, error = recover)

all_data_sets <- c("National", paste0("Region-", 1:10))
all_data_sets <- "National"
all_prediction_horizons <- as.character(seq_len(35))
all_prediction_horizons <- c(1, 6, 12)
#all_max_lags <- as.character(c(0L, 1L, 2L, 3L))
all_max_lags <- as.character(c(5L, 6L, 7L, 8L, 9L, 10L))
#all_seasonality_values <- c("TRUE")
all_seasonality_values <- c("FALSE")
all_first_test_seasons <- "2010/2011"

cores_req <- "4"
mem_req <- "5000"
time_req <- "96:00"
queue_req <- "long"

for(data_set in all_data_sets) {
  for(prediction_horizon in all_prediction_horizons) {
    for(max_lag in all_max_lags) {
      for(seasonality in all_seasonality_values) {
        for(first_test_season in all_first_test_seasons) {
          save_path <- "/home/er71a/2017-2018-cdc-flu-contest/inst/estimation/kcde/fits-np-loso-eval"
          output_path <- "/home/er71a/2017-2018-cdc-flu-contest/inst/estimation/kcde/cluster-output-np-loso-eval"
          lsfoutfilename <- "kcde-np-loso-eval.out"

          case_descriptor <- paste0(
            data_set,
            "-prediction_horizon_", prediction_horizon,
            "-max_lag_", max_lag,
            "-seasonality_", seasonality,
            "-first_test_season_", gsub("/", "-", first_test_season)
          )
          filename <- paste0(output_path, "/submit-kcde-estimation-step-", case_descriptor, ".sh")

          requestCmds <- "#!/bin/bash\n"
          requestCmds <- paste0(requestCmds, "#BSUB -n ", cores_req, " # how many cores we want for our job\n",
            "#BSUB -R span[hosts=1] # ask for all the cores on a single machine\n",
            "#BSUB -R rusage[mem=", mem_req, "] # ask for memory\n",
            "#BSUB -o ", lsfoutfilename, " # log LSF output to a file\n",
            "#BSUB -W ", time_req, " # run time\n",
            "#BSUB -q ", queue_req, " # which queue we want to run in\n")

          cat(requestCmds, file = filename)
          cat("module load R/3.4.0\n", file = filename, append = TRUE)
          cat(paste0("R CMD BATCH --vanilla \'--args ",
                     data_set, " ",
                     prediction_horizon, " ",
                     max_lag, " ",
                     seasonality, " ",
                     first_test_season, " ",
                     save_path,
                     "\'  /home/er71a/2017-2018-cdc-flu-contest/inst/estimation/kcde/kcde-np-loso-eval.R ",
                     output_path, "/output-kde-estimation-step-", case_descriptor, ".Rout"),
              file = filename, append = TRUE)

          bsubCmd <- paste0("bsub < ", filename)

          system(bsubCmd)
        } # first_test_season
      } # seasonality
    } # max_lag
  } # prediction_horizon
} # data_set
