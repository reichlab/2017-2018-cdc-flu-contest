library(lubridate)
library(plyr)
library(dplyr)
library(reshape)
library(kcde)
library(doMC)
library(forecast)
library(cdcFlu20172018)
library(np)


save_path <- "inst/estimation/kcde/fits-np-loso-eval"
region <- "National"
season_val <- "2000-2001"
prediction_horizon <- 12L

first_test_season <- "2010/2011"
#save_path <- ""



## subset data to be only the region of interest
flu_data <- flu_data[flu_data$region == region,]

## Subset data to do estimation using only data up through first test season
first_ind_test_season <- min(which(flu_data$season == first_test_season))
flu_data <- flu_data[seq_len(first_ind_test_season - 1), , drop = FALSE]


res <- data.frame(season_val = NA, prediction_horizon = NA,
# fhat_np = NA,
 fhat_old = NA)
combined_fhat_np <- c()
combined_fhat_old <- c()

for(season_val in paste0(2003:2008, "-", 2004:2009)) {
  for(prediction_horizon in c(1:35)) {
#  for(season_val in paste0(1997:2009, "-", 1998:2010)) {
#    for(prediction_horizon in c(1, 6, 12)) {

#    kcde_fhat_np <- readRDS(
#      file = file.path(save_path,
#        paste0("kcde_fhat_np_loso_eval-",
#          region,
#          "-",for(season2003 in 8aste0(1200420009 "-", 1998:2010)) {
#  for(prediction_horizon in c(1, 6, 12)) {
#  for(season_val in paste0(1997:2009, "-", 1998:2010)) {
#    for(prediction_horizon in c(1, 6, 12)) {

#          season_val,
#          "-",
##          prediction_horizon,
#          ".rds")
#      )
#    )

    combined_fhat_np <- c(combined_fhat_np, kcde_fhat_np)

    save_path_old <- "~/Documents/research/epi/ensembles/adaptively-weighted-ensemble/inst/estimation/kcde/fits/"

    lambda_old <- readRDS(
      paste0(save_path_old,
        "box_cox_lambda-X-prediction_horizon_", prediction_horizon,
        "-max_lag_1-seasonality_TRUE-season_left_out_", season_val,
        ".rds")
    )

    kcde_fit_old <- readRDS(
      paste0(save_path_old,
        "kcde_fit-X-prediction_horizon_", prediction_horizon,
        "-max_lag_1-seasonality_TRUE-season_left_out_", season_val,
        ".rds")
    )

    data <- flu_data %>% filter(region == "National")
    data$box_cox_trans_weighted_ili <- BoxCox(data$weighted_ili, lambda_old)
    season_inds <- which(data$season == gsub("-", "/", season_val))

    fhat_old <- sapply(season_inds, function(ind) {
      samples <- kcde_predict(
        p = runif(n = 10^6, min = 0, max = 1),
        n = 10^5,
        kcde_fit = kcde_fit_old,
        prediction_data = data[seq_len(ind), ],
        leading_rows_to_drop = 0L,
        trailing_rows_to_drop = 0L,
        additional_training_rows_to_drop = NULL,
        prediction_type = "quantile",
        log = TRUE
      )

      if(identical(samples, NA)) {
        return(NULL)
      } else {
        bw <- density(as.numeric(samples))$bw
        obs_val <- data$weighted_ili[ind + prediction_horizon]
        ind_dens <- sapply(as.numeric(samples),
          function(mean_val) { dnorm(obs_val, mean = mean_val, sd = bw, log = TRUE)})

        return(exp(logspace_sum(ind_dens) - log(length(samples))))
      }
    })

    fhat_old <- unlist(fhat_old)[!is.na(unlist(fhat_old))]

    combined_fhat_old <- c(combined_fhat_old, fhat_old)

    res <- rbind(res,
      data.frame(season_val = season_val,
        prediction_horizon = prediction_horizon,
#        fhat_np = mean(log(kcde_fhat_np)),
        fhat_old = mean(log(fhat_old))))
  }
}




combined_fhat_np_0 <- c()
ph_0 <- c()
for(season_val in paste0(1997:2009, "-", 1998:2010)) {
  for(prediction_horizon in c(1, 6, 12)) {

    kcde_fhat_np_0 <- readRDS(
      file = file.path(save_path,
        paste0("kcde_fhat_np_loso_eval-",
          region,
          "-",
          season_val,
          "-",
          "0",
          "-",
          prediction_horizon,
          ".rds")
      )
    )

    combined_fhat_np_0 <- c(combined_fhat_np_0, kcde_fhat_np_0)
    ph_0 <- c(ph_0, rep(prediction_horizon, length(kcde_fhat_np_0)))
  }
}


combined_fhat_np_1 <- c()
ph_1 <- c()
for(season_val in paste0(1997:2009, "-", 1998:2010)) {
  for(prediction_horizon in c(1, 6, 12)) {

    kcde_fhat_np <- readRDS(
      file = file.path(save_path,
        paste0("kcde_fhat_np_loso_eval-",
          region,
          "-",
          season_val,
          "-",
          prediction_horizon,
          ".rds")
      )
    )

    combined_fhat_np_1 <- c(combined_fhat_np_1, kcde_fhat_np)
    ph_1 <- c(ph_1, rep(prediction_horizon, length(kcde_fhat_np)))
  }
}


combined_fhat_np_2 <- c()
ph_2 <- c()
for(season_val in paste0(1997:2009, "-", 1998:2010)) {
  for(prediction_horizon in c(1, 6, 12)) {

    kcde_fhat_np_2 <- readRDS(
      file = file.path(save_path,
        paste0("kcde_fhat_np_loso_eval-",
          region,
          "-",
          season_val,
          "-",
          "2",
          "-",
          prediction_horizon,
          ".rds")
      )
    )

    combined_fhat_np_2 <- c(combined_fhat_np_2, kcde_fhat_np_2)
    ph_2 <- c(ph_2, rep(prediction_horizon, length(kcde_fhat_np_2)))
  }
}


combined_fhat_np_3 <- c()
ph_3 <- c()
for(season_val in paste0(1997:2009, "-", 1998:2010)) {
  for(prediction_horizon in c(1, 6, 12)) {

    kcde_fhat_np_3 <- readRDS(
      file = file.path(save_path,
        paste0("kcde_fhat_np_loso_eval-",
          region,
          "-",
          season_val,
          "-",
          "3",
          "-",
          prediction_horizon,
          ".rds")
      )
    )

    combined_fhat_np_3 <- c(combined_fhat_np_3, kcde_fhat_np_3)
    ph_3 <- c(ph_3, rep(prediction_horizon, length(kcde_fhat_np_3)))
  }
}







combined_fhat_np_0_ns <- c()
ph_0_ns <- c()
for(season_val in paste0(1997:2009, "-", 1998:2010)) {
  for(prediction_horizon in c(1, 6, 12)) {

    kcde_fhat_np_0 <- readRDS(
      file = file.path(save_path,
        paste0("kcde_fhat_np_loso_eval-",
          region,
          "-",
          season_val,
          "-",
          "0",
          "-",
          "seasonality_FALSE-",
          prediction_horizon,
          ".rds")
      )
    )

    combined_fhat_np_0_ns <- c(combined_fhat_np_0_ns, kcde_fhat_np_0)
    ph_0_ns <- c(ph_0_ns, rep(prediction_horizon, length(kcde_fhat_np_0)))
  }
}


combined_fhat_np_1_ns <- c()
ph_1_ns <- c()
for(season_val in paste0(1997:2009, "-", 1998:2010)) {
  for(prediction_horizon in c(1, 6, 12)) {

    kcde_fhat_np <- readRDS(
      file = file.path(save_path,
        paste0("kcde_fhat_np_loso_eval-",
          region,
          "-",
          season_val,
          "-",
          "1",
          "-",
          "seasonality_FALSE-",
          prediction_horizon,
          ".rds")
      )
    )

    combined_fhat_np_1_ns <- c(combined_fhat_np_1_ns, kcde_fhat_np)
    ph_1_ns <- c(ph_1_ns, rep(prediction_horizon, length(kcde_fhat_np)))
  }
}


combined_fhat_np_2_ns <- c()
ph_2_ns <- c()
for(season_val in paste0(1997:2009, "-", 1998:2010)) {
  for(prediction_horizon in c(1, 6, 12)) {

    kcde_fhat_np_2 <- readRDS(
      file = file.path(save_path,
        paste0("kcde_fhat_np_loso_eval-",
          region,
          "-",
          season_val,
          "-",
          "2",
          "-",
          "seasonality_FALSE-",
          prediction_horizon,
          ".rds")
      )
    )

    combined_fhat_np_2_ns <- c(combined_fhat_np_2_ns, kcde_fhat_np_2)
    ph_2_ns <- c(ph_2_ns, rep(prediction_horizon, length(kcde_fhat_np_2)))
  }
}


combined_fhat_np_3_ns <- c()
ph_3_ns <- c()
for(season_val in paste0(1997:2009, "-", 1998:2010)) {
  for(prediction_horizon in c(1, 6, 12)) {

    kcde_fhat_np_3 <- readRDS(
      file = file.path(save_path,
        paste0("kcde_fhat_np_loso_eval-",
          region,
          "-",
          season_val,
          "-",
          "3",
          "-",
          "seasonality_FALSE-",
          prediction_horizon,
          ".rds")
      )
    )

    combined_fhat_np_3_ns <- c(combined_fhat_np_3_ns, kcde_fhat_np_3)
    ph_3_ns <- c(ph_3_ns, rep(prediction_horizon, length(kcde_fhat_np_3)))
  }
}


combined_fhat_np_4_ns <- c()
ph_4_ns <- c()
for(season_val in paste0(1997:2009, "-", 1998:2010)) {
  for(prediction_horizon in c(1, 6, 12)) {

    kcde_fhat_np_4 <- readRDS(
      file = file.path(save_path,
        paste0("kcde_fhat_np_loso_eval-",
          region,
          "-",
          season_val,
          "-",
          "4",
          "-",
          "seasonality_FALSE-",
          prediction_horizon,
          ".rds")
      )
    )

    combined_fhat_np_4_ns <- c(combined_fhat_np_4_ns, kcde_fhat_np_4)
    ph_4_ns <- c(ph_4_ns, rep(prediction_horizon, length(kcde_fhat_np_4)))
  }
}


combined_fhat_np_5_ns <- c()
ph_5_ns <- c()
for(season_val in paste0(1997:2009, "-", 1998:2010)) {
  for(prediction_horizon in c(1, 6, 12)) {

    kcde_fhat_np_5 <- readRDS(
      file = file.path(save_path,
        paste0("kcde_fhat_np_loso_eval-",
          region,
          "-",
          season_val,
          "-",
          "5",
          "-",
          "seasonality_FALSE-",
          prediction_horizon,
          ".rds")
      )
    )

    combined_fhat_np_5_ns <- c(combined_fhat_np_5_ns, kcde_fhat_np_5)
    ph_5_ns <- c(ph_5_ns, rep(prediction_horizon, length(kcde_fhat_np_5)))
  }
}


combined_fhat_np_6_ns <- c()
ph_6_ns <- c()
for(season_val in paste0(1997:2009, "-", 1998:2010)) {
  for(prediction_horizon in c(1, 6, 12)) {

    kcde_fhat_np_6 <- readRDS(
      file = file.path(save_path,
        paste0("kcde_fhat_np_loso_eval-",
          region,
          "-",
          season_val,
          "-",
          "6",
          "-",
          "seasonality_FALSE-",
          prediction_horizon,
          ".rds")
      )
    )

    combined_fhat_np_6_ns <- c(combined_fhat_np_6_ns, kcde_fhat_np_6)
    ph_6_ns <- c(ph_6_ns, rep(prediction_horizon, length(kcde_fhat_np_6)))
  }
}


combined_fhat_np_7_ns <- c()
ph_7_ns <- c()
for(season_val in paste0(1997:2009, "-", 1998:2010)) {
  for(prediction_horizon in c(1, 6, 12)) {

    kcde_fhat_np_7 <- readRDS(
      file = file.path(save_path,
        paste0("kcde_fhat_np_loso_eval-",
          region,
          "-",
          season_val,
          "-",
          "7",
          "-",
          "seasonality_FALSE-",
          prediction_horizon,
          ".rds")
      )
    )

    combined_fhat_np_7_ns <- c(combined_fhat_np_7_ns, kcde_fhat_np_7)
    ph_7_ns <- c(ph_7_ns, rep(prediction_horizon, length(kcde_fhat_np_7)))
  }
}


combined_fhat_np_8_ns <- c()
ph_8_ns <- c()
for(season_val in paste0(1997:2009, "-", 1998:2010)) {
  for(prediction_horizon in c(1, 6, 12)) {

    kcde_fhat_np_8 <- readRDS(
      file = file.path(save_path,
        paste0("kcde_fhat_np_loso_eval-",
          region,
          "-",
          season_val,
          "-",
          "8",
          "-",
          "seasonality_FALSE-",
          prediction_horizon,
          ".rds")
      )
    )

    combined_fhat_np_8_ns <- c(combined_fhat_np_8_ns, kcde_fhat_np_8)
    ph_8_ns <- c(ph_8_ns, rep(prediction_horizon, length(kcde_fhat_np_8)))
  }
}


combined_fhat_np_9_ns <- c()
ph_9_ns <- c()
for(season_val in paste0(1997:2009, "-", 1998:2010)) {
  for(prediction_horizon in c(1, 6, 12)) {

    kcde_fhat_np_9 <- readRDS(
      file = file.path(save_path,
        paste0("kcde_fhat_np_loso_eval-",
          region,
          "-",
          season_val,
          "-",
          "9",
          "-",
          "seasonality_FALSE-",
          prediction_horizon,
          ".rds")
      )
    )

    combined_fhat_np_9_ns <- c(combined_fhat_np_9_ns, kcde_fhat_np_9)
    ph_9_ns <- c(ph_9_ns, rep(prediction_horizon, length(kcde_fhat_np_9)))
  }
}


combined_fhat_np_10_ns <- c()
ph_10_ns <- c()
for(season_val in paste0(1997:2009, "-", 1998:2010)) {
  for(prediction_horizon in c(1, 6, 12)) {

    kcde_fhat_np_10 <- readRDS(
      file = file.path(save_path,
        paste0("kcde_fhat_np_loso_eval-",
          region,
          "-",
          season_val,
          "-",
          "10",
          "-",
          "seasonality_FALSE-",
          prediction_horizon,
          ".rds")
      )
    )

    combined_fhat_np_10_ns <- c(combined_fhat_np_10_ns, kcde_fhat_np_10)
    ph_10_ns <- c(ph_10_ns, rep(prediction_horizon, length(kcde_fhat_np_10)))
  }
}


mean(log(combined_fhat_np_0))
mean(log(combined_fhat_np_1))
mean(log(combined_fhat_np_2))
mean(log(combined_fhat_np_3))
mean(log(combined_fhat_np_0_ns))
mean(log(combined_fhat_np_1_ns))
mean(log(combined_fhat_np_2_ns))
mean(log(combined_fhat_np_3_ns))
mean(log(combined_fhat_np_4_ns))
mean(log(combined_fhat_np_5_ns))
mean(log(combined_fhat_np_6_ns))
mean(log(combined_fhat_np_7_ns))
mean(log(combined_fhat_np_8_ns))
mean(log(combined_fhat_np_9_ns))
mean(log(combined_fhat_np_10_ns))

combined_df <- rbind(
  data.frame(
    fhat_val = combined_fhat_np_0,
    combo = "lag 0, seasonal",
    ph = ph_0
  ),
  data.frame(
    fhat_val = combined_fhat_np_1,
    combo = "lag 1, seasonal",
    ph = ph_1
  ),
  data.frame(
    fhat_val = combined_fhat_np_2,
    combo = "lag 2, seasonal",
    ph = ph_2
  ),
  data.frame(
    fhat_val = combined_fhat_np_3,
    combo = "lag 3, seasonal",
    ph = ph_3
  ),
  data.frame(
    fhat_val = combined_fhat_np_0_ns,
    combo = "lag 0, nonseasonal",
    ph = ph_0_ns
  ),
  data.frame(
    fhat_val = combined_fhat_np_1_ns,
    combo = "lag 1, nonseasonal",
    ph = ph_1_ns
  ),
  data.frame(
    fhat_val = combined_fhat_np_2_ns,
    combo = "lag 2, nonseasonal",
    ph = ph_2_ns
  ),
  data.frame(
    fhat_val = combined_fhat_np_3_ns,
    combo = "lag 3, nonseasonal",
    ph = ph_3_ns
  ),
  data.frame(
    fhat_val = combined_fhat_np_4_ns,
    combo = "lag 4, nonseasonal",
    ph = ph_4_ns
  ),
  data.frame(
    fhat_val = combined_fhat_np_5_ns,
    combo = "lag 5, nonseasonal",
    ph = ph_5_ns
  ),
  data.frame(
    fhat_val = combined_fhat_np_6_ns,
    combo = "lag 6, nonseasonal",
    ph = ph_6_ns
  ),
  data.frame(
    fhat_val = combined_fhat_np_7_ns,
    combo = "lag 7, nonseasonal",
    ph = ph_7_ns
  ),
  data.frame(
    fhat_val = combined_fhat_np_8_ns,
    combo = "lag 8, nonseasonal",
    ph = ph_8_ns
  ),
  data.frame(
    fhat_val = combined_fhat_np_9_ns,
    combo = "lag 4, nonseasonal",
    ph = ph_9_ns
  ),
  data.frame(
    fhat_val = combined_fhat_np_10_ns,
    combo = "lag 10, nonseasonal",
    ph = ph_10_ns
  )
)
combined_df$log_fhat <- log(combined_df$fhat_val)


tapply(combined_df$log_fhat, combined_df[, c("combo", "ph")], mean)


library(ggplot2)

ggplot(data = combined_df) +
  geom_freqpoly(aes(x = fhat_val, colour = combo, group = combo)) +
  theme_bw()

ggplot(data = combined_df %>%
  filter(combo %in% paste0("lag ", 0:4, ", nonseasonal"))) +
  geom_freqpoly(aes(x = fhat_val, colour = combo, group = combo), bins = 100) +
  theme_bw()

ggplot(data = combined_df %>%
  filter(combo %in% paste0("lag ", 0:10, ", nonseasonal"))) +
  geom_freqpoly(aes(x = fhat_val, y = ..density.., colour = combo, group = combo), bins = 40) +
  facet_wrap(~ factor(ph)) +
  theme_bw()

ggplot(data = combined_df %>%
  filter(combo %in% paste0("lag ", 0:10, ", nonseasonal") &
    ph == 1)) +
  geom_freqpoly(aes(x = fhat_val, y = ..density.., colour = combo, group = combo), bins = 20) +
  facet_wrap(~ factor(ph)) +
  theme_bw()

ggplot(data = combined_df %>%
  filter(combo %in% paste0("lag ", 1:7, ", nonseasonal") &
    ph == 1)) +
  geom_freqpoly(aes(x = fhat_val, y = ..density.., colour = combo, group = combo), bins = 40) +
  facet_wrap(~ factor(ph)) +
  theme_bw()

ggplot(data = combined_df %>%
  filter(combo %in% paste0("lag ", c(3, 5, 6, 7), ", nonseasonal") &
    ph == 1)) +
  geom_freqpoly(aes(x = fhat_val, y = ..density.., colour = combo, group = combo), bins = 100) +
  facet_wrap(~ factor(ph)) +
  theme_bw()












combined_df <- data.frame(
  season = NA,
  prediction_horizon = NA,
  max_lag = NA,
  fhat = NA,
  log_fhat = NA
)

for(season_val in paste0(1997:2009, "-", 1998:2010)) {
  for(prediction_horizon in 1:52) {
    for(max_lag in 0:8) {
      if(file.exists(file.path(save_path,
        paste0("kcde_fhat_np_loso_eval-",
          region,
          "-",
          season_val,
          "-",
          max_lag,
          "-",
          "seasonality_FALSE-",
          prediction_horizon,
          ".rds")
      ))) {
        kcde_fhat <- readRDS(
          file = file.path(save_path,
            paste0("kcde_fhat_np_loso_eval-",
              region,
              "-",
              season_val,
              "-",
              max_lag,
              "-",
              "seasonality_FALSE-",
              prediction_horizon,
              ".rds")
          )
        )

        combined_df <- rbind(
          combined_df,
          data.frame(
            season = season_val,
            prediction_horizon = prediction_horizon,
            max_lag = max_lag,
            fhat = kcde_fhat,
            log_fhat = log(kcde_fhat)
          )
        )
      }
    }
  }
}

tapply(combined_df$log_fhat, combined_df[, c("max_lag", "prediction_horizon")], mean)
tapply(combined_df$log_fhat, combined_df[, c("max_lag", "prediction_horizon", "season")], length)

ggplot(data = combined_df %>%
    filter(season %in% paste0(2003:2008, "-", 2004:2009)) %>%
    group_by(max_lag, prediction_horizon) %>%
    summarize(mean_log_score = mean(log_fhat))) +
  geom_line(aes(x = prediction_horizon, y = mean_log_score, colour = factor(max_lag), group = factor(max_lag))) +
  theme_bw()


ggplot(data = combined_df %>%
  filter(max_lag %in% 0:8)) +
  geom_freqpoly(aes(x = fhat_val, y = ..density.., colour = combo, group = combo), bins = 40) +
  facet_wrap(~ factor(prediction_horizon)) +
  theme_bw()










bw <- readRDS(
  file = file.path(save_path,
    paste0("kcde_fit_np_loso_eval-",
      region,
      "-",
      season_val,
      "-",
      prediction_horizon,
      ".rds")
  )
)

prediction_target_var <- "weighted_ili"

lambda <- BoxCox.lambda(flu_data$weighted_ili, method = "loglik")
flu_data$box_cox_trans_weighted_ili <-
  BoxCox(flu_data[[prediction_target_var]], lambda)


flu_data$sin_sq_t <- sin(flu_data$time_index * pi / 365.2425)^2
flu_data$cos_sq_t <- cos(flu_data$time_index * pi / 365.2425)^2

temp <- compute_offset_obs_vecs(data = flu_data,
  filter_control = NULL,
  phi = NULL,
  vars_and_offsets = data.frame(
      var_name = c(rep(prediction_target_var, max_lag + 1 + 1), "sin_sq_t", "cos_sq_t", "season"),
      offset_type = c(rep("lag", max_lag + 1), "lead", "lag", "lag", "lag"),
      offset_value = c(0, seq_len(max_lag), prediction_horizon, 0, 0, 0),
      stringsAsFactors = FALSE
    ) %>%
    mutate(
      combined_name = paste0(var_name, "_", offset_type, offset_value)
    ),
  time_name = NULL,
  leading_rows_to_drop = 0,
  trailing_rows_to_drop = 0,
  additional_rows_to_drop = NULL,
  na.action = "na.omit")


get_fitted <- function(yvals) {
  sapply(yvals, function(yval) {
    fitted(npcdens(
          exdat = temp[which(temp$season == season_val)[1], c("weighted_ili_lag0", "weighted_ili_lag1", "sin_sq_t_lag0", "cos_sq_t_lag0"), drop = FALSE],
          eydat = yval,
          bws = bw))
  })
}

fitted_vals <- get_fitted(seq(from = 0, to = 1000, length = 10^5))
y_vals <- seq(from = 0, to = 1000, length = 10^5)
plot(fitted_vals ~ y_vals)


integrate(get_fitted,
  lower = 0,
  upper = 10^2)
