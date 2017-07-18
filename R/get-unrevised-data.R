

#' Wrapper function to return unrevised CDC flu data
#'
#' @param region_str character defining region of interest, must be in c("nat", paste0("hhs", 1:10))
#' @param epiweek character string defining an epiweek in YYYYWW format
#'
#' @return a dataset in same format returned by `cdcfluview::get_flu_data()`
#' @export
#'
#' @examples
get_unrevised_data <- function(region_str, epiweek) {
  require(epiforecast)
  require(dplyr)

  unrevised_data <- readRDS("data/flu_data_with_backfill.rds")
  temp <- mimicPastEpidataDF(filter(unrevised_data, region == region_str), epiweek) %>% 
    as.data.frame()
  
  return(temp)
}
