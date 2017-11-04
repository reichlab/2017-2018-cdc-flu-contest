

#' Wrapper function around epiforecast::mimicPastEpidataDF to return partially
#' revised CDC flu data available as of the specified epiweek (but with missing
#' values filled in with more recent data from ilinet).  On top of
#' epiforecast::mimicPastEpidataDF, this function: 1) uses a static pull of
#' data from the delphi API, stored in the data directory of this package;
#' 2) subsets data to a specified region; and 3) drops some occasional extra
#' rows of NA's at the end of the output from epiforecast::mimicPastEpidataDF.
#'
#' @param region_str character defining region of interest, must be in c("nat", paste0("hhs", 1:10))
#' @param epiweek_str character string defining an epiweek in YYYYWW format
#'
#' @return a dataset in similar format to that returned by the Delphi epidata API
#'
#'
#' @export
get_partially_revised_ilinet <- function(region_str, epiweek_str) {
  require(epiforecast)
  require(dplyr)

  ## from ilinet, via DELPHI API
  partially_revised_data <- readRDS(file.path(
      find.package("cdcFlu20172018"),
      "data",
      "flu_data_with_backfill.rds")) %>%
    dplyr::filter(region == region_str)

  temp <- epiforecast::mimicPastEpidataDF(
      partially_revised_data,
      epiweek_str) %>%
    dplyr::filter(epiweek <= as.integer(epiweek_str)) %>%
    as.data.frame()

  return(temp)
}
