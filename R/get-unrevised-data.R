

#' Wrapper function around epiforecast::mimicPastEpidataDF to return partially
#' revised CDC flu data available as of the specified epiweek (but with missing
#' values filled in with more recent data from ilinet)
#'
#' @param region_str character defining region of interest, must be in c("nat", paste0("hhs", 1:10))
#' @param epiweek_str character string defining an epiweek in YYYYWW format
#'
#' @return a dataset in similar format to that returned by the Delphi epidata API
#'
#' @examples
#''
#' @export
get_partially_revised_ilinet <- function(region_str, epiweek_str) {
  require(epiforecast)
  require(dplyr)

  ## from ilinet, via DELPHI API
  unrevised_data <- readRDS(file.path(
    find.package("cdcFlu20172018"),
    "data",
    "flu_data_with_backfill.rds"))

  ## from ilinet, via cdcfluview package for R
  final_data <- readRDS(file.path(
    find.package("cdcFlu20172018"),
    "data",
    "flu_data_final_for_prospective_predictions.rds"))

  combined_data <- rbind(unrevised_data, final_data) %>%
    dplyr::filter(region == region_str)

  temp <- epiforecast::mimicPastEpidataDF(
      combined_data,
      epiweek_str) %>%
    dplyr::filter(epiweek <= as.integer(epiweek_str)) %>%
    as.data.frame()

  return(temp)
}
