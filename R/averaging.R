## averaging.R
## functions for temporal averaging (annual + seasonal)

#' Calculate seasonal averages for a variable
#'
#' @param var variable name containing the value to be averaged
#' @param file_stem the stem of all files that contain daily data for a variable. Should include
#'        paths along with the file stem. Assumes that all files have this name followed
#'        by the year of data. For example, temperature data named "tmmx_zip_yyyy.csv" stored in
#'        "data/temperature" would have "data/temperature/tmmx_zip_" as the file stem.
#' @param years years to create seasonal averages for.
#' @param geoid variable name containing the unique ID for each geographical area.
#' @param outdir Directory to save output in.  Output data is saved there
#'        in a file names "[var]_seasonal_averages.csv"
#'
#' @details
#' Assumes that for a given year \code{y}, winter runs from
#' December 1st, y-1 to February 28/29, y and that summer
#' runs from June 1st, y to August 31st, y. Future functionality
#' may allow for customization of winter and summer intervals. If data
#' for a previous year is not available, data from the start of the year, onward is used.
#'
#'
#' @importFrom lubridate leap_year
#' @export
seasonal_averages <- function(var, file_stem, years, geoid = "zip", outdir = "../data/out") {
  out <- NULL
  for (year in years) {
      winter_start <- as.Date(paste0(year - 1, "-12-01"))
      winter_end <- as.Date(paste0(year, "-03-01"))
      summer_start <- as.Date(paste0(year, "-06-01"))
      summer_end <- as.Date(paste0(year, "-09-01"))

  }
}

#' Calculate Annual Averages for a variable
#'
#' @param var variable name containing the value to be averaged
#' @param file_stem the stem of all files that contain daily data for a variable. Should include
#'        paths along with the file stem. Assumes that all files have this name followed
#'        by the year of data. For example, temperature data named "tmmx_zip_yyyy.csv" stored in
#'        "data/temperature" would have "data/temperature/tmmx_zip_" as the file stem.
#' @param years years to create annual averages for.
#' @param geoid variable name containing the unique ID for each geographical area.
#' @param outdir Directory to save output in.  Output data is saved there
#'        in a file names "[var]_annual_averages.csv"
#'
annual_averages <- function(var, file_stem, years, geoid = "zip", outdir = "../data/out") {

}
