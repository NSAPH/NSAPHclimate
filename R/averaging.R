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
#' @export
seasonal_averages <- function(var, file_stem, years, geoid = "zip", outdir = "../data/out") {
  out <- NULL
  for (year in years) {
    print(year)
    if (!file.exists(paste0(file_stem, year, ".csv"))) {
      warning("data for ", year, " not available")
      next()
    }

    winter_start <- as.Date(paste0(year - 1, "-12-01"))
    winter_end <- as.Date(paste0(year, "-03-01"))
    summer_start <- as.Date(paste0(year, "-06-01"))
    summer_end <- as.Date(paste0(year, "-09-01"))
    print(winter_start)

    year_data <- fread(paste0(file_stem, year, ".csv"))
    if (file.exists(paste0(file_stem, year-1, ".csv"))) {
      year_data <- rbind(fread(paste0(file_stem, year-1, ".csv")), year_data)
    }
    year_data[, date := as.Date(date)]

    winter_data <- year_data[date >= winter_start & date < winter_end,
                             lapply(.SD, mean, na.rm = T),
                             by = geoid,
                             .SD = var]
    setnames(winter_data, var, paste0("winter_", var))

    summer_data <- year_data[date >= summer_start & date < summer_end,
                             lapply(.SD, mean, na.rm = T),
                             by = geoid,
                             .SD = var]
    setnames(summer_data, var, paste0("summer_", var))
    year_data <- merge(summer_data, winter_data, by = geoid)

    year_data[, year := year]
    out <- rbind(out, year_data)
  }

  return(out)
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
#' @export
annual_averages <- function(var, file_stem, years, geoid = "zip", outdir = "../data/out") {
  out <- NULL
  for (year in years) {
    if (!file.exists(paste0(file_stem, year, ".csv"))) {
      warning("data for ", year, " not available")
      next()
    }
    year_data <- fread(paste0(file_stem, year, ".csv"))
    year_data <- year_data[, lapply(.SD, mean, na.rm = T), by = geoid, .SDcols = var]
    year_data[, year := year]
    out <- rbind(out, year_data)
  }
  return(out)
}
