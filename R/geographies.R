## Geographies.R
## Code to handle reading/processing shape files

#' Read in polygon and point shape from the standard
#' zip code structure
#' @param shape_path path to the directory containing the zip code shape files
#' @param year year of zip codes to grab
#'
#' @return a list with the polygon and point shpae files foor a given year
#' @details Assumes the folder structure in the zip_for_loop directory
#' @importFrom sf st_as_sf read_sf
#' @importFrom dplyr select
#' @importFrom stringr str_c str_sub  str_subset
#' @export
get_year_zip_shapefiles <- function(shape_path, year) {
  if (year < 2000) year <- 2000 # no files pre 2000 available, use 2000
  if (year >= 2018) year <- 2017 #no file after 2017, use 2017

  polyfiles <- list.files(str_c(shape_path, "polygon", sep = "/"),
                          full.names = TRUE,
                          pattern = "\\.shp$")
  pointfiles <- list.files(str_c(shape_path, "pobox_csv", sep = "/"),
                           full.names = TRUE,
                           pattern = "\\.csv$")
  year_abbrev <- str_sub(year, 3, 4)
  polyfile <- str_subset(polyfiles, str_c(year_abbrev, "USZIP", sep = ""))
  pointfile <- str_subset(pointfiles, str_c(year_abbrev, "USZIP", sep = ""))

  out <- list()
  out$point <- fread(pointfile)
  out$point <- st_as_sf(out$point, coords = c("POINT_X", "POINT_Y"))
  out$poly <- read_sf(polyfile)
  out$poly <- select(out$poly, ZIP)



  return(out)

}
