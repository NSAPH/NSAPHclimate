### Functions to handle downloading of raster cliamte data and aggregate it to zip code


#' Download Gridmet data set, aggregate to zip code or county
#'
#' @param variable name of the variable to download
#' @param years vector of years of data to process
#' @param poly sf spatial polygoon object
#' @param points sf spatial points object
#' @param geoid.in The name of the column in the input shapefile(s) that has the ID of the geographical area
#' @param geoid.out What to name the geographic ID in the ooutput data
#' @param temp_dirname where should downloaded data be stored
#' @param outdir directory to store the output data
#' @param geoname What to name the geogrpahy in the output file
#' @param subdivide Should downloaded rasters be subdivided for finer area weighting?
#' @param cache should the downloaded data be stored
#'
#' @importFrom ncdf4 nc_open nc_close
#' @importFrom raster brick
#' @export
get_gridmet <- function(variable,
                        years = 1999:2019,
                        poly,
                        points = NULL,
                        geoid.in = "ZIP",
                        geoid.out = "zip",
                        temp_dirname = "temp",
                        outdir = "out",
                        geo_name = "zip",
                        subdivide = T,
                        cache = T) {

  ## Create temporary directory if it doesn't already exist
  if (!dir.exists(temp_dirname)) {
    dir.create(temp_dirname)
  }

  ## download grid met files
  ## files are small enough (~3GB per variable) that we don't need to worry about storage
  for (year in years) {
    if (!file.exists(file.path(temp_dirname, paste0(variable, "_", year, ".nc"))))
    download_gridmet(variable, year, temp_dirname)
  }

  for (year in years) {
    ## read in ncdf data
    ncin <- nc_open(paste0(temp_dirname,"/", variable, "_", year, ".nc"))
    varname <- names(ncin$var)[1]
    nc_close(ncin)

    rast <- brick(paste0(temp_dirname,"/", variable, "_", year, ".nc"), varname = varname)
    year_data <- process_year_polygon(rast, poly, variable, geoid.in, geoid.out, subdivide)

    if (!is.null(points)) {
      year_data <- rbind(year_data, process_year_point(rast, points, variable, geoid.in, geoid.out)[!get(geoid.out) %in% year_data[[geoid.out]]])
    }

    setkeyv(year_data, "date")

    fwrite(year_data, file.path(outdir, paste0(variable, "_", geo_name, "_", year, ".csv")))


  }


  ## If we're not caching delete files
  if (!cache) {
    lapply(list.files(path = temp_dirname,
                      pattern = paste0(variable, "_[0-9]{4}.nc"),
                      full.names = T),
           file.remove)
    #if temp dir is empty, delete it too
    if (length(dir) == 0) {
      unlink(dir)
    }
  }

}

#' Download grid met data and store in the temporary directory
#'
#' @param variable Gridmet variable to download
#' @param year year of data to download
#' @param temp_dirname directory to store the downloaded file in
#'
#' @export
download_gridmet <- function(variable, year, temp_dirname) {

  url <- paste0("https://www.northwestknowledge.net/metdata/data/", variable, "_", year, ".nc")
  outname <- paste0(temp_dirname,"/", variable, "_", year, ".nc")
  utils::download.file(url, outname)
}

#' Create a data frame of daily zip code area weighted estiamtes
#'
#' @importFrom raster nlayers disaggregate
#' @importFrom velox velox
#' @import data.table
process_year_polygon <- function(rast, poly, variable, geoid.in, geoid.out, subdivide) {
  out <- NULL
  for (i in 1:nlayers(rast)) {
    temp_layer <- rast[[i]]
    date <- as.Date(temp_layer@z[[1]], origin = "1900-01-01")
    cat(paste("processing",date, "\n"))

    if (subdivide) temp_layer <- disaggregate(temp_layer, fact = 5)

    temp_df <- as.data.table(velox(temp_layer)$extract(poly, small = T, fun = function(x){mean(x, na.rm = T)})[,1])
    names(temp_df) <- variable
    temp_df[, date := date]
    temp_df[, (geoid.out) := poly[[geoid.in]]]
    out <- rbind(out, temp_df)
  }

  return(out)
}



process_year_point <- function(rast, point, variable, geoid.in, geoid.out) {
  out <- NULL
  for (i in 1:nlayers(rast)) {
    temp_layer <- rast[[i]]
    date <- as.Date(temp_layer@z[[1]], origin = "1900-01-01")
    cat(paste("processing",date, "\n"))
    temp_df <- data.table(velox(temp_layer)$extract_points(point)[,1])
    names(temp_df) <- variable
    temp_df[, date := date]
    temp_df[, (geoid.out) := point[[geoid.in]]]
    out <- rbind(out, temp_df)
  }

  return(out)

}





