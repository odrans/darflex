#' Read and Process DARDAR Data from a NetCDF File
#'
#' This function reads DARDAR data from a NetCDF file and extracts variables such as latitude, longitude, height, and time.
#' The data is returned as a data frame with indexed height and time levels.
#'
#' @param fn_dardar A character string specifying the file path to the DARDAR NetCDF file.
#'
#' @return A data frame containing the DARDAR data with columns for latitude (`lat`), longitude (`lon`), height (`height`),
#' time (`time`), and their corresponding indices (`idx_height`, `idx_time`).
#'
#' @details
#' The function reads specific variables like latitude, longitude, height, and time from the DARDAR NetCDF file using `ncdf4::ncvar_get`.
#' Time is adjusted using a base time value to return proper timestamps. The DARDAR data is structured in a data frame with expanded grids
#' for height and time indices.
#'
#' @importFrom ncdf4 nc_open nc_close
#' @importFrom dplyr mutate
#'
read_dardar <- function(fn_dardar) {

  ## Read the Dardar data
  ## df_dardar <- darnitools::l2_read(fn_dardar, keep_index = TRUE, filter_ice = TRUE, filter_quality = FALSE) %>%
  ##   dplyr::select(lat, lon, height, idx_height, idx_time)
  ## gc()

  nc <- ncdf4::nc_open(fn_dardar)
  nheight <- nc$dim$height$len
  ntime <- nc$dim$time$len

  ## Read the temperature, ice concentration, cloud mask, mixed phase flag, and ice water content
  df_dardar <- data.frame(expand.grid(idx_height = 1:nheight,
                                idx_time = 1:ntime)) %>%
    dplyr::mutate(
             lat = nc_read(nc, "lat", idx_time),
             lon = nc_read(nc, "lon", idx_time),
             height = nc_read(nc, "height", idx_height),
             time = nc_read(nc, "dtime", idx_time) + as.POSIXct(nc_read(nc, "base_time"), origin = "1970-01-01 00:00:00", tz = "UTC")
           )

  ncdf4::nc_close(nc)

  ## Read the DARDAR CRE data
  ## nc <- ncdf4::nc_open(fn_dardar_flux)
  ## df_cre <- data.frame(
  ##   lat = ncdf4::ncvar_get(nc, "latitude"),
  ##   lon = ncdf4::ncvar_get(nc, "longitude"),
  ##   cre_ice_sw = ncdf4::ncvar_get(nc, "cre_ice_sw"),
  ##   cre_ice_lw = ncdf4::ncvar_get(nc, "cre_ice_lw"),
  ##   time = ncdf4::ncvar_get(nc, "dtime") + as.POSIXct(ncdf4::ncvar_get(nc, "base_time"), origin = "1970-01-01 00:00:00", tz = "UTC")
  ## )
  ## ncdf4::nc_close(nc)

  ## df_dardar <- dplyr::left_join(df_dardar, df_cre, by = c("time")) %>%
  ##   dplyr::select(lat.x, lon.x, height, idx_height, idx_time) %>%
  ##   dplyr::rename(lat = lat.x, lon = lon.x)

  return(df_dardar)

}


#' Read Variable from a NetCDF File
#'
#' This function reads a variable from a NetCDF file, either entirely or by specific indices, and returns it as a numeric vector.
#'
#' @param nc A NetCDF file object, typically opened with `ncdf4::nc_open`.
#' @param varname A character string specifying the name of the variable to be read from the NetCDF file.
#' @param idx An optional vector of indices to subset the variable. If `NULL` (default), the entire variable is read.
#'
#' @return A numeric vector containing the values of the specified variable from the NetCDF file, either entirely or by the specified indices.
#'
#' @details
#' The function uses `ncdf4::ncvar_get` to extract the variable's data from the NetCDF file. If no indices are provided, the entire variable is read. If indices are provided, only the specified subset is returned.
#'
#' @importFrom ncdf4 ncvar_get
#'
nc_read <- function(nc, varname, idx = NULL) {
  if(is.null(idx)) {
    as.numeric(ncdf4::ncvar_get(nc, varname))
  } else {
    as.numeric(ncdf4::ncvar_get(nc, varname)[idx])
  }
}
