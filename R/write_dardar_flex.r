#' Write Combined DARDAR and FLEXPART Data to a NetCDF File
#'
#' This function writes combined DARDAR and FLEXPART data to a new NetCDF file. It creates the necessary variables and dimensions based on the input data and writes the values for particle information, origin, cloud properties, temperature, and geographic information.
#'
#' @param df_dardar_flex A data frame containing the combined DARDAR and FLEXPART data, with variables such as `n_part`, `origin`, `origin_quality`, `dt_cloud`, `ta_origin`, `lat_origin`, and `lon_origin`.
#' @param fn_dardar A character string specifying the file path to the original DARDAR NetCDF file, used to extract base variables such as latitude, longitude, and time.
#' @param fn_out A character string specifying the file path for the output NetCDF file. If the file exists, it will be overwritten.
#'
#' @return Returns `NULL`. The function creates a new NetCDF file at the specified location with the processed and combined data.
#'
#' @details
#' The function first reads the original DARDAR NetCDF file to extract dimensions and base variables. It then merges the data from `df_dardar_flex` into a full grid, fills missing values with a specified fill value, and creates the necessary variables in the NetCDF file. The new file contains variables for the number of particles, origin, origin quality, temperature at ice formation, latitude, and longitude, among others.
#'
#' @importFrom ncdf4 nc_open nc_create ncvar_def ncvar_put nc_close
#' @importFrom dplyr left_join mutate select
#' @importFrom reshape2 acast
#'
write_dardar_flex <- function(df_dardar_flex, fn_dardar, fn_out) {

  if(file.exists(fn_out)) null <- file.remove(fn_out)

  fill_value <- -999

  ## Read the original DARDAR data to extract base variables
  nc_dardar <- ncdf4::nc_open(fn_dardar)

  ## Information on dimentions
  dim_time <- nc_dardar$dim$time
  dim_height <- nc_dardar$dim$height

  var_height <- c(ncdf4::ncvar_get(nc_dardar, "height"))

  ## Variable definitions
  vardef_lat <- nc_dardar$var$lat
  vardef_lon <- nc_dardar$var$lon
  vardef_basetime <- nc_dardar$var$base_time
  vardef_dtime <- nc_dardar$var$dtime

  ## Variable values
  var_lat <- c(ncdf4::ncvar_get(nc_dardar, "lat"))
  var_lon <- c(ncdf4::ncvar_get(nc_dardar, "lon"))
  var_basetime <- c(ncdf4::ncvar_get(nc_dardar, "base_time"))
  var_dtime <- c(ncdf4::ncvar_get(nc_dardar, "dtime"))

  ncdf4::nc_close(nc_dardar)

  ## Dimentions indices
  n_time <- dim_time$len
  n_height <- dim_height$len
  idx_time_full <- 1:n_time
  idx_height_full <- 1:n_height

  ## Create a full data frame with all dimension indices
  df_full <- data.frame(expand.grid(idx_height = idx_height_full,
                                    idx_time = idx_time_full))

  ## Join the FLEX data with partial indice levels to the full data frame
  df_joint <- dplyr::left_join(df_full, df_dardar_flex,
                               by = c("idx_time", "idx_height"))

  ## Create matrices containing the FLEX variables
  var_n_part <- reshape2::acast(df_joint, idx_height ~ idx_time, value.var = "n_part")
  attr(var_n_part, "dimnames") <- NULL
  var_n_part[is.na(var_n_part)] <- fill_value

  var_origin_quality <- reshape2::acast(df_joint, idx_height ~ idx_time, value.var = "origin_quality")
  attr(var_origin_quality, "dimnames") <- NULL
  var_origin_quality[is.na(var_origin_quality)] <- fill_value

  var_origin <- reshape2::acast(df_joint, idx_height ~ idx_time, value.var = "origin")
  attr(var_origin, "dimnames") <- NULL
  var_origin[is.na(var_origin)] <- fill_value

  var_origin_sd <- reshape2::acast(df_joint, idx_height ~ idx_time, value.var = "origin_sd")
  attr(var_origin_sd, "dimnames") <- NULL
  var_origin_sd[is.na(var_origin_sd)] <- fill_value

  var_dt_cloud <- reshape2::acast(df_joint, idx_height ~ idx_time, value.var = "dt_cloud")
  attr(var_dt_cloud, "dimnames") <- NULL
  var_dt_cloud[is.na(var_dt_cloud)] <- fill_value

  var_ta <- reshape2::acast(df_joint, idx_height ~ idx_time, value.var = "ta_origin")
  attr(var_ta, "dimnames") <- NULL
  var_ta[is.na(var_ta)] <- fill_value

  var_lat_orig <- reshape2::acast(df_joint, idx_height ~ idx_time, value.var = "lat_origin")
  attr(var_lat_orig, "dimnames") <- NULL
  var_lat_orig[is.na(var_lat_orig)] <- fill_value

  var_lon_orig <- reshape2::acast(df_joint, idx_height ~ idx_time, value.var = "lon_origin")
  attr(var_lon_orig, "dimnames") <- NULL
  var_lon_orig[is.na(var_lon_orig)] <- fill_value

  ## Create a new NetCDF file
  l_dim <- list(dim_height, dim_time)

  ## Define variables
  vardef_n_part <- ncdf4::ncvar_def(
                            name = "n_part", units = "-", longname = "Number of particles (FLEXPART)",
                            dim = l_dim, missval = fill_value, compression = 5, prec = "integer"
                          )

  vardef_origin_quality <- ncdf4::ncvar_def(
                            name = "origin_quality", units = "-", longname = "Number of particles (FLEXPART)",
                            dim = l_dim, missval = fill_value, compression = 5, prec = "integer"
                          )

  vardef_origin <- ncdf4::ncvar_def(
                            name = "origin", units = "-", longname = "Ice cloud origin",
                            dim = l_dim, missval = fill_value, compression = 5, prec = "float"
                          )

  vardef_origin_sd <- ncdf4::ncvar_def(
                               name = "origin_sd", units = "-", longname = "Ice cloud origin standard deviation",
                               dim = l_dim, missval = fill_value, compression = 5, prec = "float"
                             )

  vardef_dt_cloud <- ncdf4::ncvar_def(
                            name = "dt_cloud", units = "-", longname = "Ice cloud dt_cloud",
                            dim = l_dim, missval = fill_value, compression = 5, prec = "float"
                          )

  vardef_ta <- ncdf4::ncvar_def(
                        name = "ta_origin", units = "K", longname = "Temperature at ice formation",
                        dim = l_dim, missval = fill_value, compression = 5, prec = "float"
                      )

  vardef_lat_orig <- ncdf4::ncvar_def(
                       name = "lat_origin", units = "", longname = "Latitude at ice formation",
                       dim = l_dim, missval = fill_value, compression = 5, prec = "float"
                     )

  vardef_lon_orig <- ncdf4::ncvar_def(
                              name = "lon_origin", units = "", longname = "Longitude at ice formation",
                              dim = l_dim, missval = fill_value, compression = 5, prec = "float"
                            )



  l_var <- list(vardef_lat, vardef_lon, vardef_basetime, vardef_dtime,
                vardef_n_part, vardef_origin_quality,
                vardef_origin, vardef_origin_sd, vardef_dt_cloud, vardef_ta,
                vardef_lat_orig, vardef_lon_orig)

  if(file.exists(fn_out)) null <- file.remove(fn_out)
  nc_new <- ncdf4::nc_create(fn_out, l_var, force_v4 = TRUE)

  ## Put data into variables
  ncdf4::ncvar_put(nc_new, vardef_lat, var_lat)
  ncdf4::ncvar_put(nc_new, vardef_lon, var_lon)
  ncdf4::ncvar_put(nc_new, vardef_basetime, var_basetime)
  ncdf4::ncvar_put(nc_new, vardef_dtime, var_dtime)
  ncdf4::ncvar_put(nc_new, vardef_n_part, var_n_part)
  ncdf4::ncvar_put(nc_new, vardef_origin_quality, var_origin_quality)
  ncdf4::ncvar_put(nc_new, vardef_origin, var_origin)
  ncdf4::ncvar_put(nc_new, vardef_origin_sd, var_origin_sd)
  ncdf4::ncvar_put(nc_new, vardef_dt_cloud, var_dt_cloud)
  ncdf4::ncvar_put(nc_new, vardef_ta, var_ta)
  ncdf4::ncvar_put(nc_new, vardef_lat_orig, var_lat_orig)
  ncdf4::ncvar_put(nc_new, vardef_lon_orig, var_lon_orig)

  ## Close the NetCDF file to write and save it
  ncdf4::nc_close(nc_new)

  return(NULL)

}


