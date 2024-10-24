#' Read FLEXPART Trajectory Data and Process Cloud and Temperature Variables
#'
#' This function reads and processes FLEXPART trajectory data from a NetCDF file. It extracts key variables such as longitude, latitude, height, temperature, ice water content (IWC), and liquid water content (LWC). The function applies thresholds to identify clouds, converts temperatures from Kelvin to Celsius, and handles potential edge cases with the data.
#'
#' @param fn A character string specifying the path to the NetCDF file containing the FLEXPART trajectory data.
#'
#' @return A data frame with processed trajectory data, including time, particle, temperature, latitude, longitude, height, cloud flags (ice and liquid water), and a temporary origin identifier.
#'
#' @details
#' - The temperature is converted from Kelvin to Celsius.
#' - Thresholds are applied to the IWC and LWC variables to identify clouds.
#' - The function also accounts for a known issue with the first and last time steps in the data.
#'
#' @importFrom ncdf4 ncvar_get ncatt_get nc_close
#' @importFrom dplyr mutate arrange select
#'
read_flexpart <- function(fn) {

  threshold_iwc <- 1E-7
  threshold_lwc <- 1E-7

  ## Read the Flexpart trajectory data
  nc_flex <- safe_open_ncdf(fn)
  if (is.null(nc_flex)) {
    file.remove(fn)
    return(NULL)
  }

  lon <- ncdf4::ncvar_get(nc_flex, "longitude")
  lat <- ncdf4::ncvar_get(nc_flex, "latitude")
  height <- ncdf4::ncvar_get(nc_flex, "height")

  time_traj <- c(ncdf4::ncvar_get(nc_flex, "time"))
  particle <- c(ncdf4::ncvar_get(nc_flex, "particle"))

  time_init_base <- as.POSIXct(ncdf4::ncatt_get(nc_flex, "DARDAR_time")$units, format = "Seconds from %Y-%m-%d %H:%M:%S", tz = "UTC")

  time_dardar <- ncdf4::ncvar_get(nc_flex, "DARDAR_time")
  time_dardar <- time_dardar + as.numeric(time_init_base)

  iwc <- ncdf4::ncvar_get(nc_flex, "IWC")
  lwc <- ncdf4::ncvar_get(nc_flex, "LWC")
  ta <- ncdf4::ncvar_get(nc_flex, "temperature")

  ncdf4::nc_close(nc_flex)

  ## Because variables at the first initialization time are set to zero
  ta[1, ] <- ta[2, ]

  ##  Convert variables
  ta <- ta - 273.15 # temperatures to celcius

  ## Basic process of the data
  time_traj <- time_traj / 3600
  lon <- ifelse(lon > 180, lon - 360, lon)

  ## Identify clouds and apply filters
  flag_ice <- iwc > threshold_iwc & !is.na(iwc)
  flag_liq <- lwc > threshold_lwc & !is.na(lwc)

  ## This is a fix for the last time step, to consider the last time step as ice.
  ## It will allow the ice origin identification to work properly in case there is no other ice parcel within the 120 hours.
  ## Could be improved
  flag_ice[127, ] <- TRUE

  origin_tmp <- as.numeric(flag_ice & flag_liq)

  ## iwc <- ifelse(flag_ice, iwc, 0)
  ## lwc <- ifelse(flag_liq, lwc, 0)

  ## Convert to data frame
  idx_time <- 1:length(time_traj)
  idx_particle <- 1:length(particle)

  df_flex <- data.frame(expand.grid(idx_time = idx_time,
                                    idx_particle = idx_particle)) %>%
    dplyr::mutate(time_traj = time_traj[idx_time],
                  particle = particle[idx_particle],
                  time_dardar = time_dardar[idx_particle],
                  lat = c(lat),
                  lon = c(lon),
                  height = c(height),
                  ta = c(ta),
                  flag_ice = c(flag_ice),
                  flag_liq = c(flag_liq),
                  origin_tmp = c(origin_tmp)
                  ) %>%
    dplyr::arrange(idx_particle, time_traj) %>%
    dplyr::select(-c(idx_time, idx_particle))

  rm(lat, lon, height, time_traj, particle, time_dardar, iwc, lwc, flag_ice, flag_liq, ta, origin_tmp, idx_time, idx_particle)
  gc()

  return(df_flex)

}

#' Safely Open a NetCDF File
#'
#' This function attempts to open a NetCDF file using `ncdf4::nc_open`. If an error occurs (e.g., the file cannot be opened), it catches the error and returns `NULL` instead of stopping execution.
#'
#' @param file_path A character string specifying the path to the NetCDF file to be opened.
#'
#' @return An object of class `ncdf4` if the file is successfully opened, or `NULL` if an error occurs.
#'
#' @details
#' This function uses `tryCatch` to handle errors that may occur when opening a NetCDF file. If the file cannot be opened, an error message is printed, and the function returns `NULL`.
#'
#' @importFrom ncdf4 nc_open
#'
safe_open_ncdf <- function(file_path) {
  tryCatch({
    ncdf4::nc_open(file_path)
  }, error = function(e) {
    cat("Error opening NetCDF file:", e$message, "\n")
    NULL  # Return NULL or another appropriate value on error
  })
}
