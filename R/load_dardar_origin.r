#' Load and Process 2D DARDAR Origin Data
#'
#' This function loads and processes 2D DARDAR origin and related DARDAR data, including DARDAR cloud radiative effect (CRE) data. It combines cloud properties such as ice water content (IWC), ice concentration, and temperature at cloud top, and merges it with DARDAR flux data (CRE).
#'
#' @param fn_dardar_origin A character string specifying the file path to the DARDAR origin NetCDF file.
#' @param lf_dardar A character vector containing the list of available DARDAR NetCDF file paths.
#' @param lf_dardar_flux A character vector containing the list of available DARDAR CRE NetCDF file paths.
#'
#' @return A data frame containing combined cloud properties from the DARDAR and DARDAR origin files, including latitude, longitude, ice water path (IWP), ice concentration (ICNC), cloud top temperature (CTT), and cloud radiative effects (CRE).
#'
#' @details
#' The function attempts to match the DARDAR origin file with the corresponding DARDAR and DARDAR flux files based on the file name. It verifies that the dimensions of these files are consistent, then extracts and combines cloud-related data such as origin, IWC, ice concentration, temperature, and cloud radiative effects. Data is filtered based on cloud mask, mixed-phase flags, and iteration flags.
#'
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @importFrom dplyr mutate filter group_by summarize select
#' @return A processed data frame with key cloud properties and radiative effects.
#' @export
load_dardar_origin_2D <- function(fn_dardar_origin, lf_dardar, lf_dardar_flux) {

  idx_name_dardar <- gsub(".nc", "", strsplit(basename(fn_dardar_origin), "_", fixed = TRUE)[[1]][5])
  fn_dardar <- lf_dardar[which(grepl(idx_name_dardar, lf_dardar))]
  fn_dardar_flux <- lf_dardar_flux[which(grepl(idx_name_dardar, lf_dardar_flux))]
  if(!length(fn_dardar) | !length(fn_dardar_flux)) {
    return(NULL)
  }

  nc_dardar_origin <- ncdf4::nc_open(fn_dardar_origin)
  nc_dardar <- ncdf4::nc_open(fn_dardar)
  nc_dardar_flux <- ncdf4::nc_open(fn_dardar_flux)

  len_height_origin <- nc_dardar_origin$dim$height$len
  len_time_origin <- nc_dardar_origin$dim$time$len

  len_height <- nc_dardar$dim$height$len
  len_time <- nc_dardar$dim$time$len

  len_time_flux <- nc_dardar_flux$dim$point$len

  if(len_height != len_height_origin | len_time != len_time_origin | len_time != len_time_flux) {
    return(NULL)
  }

  df <- data.frame(expand.grid(idx_height = 1:len_height_origin,
                               idx_time = 1:len_time_origin)) %>%
    dplyr::mutate(
             origin = c(ncdf4::ncvar_get(nc_dardar_origin, "origin")),
             ##origin_flag = c(ncdf4::ncvar_get(nc_dardar_origin, "origin_flag")),
             layer_index = c(ncdf4::ncvar_get(nc_dardar, "layer_index")),
             dz_top = c(ncdf4::ncvar_get(nc_dardar, "dz_top")),
             iwc = c(ncdf4::ncvar_get(nc_dardar, "iwc")),
             icnc = c(ncdf4::ncvar_get(nc_dardar, "icnc_5um")),
             ta = c(ncdf4::ncvar_get(nc_dardar, "ta")),
             clm = c(ncdf4::ncvar_get(nc_dardar, "clm")),
             flag_mixed = c(ncdf4::ncvar_get(nc_dardar, "mixedphase_flag"))
             ) %>%
    dplyr::filter(!is.na(origin), !is.na(layer_index), iwc > 1E-8, clm == 1, flag_mixed == 0) %>%
    dplyr::group_by(idx_time, layer_index) %>%
    dplyr::summarize(
             origin_cloud = mean(origin),
             H = max(dz_top) *1E-3,
             iwp = sum(iwc) * 60 * 1E3,
             icncc = sum(icnc) * 60 * 1E-6,
             ctt = ta[which.min(dz_top)],
             .groups = "drop"
             ) %>%
    data.frame() %>%
    dplyr::mutate(
             lat = c(ncdf4::ncvar_get(nc_dardar, "lat"))[idx_time],
             lon = c(ncdf4::ncvar_get(nc_dardar, "lon"))[idx_time],
             cre_ice_sw = c(ncdf4::ncvar_get(nc_dardar_flux, "cre_ice_sw"))[idx_time],
             cre_ice_lw = c(ncdf4::ncvar_get(nc_dardar_flux, "cre_ice_lw")[idx_time]),
             iteration_flag =  c(ncdf4::ncvar_get(nc_dardar, "iteration_flag")[idx_time])
           ) %>%
    dplyr::filter(iteration_flag == 1) %>%
    dplyr::select(-c(layer_index, iteration_flag))


  ncdf4::nc_close(nc_dardar_origin)
  ncdf4::nc_close(nc_dardar)
  ncdf4::nc_close(nc_dardar_flux)

  return(df)

}


#' Load and Process 3D DARDAR Origin Data
#'
#' This function loads DARDAR origin and corresponding DARDAR data from NetCDF files, processes key variables such as origin, cloud properties, ice water content (IWC), and temperature. It filters the data based on cloud masks and mixed-phase flags and returns the processed data.
#'
#' @param fn_dardar_origin A character string specifying the file path to the DARDAR origin NetCDF file.
#' @param lf_dardar A character vector containing the list of available DARDAR files.
#'
#' @return A data frame containing the processed DARDAR origin data combined with the DARDAR file, including cloud properties, ice water content (IWC), and temperature at ice formation.
#'
#' @details
#' This function matches the DARDAR origin file to the corresponding DARDAR file and reads both to extract key variables. It filters the data for valid cloud layers and non-mixed-phase clouds and combines the data into a single data frame.
#'
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @importFrom dplyr mutate filter select
#' @export
load_dardar_origin_3D <- function(fn_dardar_origin, lf_dardar) {

  idx_name_dardar <- gsub(".nc", "", strsplit(basename(fn_dardar_origin), "_", fixed = TRUE)[[1]][5])
  fn_dardar <- lf_dardar[which(grepl(idx_name_dardar, lf_dardar))]
  if(!length(fn_dardar)) {
    return(NULL)
  }

  nc_dardar_origin <- ncdf4::nc_open(fn_dardar_origin)
  nc_dardar <- ncdf4::nc_open(fn_dardar)

  len_height_origin <- nc_dardar_origin$dim$height$len
  len_time_origin <- nc_dardar_origin$dim$time$len

  len_height <- nc_dardar$dim$height$len
  len_time <- nc_dardar$dim$time$len

  if(len_height != len_height_origin | len_time != len_time_origin) {
    return(NULL)
  }

  df <- data.frame(expand.grid(idx_height = 1:len_height_origin,
                               idx_time = 1:len_time_origin)) %>%
    dplyr::mutate(
             origin = c(ncdf4::ncvar_get(nc_dardar_origin, "origin")),
             dt_cloud = c(ncdf4::ncvar_get(nc_dardar_origin, "dt_cloud")),
             origin_flag = c(ncdf4::ncvar_get(nc_dardar_origin, "origin_flag")),
             iwc = c(ncdf4::ncvar_get(nc_dardar, "iwc")),
             icnc = c(ncdf4::ncvar_get(nc_dardar, "icnc_5um")),
             clm = c(ncdf4::ncvar_get(nc_dardar, "clm")),
             ta = c(ncdf4::ncvar_get(nc_dardar, "ta")),
             flag_mixed = c(ncdf4::ncvar_get(nc_dardar, "mixedphase_flag")),
             dz_top = c(ncdf4::ncvar_get(nc_dardar, "dz_top")),
             layer_index = c(ncdf4::ncvar_get(nc_dardar, "layer_index")),
             ta_origin = c(ncdf4::ncvar_get(nc_dardar_origin, "ta_origin"))
             ) %>%
    dplyr::filter(!is.na(origin), iwc > 1E-8, clm == 1, flag_mixed == 0) %>%
    dplyr::mutate(
             lat = c(ncdf4::ncvar_get(nc_dardar, "lat"))[idx_time],
             lon = c(ncdf4::ncvar_get(nc_dardar, "lon"))[idx_time],
             height = c(ncdf4::ncvar_get(nc_dardar, "height"))[idx_height],
             iteration_flag =  c(ncdf4::ncvar_get(nc_dardar, "iteration_flag")[idx_time])
           ) %>%
    dplyr::filter(iteration_flag == 1) %>%
    dplyr::select(-c(iteration_flag, flag_mixed, clm))

  ncdf4::nc_close(nc_dardar_origin)
  ncdf4::nc_close(nc_dardar)

  return(df)

}
