#' Merge FLEXPART Overpass Data with DARDAR Data
#'
#' This function merges FLEXPART overpass data with DARDAR data by binning the time and height variables into matching grids. It computes summary statistics for the FLEXPART data in each bin and joins it with the corresponding DARDAR data.
#'
#' @param df_flex_overpass A data frame containing FLEXPART overpass data, including columns for height, origin, time, and other related variables.
#' @param df_dardar A data frame containing DARDAR data with columns for time, height, latitude, longitude, and other related variables.
#'
#' @return A merged data frame where DARDAR data is augmented with FLEXPART overpass data, binned by time and height. The merged data includes summary statistics such as origin standard deviation, mean origin, and cloud-related information for each bin.
#'
#' @details
#' The function bins both FLEXPART and DARDAR data into specified intervals for time, height, latitude, and longitude. It then computes summary statistics (mean, standard deviation, etc.) for the FLEXPART data within each bin and merges it with the DARDAR data at the corresponding binned time and height levels.
#'
#' @importFrom dplyr select filter group_by summarize left_join mutate
#' @importFrom plotutils bin
#'
merge_dardar_flex <- function(df_flex_overpass, df_dardar) {

  bins_lat <- seq(-90, 90, 0.03)
  bins_lon <- seq(-180, 180, 5)
  bins_height <- c(0, sort(unique(df_flex_overpass$height)))

  time_min <- as.numeric(min(df_dardar$time, na.rm = TRUE))
  time_max <- as.numeric(max(df_dardar$time, na.rm = TRUE))
  bins_time <- seq(time_min - 60, time_max + 60, 0.5)

  ## bins_time <- as.numeric(unique(df_flex_overpass$time_dardar))
  ## bins_time <- bins_time[!is.na(bins_time)]

  df_flex_grid <- df_flex_overpass %>%
    dplyr::select(height, origin, dt_cloud, origin_quality,
                  ta_origin, lat_origin, lon_origin, time_dardar) %>%
    dplyr::mutate(time = time_dardar) %>%
    plotutils::bin(time, bins_time) %>%
    plotutils::bin(height, bins_height) %>%
    dplyr::filter(!is.na(time_bin), !is.na(height_bin)) %>%
    dplyr::group_by(time_bin, height_bin) %>%
    dplyr::summarize(
             origin_sd = sd(origin, na.rm = TRUE),
             origin = mean(origin, na.rm = TRUE),
             dt_cloud = mean(dt_cloud, na.rm = TRUE),
             ta_origin = mean(ta_origin, na.rm = TRUE),
             lat_origin = mean(lat_origin, na.rm = TRUE),
             lon_origin = mean(lon_origin, na.rm = TRUE),
             n_part = n(),
             origin_quality = mean(origin_quality),
             .groups = "drop"
             ) %>%
    data.frame() %>%
    dplyr::mutate(origin = ifelse(is.nan(origin), NA, origin),
                  origin_sd = ifelse(is.nan(origin_sd), NA, origin_sd),
                  dt_cloud = ifelse(is.nan(dt_cloud), NA, dt_cloud),
                  ta_origin = ifelse(is.nan(ta_origin), NA, ta_origin),
                  lat_origin = ifelse(is.nan(lat_origin), NA, lat_origin),
                  lon_origin = ifelse(is.nan(lon_origin), NA, lon_origin),
                  origin_quality = ifelse(is.nan(origin_quality), NA, origin_quality)
                  )

  df_dardar_grid <- df_dardar %>%
    mutate(time = as.numeric(time)) %>%
    plotutils::bin(time, bins_time) %>%
    plotutils::bin(height, bins_height) %>%
    dplyr::filter(!is.na(time_bin), !is.na(height_bin)) %>%
    dplyr::select(c(time_bin, height_bin, idx_height, idx_time))

  df_joint <- dplyr::left_join(df_dardar_grid,
                               df_flex_grid,
                               by = c("time_bin", "height_bin"),
                               ) %>%
    dplyr::select(-c(time_bin, height_bin))

  df_dardar_flex <- df_dardar %>%
    dplyr::left_join(df_joint,
                     by = c("idx_time", "idx_height"))

  return(df_dardar_flex)

}
