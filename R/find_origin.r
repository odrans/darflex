#' Determine Final Cloud Origin and Quality for FLEXPART Trajectories
#'
#' This function refines the origin of cloud formation in FLEXPART trajectories using a series of case conditions.
#' It assigns a final origin (`origin_final`) and a quality score based on temperature and trajectory data.
#' The output includes the final origin, associated cloud data (latitude, longitude, temperature), and quality flags.
#'
#' @param df_in A data frame containing the input trajectory data. This should include variables such as `dt_ice`,
#' `always_cold`, `always_warm`, and others required for origin determination.
#'
#' @return A data frame with particle trajectories and their final cloud origin (`origin`), along with associated cloud parameters
#' (latitude, longitude, temperature) and a quality flag (`origin_quality`).
#'
#' @details
#' The function applies several case conditions to assess the origin of clouds and the reliability of the origin identification.
#' It uses pre-determined conditions such as `always_cold`, `always_warm`, and `dt_ice` to adjust the final origin and quality flag.
#'
#' @importFrom dplyr mutate select rename
#'
find_origin_overpass <- function(df_in) {

  df_origin <- find_origin_wernli(df_in)

  df_origin_final <- df_origin %>%
    dplyr::mutate(origin_final = origin,
                  quality = 0) %>%
    dplyr::mutate(
             case_1 = dt_ice == 0,
             case_2 = (dt_ice > 0 & dt_ice < 126) & origin == 0 & always_cold,
             case_3 = (dt_ice > 0 & dt_ice < 126) & origin == 0 & !always_cold,
             case_4 = (dt_ice > 0 & dt_ice < 126) & origin == 1 & always_warm,
             case_5 = (dt_ice > 0 & dt_ice < 126) & origin == 1 & !always_warm,
             case_6 = dt_ice == 126 & always_cold,
             case_7 = dt_ice == 126 & always_warm,
             case_8 = dt_ice == 126 & !always_cold & !always_warm,
             origin_final = ifelse(case_6, 0, origin_final),
             origin_final = ifelse(case_7, 1, origin_final),
             origin_final = ifelse(case_8, 1, origin_final),
             quality = ifelse(case_1 | case_2 | case_6, 1, quality),
             quality = ifelse(case_3 | case_5 | case_8, 0, quality),
             quality = ifelse(case_4 | case_7, 0.5, quality),
             dt_cloud = ifelse(case_1, dt_cloud, NA),
             lat = ifelse(case_1, lat, NA),
             lon = ifelse(case_1, lon, NA),
             ta = ifelse(case_1, ta, NA)
           ) %>%
    dplyr::select(particle, origin_final, dt_cloud, lat, lon, ta, quality) %>%
    dplyr::rename(origin = origin_final,
                  origin_quality = quality,
                  lat_origin = lat,
                  lon_origin = lon,
                  ta_origin = ta)

  return(df_origin_final)
}


#' Identify Ice Cloud Origin and Characteristics in FLEXPART Trajectories
#'
#' This function identifies the origin of ice formation in FLEXPART trajectories by analyzing temperature and cloud flags.
#' It determines the time, location, and temperature at the point of ice formation and calculates the time difference between
#' the formation and the overpass.
#'
#' @param df_in A data frame containing FLEXPART trajectory data, including cloud and temperature variables such as `flag_ice`,
#' `origin_tmp`, `time_traj`, `lat`, `lon`, and `ta`.
#'
#' @return A data frame summarizing the origin of the ice cloud for each particle,
#' including the time difference between ice formation and overpass (`dt_cloud`),
#' the time difference between the last ice cloud and overpass (`dt_ice`), and information on whether the trajectory
#' remained in a cold or warm region. It also provides the latitude, longitude, and temperature at the point of ice formation.
#'
#' @details
#' The function uses temperature thresholds (e.g., below -40°C) to flag regions as cold and identifies when
#' ice formation occurs in the trajectory. The function summarizes the trajectory data to determine the origin of the ice
#' cloud and other related metrics.
#'
#' @importFrom dplyr mutate group_by arrange lag summarize
#'
find_origin_wernli <- function(df_in) {

  df_origin <- df_in %>%
    dplyr::mutate(flag_region_hom = ta < -40) %>%
    ## Group by particle and arrange by time
    dplyr::group_by(particle) %>%
    dplyr::arrange(time_traj) %>%
    ## Identify ice formation and calculate origin
    dplyr::mutate(
             flag_noice_tm1 = dplyr::lag(flag_ice, 1, default = FALSE) == FALSE, ## No ice at previous time step
             ice_forming = flag_ice & flag_noice_tm1 ## Identify ice formation (could add more time steps here)
           ) %>%
    ## Summarize to find the last ice formation and associated metrics
    dplyr::summarize(
             idx_last_ice = max(which(flag_ice)), ## Latest index for an ice cloud
             idx_last_origin = max(which(ice_forming)), ## Latest index for ice origin (meaning the "earliest", idx = 127 means overpass time)
             origin = origin_tmp[idx_last_origin], ## Attribute the ice origin origin
             dt_cloud = last(as.numeric(time_traj)) - as.numeric(time_traj[idx_last_origin]), ## Time difference between ice origin and overpass
             dt_ice = last(as.numeric(time_traj)) - as.numeric(time_traj[idx_last_ice]), ## Time difference between last ice cloud and overpass
             always_cold = all(flag_region_hom[idx_last_ice:127]), ## Always cold region
             always_warm = all(!flag_region_hom[idx_last_ice:127]), ## Always cold region
             lat = lat[idx_last_origin], ## Latitude at ice origin
             lon = lon[idx_last_origin], ## Longitude at ice origin
             ta = ta[idx_last_origin], ## Temperature at ice origin
             .groups = "drop"
           ) %>%
    data.frame()

  ## Implicit return
  df_origin
}
