#' Load and Process FLEXPART and DARDAR Data
#'
#' This function loads FLEXPART and DARDAR data, computes the origin of particles from FLEXPART, and merges it with the DARDAR data. The combined data is then written to a new NetCDF file. Optionally, an existing output file can be overwritten.
#'
#' @param fn_flex A character string specifying the file path to the FLEXPART data.
#' @param lf_dardar A character vector of available DARDAR file paths.
#' @param dir_out A character string specifying the output directory where the processed file will be saved.
#' @param overwrite Logical, if `TRUE`, any existing output file with the same name will be overwritten. Default is `FALSE`.
#' @param suffix A character string appended to the output file name for distinguishing different runs. Default is an empty string.
#'
#' @return Returns `NULL`. The processed data is saved as a NetCDF file in the specified output directory.
#'
#' @details
#' The function identifies the correct DARDAR file corresponding to the FLEXPART file, computes particle origins using `find_origin_overpass`, and merges FLEXPART and DARDAR data at the time of overpass. The combined data is written to a new NetCDF file with the option to overwrite.
#'
#' @importFrom dplyr filter select inner_join
#' @export
load_data <- function(fn_flex, lf_dardar, dir_out, overwrite = FALSE, suffix = "") {

  ## Identify DARDAR file
  idx_name_dardar <- strsplit(gsub("_FLEX", "", basename(fn_flex)), "_", fixed = TRUE)[[1]][5]
  fn_dardar <- lf_dardar[which(grepl(idx_name_dardar, lf_dardar))]

  fn_out <- gsub("_L2_", "_origin_", basename(fn_dardar))
  fn_out <- paste0(dir_out, "/", fn_out)
  fn_out <- gsub(".nc", paste0("_", suffix, ".nc"), fn_out)
  if(overwrite) {
    if(file.exists(fn_out)) null <- file.remove(fn_out)
  } else {
    if(file.exists(fn_out)) return(NULL)
  }

  if((file.exists(fn_flex) & file.exists(fn_dardar)) == FALSE) {
    stop("Files not found")
  }

  ## Check the files
  print(paste0("fn_dardar: ", fn_dardar))
  print(paste0("fn_flex: ", fn_flex))
  print(paste0("fn_out: ", fn_out))

  ## Read the Flexpart data
  print("Reading FLEXPART data")
  df_flex <- read_flexpart(fn_flex)
  if(is.null(df_flex)) return(NULL)

  ## Compute the origin of the particles
  print("Computing particle origins")
  df_origin <- find_origin_overpass(df_flex)

  ## Keep the overpass information from Flexpart
  df_flex_overpass <- dplyr::inner_join(dplyr::filter(df_flex, time_traj == 0),
                                        dplyr::select(df_origin, c(particle, origin, origin_quality, dt_cloud,
                                                                   lat_origin, lon_origin, ta_origin)),
                                        by = "particle")
  rm(df_origin, df_flex)

  ## Read the DARDAR data
  print("Reading DARDAR data")
  df_dardar <- read_dardar(fn_dardar)

  ## Merge DARDAR with the FLEXPART data (at the overpass time)
  print("Merging FLEXPART and DARDAR data")
  df_dardar_flex <- merge_dardar_flex(df_flex_overpass, df_dardar)
  rm(df_flex_overpass, df_dardar)

  print(str(df_dardar_flex))

  ## Write the data
  print("Writing data")
  null <- write_dardar_flex(df_dardar_flex, fn_dardar, fn_out)
  rm(df_dardar_flex)
  gc()

  return(NULL)

}
