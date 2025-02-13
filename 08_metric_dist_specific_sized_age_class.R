library(terra)
library(fs)
library(here)
library(tools)
library(landscapemetrics)
library(readr)
library(cli)




# Vegetation age classes for all patches ----------------------------------

# USER - input unburnt patch size in hectares (calculates for patches larger than this number)
req_ha <- 20

# # Look for reporting region masks
msk_paths <- fs::dir_ls(here::here("reporting_masks"))
msk_names <- gsub("_msk", "", tools::file_path_sans_ext(basename(msk_paths)))

# # Look for tsf data
tsfs <- fs::dir_ls(here::here("mosaics", "04_time_since_fire"))
years <- readr::parse_number(basename(tsfs))

# age class classification matices
age6 <- c(-Inf, 5, 0,
          5, Inf, 1)
rcl6 <- matrix(age6, ncol=3, byrow=TRUE)

age10 <- c(-Inf, 9, 0,
           9, Inf, 1)
rcl10 <- matrix(age10, ncol=3, byrow=TRUE)

# # Empty date frame for results
dist_tsf_df <- dplyr::tibble()

for(i in seq_along(msk_paths)){
  reg_name <- msk_names[i]
  cli::cli_alert_info("Working on {reg_name}...hang tight")
  msk <- terra::rast(msk_paths[i])
  for(j in seq_along(tsfs)){
    tsfm <- terra::rast(tsfs[j]) |>
      terra::crop(msk, mask = TRUE)
    # make age classes
    tsfm6 <- terra::classify(tsfm, rcl6)
    tsfm10 <- terra::classify(tsfm, rcl10)
    year <- years[j]
    cli::cli_alert_info("Doing {year}...")
    ## work on age class 6+
    # catch for no patches
    if(terra::minmax(tsfm6)[2] == 1){
      # find patches
      tsfm6_patches <- landscapemetrics::get_patches(tsfm6, class = 1)[[1]][[1]]
      # find area of patches
      area_tsfm6_patches <- terra::cellSize(tsfm6_patches, unit="ha") |>
        terra::zonal(tsfm6_patches, sum)
      # find IDs of patches smaller than req_ha
      exclude_id <- area_tsfm6_patches$lyr.1[which(area_tsfm6_patches$area < req_ha)]
      # create raster with only right sized patches
      tsfm6_sized <- tsfm6_patches
      # need to catch instances with no patches of size
      if(length(exclude_id) != 0){
        tsfm6_sized[tsfm6_sized %in% exclude_id] <- NA
      }
      # munge metrics
      out_df6 <- suppressMessages(landscapemetrics::get_nearestneighbour(tsfm6_sized[[1]][[1]]) |>
        dplyr::mutate(region = reg_name,
                      year = year,
                      age_class = "6+") |>
        dplyr::select(-layer) |>
        dplyr::group_by(region, year, age_class) |>
        dplyr::summarise(min_m = min(dist, na.rm = TRUE),
                         max_m = max(dist, na.rm = TRUE),
                         range_m = max_m - min_m,
                         mean_m = mean(dist, na.rm = TRUE),
                         std_m = sd(dist, na.rm = TRUE),
                         sum_m = sum(dist, na.rm = TRUE),
                         median_m = median(dist, na.rm = TRUE)))
    } else {
      out_df6 <- dplyr::tibble(
        region = reg_name,
        year = year,
        age_class = "6+",
        min_m = NA,
        max_m = NA,
        range_m = NA,
        mean_m = NA,
        std_m = NA,
        sum_m = NA,
        median_m = NA)
    }

    # update results data frame
    dist_tsf_df <- dplyr::bind_rows(dist_tsf_df, out_df6)

    ## work on age class 10+

    if(terra::minmax(tsfm10)[2] == 1){
      # find patches
      tsfm10_patches <- landscapemetrics::get_patches(tsfm10, class = 1)[[1]][[1]]
      # find area of patches
      area_tsfm10_patches <- terra::cellSize(tsfm10_patches, unit="ha") |>
        terra::zonal(tsfm10_patches, sum)
      # find IDs of patches smaller than req_ha
      exclude_id <- area_tsfm10_patches$lyr.1[which(area_tsfm10_patches$area < req_ha)]
      # create raster with only right sized patches
      tsfm10_sized <- tsfm10_patches
      # need to catch instances with no patches of size
      if(length(exclude_id) != 0){
        tsfm10_sized[tsfm10_sized %in% exclude_id] <- NA
      }
      # munge metrics
      out_df10 <- suppressMessages(landscapemetrics::get_nearestneighbour(tsfm10_sized[[1]][[1]]) |>
        dplyr::mutate(region = reg_name,
                      year = year,
                      age_class = "10+") |>
        dplyr::select(-layer) |>
        dplyr::group_by(region, year, age_class) |>
        dplyr::summarise(min_m = min(dist, na.rm = TRUE),
                         max_m = max(dist, na.rm = TRUE),
                         range_m = max_m - min_m,
                         mean_m = mean(dist, na.rm = TRUE),
                         std_m = sd(dist, na.rm = TRUE),
                         sum_m = sum(dist, na.rm = TRUE),
                         median_m = median(dist, na.rm = TRUE)))
    } else {
      out_df10 <- dplyr::tibble(
        region = reg_name,
        year = year,
        age_class = "10+",
        min_m = NA,
        max_m = NA,
        range_m = NA,
        mean_m = NA,
        std_m = NA,
        sum_m = NA,
        median_m = NA)
    }

    # update results data frame
    dist_tsf_df <- dplyr::bind_rows(dist_tsf_df, out_df10)

  }
}

# make suitable name

nom <- paste0("08_annual_distance_between_", req_ha, "ha_plus_specific_age_class_metrics.csv")
readr::write_csv(dist_tsf_df, here::here("statistics", nom))
