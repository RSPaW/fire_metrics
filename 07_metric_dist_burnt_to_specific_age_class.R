library(terra)
library(fs)
library(here)
library(tools)
library(landscapemetrics)
library(readr)
library(cli)




# Vegetation age classes for all patches ----------------------------------

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
    year <- years[j]
    cli::cli_alert_info("Doing {year}...")
   tsfm <- terra::rast(tsfs[j]) |>
      terra::crop(msk, mask = TRUE)
   tsfm6 <- terra::classify(tsfm, rcl6)
   tsfm10 <- terra::classify(tsfm, rcl10)
   # find distance of ALL cells to age class 6+
   gdist6 <- terra::gridDist(tsfm6, target = 1) |>
     terra::values()
   out_df6 <- dplyr::tibble(region = reg_name,
                           year = year,
                           age_class = "6+") |>
     dplyr::mutate(min_m = min(gdist6, na.rm = TRUE),
                   max_m = max(gdist6, na.rm = TRUE),
                   range_m = max_m - min_m,
                   mean_m = mean(gdist6, na.rm = TRUE),
                   std_m = sd(gdist6, na.rm = TRUE),
                   sum_m = sum(gdist6, na.rm = TRUE),
                   median_m = median(gdist6, na.rm = TRUE))
   # find distance of ALL cells to age class 10+
   gdist10 <- terra::gridDist(tsfm10, target = 1) |>
     terra::values()
   out_df10 <- dplyr::tibble(region = reg_name,
                            year = year,
                            age_class = "10+") |>
     dplyr::mutate(min_m = min(gdist10, na.rm = TRUE),
                   max_m = max(gdist10, na.rm = TRUE),
                   range_m = max_m - min_m,
                   mean_m = mean(gdist10, na.rm = TRUE),
                   std_m = sd(gdist10, na.rm = TRUE),
                   sum_m = sum(gdist10, na.rm = TRUE),
                   median_m = median(gdist10, na.rm = TRUE))
   out_df <- dplyr::bind_rows(out_df6, out_df10)

   dist_tsf_df <- dplyr::bind_rows(dist_tsf_df, out_df)

  }

}

readr::write_csv(dist_tsf_df, here::here("statistics", "07_annual_distance_to_specific_age_classes_metrics.csv"))
