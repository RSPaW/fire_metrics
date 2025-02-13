library(terra)
library(fs)
library(here)
library(tools)
library(landscapemetrics)
library(readr)
library(cli)

## Distance between all unburnt patches regardless of size using minimum
# euclidean distance i.e. the reported minimum is the minimum distance from edge
# of one patch to it's nearest neighbour and the reported maximum is the maximum
# of the minimums.


# Make distance unburnt to unburnt patch stats -----------------------------

# Look for reporting region masks
msk_paths <- fs::dir_ls(here::here("reporting_masks"))
msk_names <- gsub("_msk", "", tools::file_path_sans_ext(basename(msk_paths)))

# Look for annual fire masks
anfms <- fs::dir_ls(here::here("mosaics", "02_annual_fire_masks"))
years <- readr::parse_number(basename(anfms))

# Empty date frame for results
unburnt_df <- dplyr::tibble()

for(i in seq_along(msk_paths)){
  reg_name <- msk_names[i]
  cli::cli_alert_info("Working on {reg_name}...hang tight")
  msk <- terra::rast(msk_paths[i])
  for(j in seq_along(anfms)){
    anfm <- terra::rast(anfms[j]) |>
      terra::crop(msk, mask = TRUE)
    year <- years[j]
    cli::cli_alert_info("Doing {year}...")
    # make unique IDs for class 0
    class_0 <- landscapemetrics::get_patches(anfm, class = 0)
    suppressMessages(out_df <- landscapemetrics::get_nearestneighbour(class_0[[1]][[1]]) |>
      dplyr::mutate(region = reg_name,
                    year = year) |>
      dplyr::select(-layer) |>
      dplyr::group_by(region, year) |>
      dplyr::summarise(min_m = min(dist, na.rm = TRUE),
                       max_m = max(dist, na.rm = TRUE),
                       range_m = max_m - min_m,
                       mean_m = mean(dist, na.rm = TRUE),
                       std_m = sd(dist, na.rm = TRUE),
                       sum_m = sum(dist, na.rm = TRUE),
                       median_m = median(dist, na.rm = TRUE)))
    unburnt_df <- dplyr::bind_rows(unburnt_df, out_df)

  }
}

readr::write_csv(unburnt_df, here::here("statistics", "04a_annual_distance_between_unburnt_patch_metrics.csv"))


