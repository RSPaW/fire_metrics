library(terra)
library(fs)
library(here)
library(tools)
library(readr)
library(cli)



## Distance between all burnt to unburnt patches regardless of size


# Make distance to unburnt cells stats ------------------------------------

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
    # find distance of ALL cells to 0
    gdist <- terra::gridDist(anfm, target = 0) |>
      terra::values()
    # subset positive values only
    no_zero <- gdist[gdist > 0]
    out_df <- dplyr::tibble(region = reg_name,
                            year = year) |>
      dplyr::mutate(min_m = min(no_zero, na.rm = TRUE),
                    max_m = max(no_zero, na.rm = TRUE),
                    range_m = max_m - min_m,
                    mean_m = mean(no_zero, na.rm = TRUE),
                    std_m = sd(no_zero, na.rm = TRUE),
                    sum_m = sum(no_zero, na.rm = TRUE),
                    median_m = median(no_zero, na.rm = TRUE))
    unburnt_df <- dplyr::bind_rows(unburnt_df, out_df)

  }
}

readr::write_csv(unburnt_df, here::here("statistics", "04_annual_distance_to_unburnt_patch_metrics.csv"))

