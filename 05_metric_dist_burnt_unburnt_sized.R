library(terra)
library(fs)
library(here)
library(tools)
library(landscapemetrics)
library(readr)
library(cli)





# Make distance to unburnt cells based on patch size ----------------------

# USER - input unburnt patch size in hectares (calculates for patches larger than this number)
req_ha <- 20

# # Look for reporting region masks
msk_paths <- fs::dir_ls(here::here("reporting_masks"))
msk_names <- gsub("_msk", "", tools::file_path_sans_ext(basename(msk_paths)))

# # Look for annual fire masks
anfms <- fs::dir_ls(here::here("mosaics", "02_annual_fire_masks"))
years <- readr::parse_number(basename(anfms))

# # Empty date frame for results
unburnt_dist_df <- dplyr::tibble()

for(i in seq_along(msk_paths)){
  reg_name <- msk_names[i]
  cli::cli_alert_info("Working on {reg_name}...hang tight")
  msk <- terra::rast(msk_paths[i])
  for(j in seq_along(anfms)){
    anfm <- terra::rast(anfms[j]) |>
      terra::crop(msk, mask = TRUE)
    year <- years[j]
    cli::cli_alert_info("Doing {year}...")
    # get all unburnt patches of class 0
    anfm_zp <- landscapemetrics::get_patches(anfm, class = 0)[[1]][[1]]
    # find area of zero patches
    area_zp <- terra::cellSize(anfm_zp, unit="ha") |>
      terra::zonal(anfm_zp, sum)
    # find IDs of patches smaller than req_ha
    exclude_id <- area_zp$lyr.1[which(area_zp$area < req_ha)]
    # create unburnt raster with only right sized patches
    zp_sized <- anfm_zp
    # need to catch instances with no unburnt of size
    if(length(exclude_id) != 0){
      zp_sized[anfm_zp %in% exclude_id] <- NA
    }
    # reclass IDs of large zero patches to class 5
    m_zero <- c(0, Inf, 5)
    rcl_zero <- matrix(m_zero, ncol=3, byrow=TRUE)
    rst_5 <- terra::classify(zp_sized, rcl_zero)
    # add back in the fire mask of 1's
    rst_51 <- terra::cover(rst_5, anfm)
    # reclass small zero patches to NA
    m_na <- c(-Inf, 0, NA)
    rcl_na <- matrix(m_na, ncol=3, byrow=TRUE)
    rst_51na <- terra::classify(rst_51, rcl_na)
    # now find dist to sized unburnt (class 5)
    dist <- terra::gridDist(rst_51na, target = 5)  |>
      terra::values()
    out_df <- dplyr::tibble(region = reg_name,
                            year = year) |>
      dplyr::mutate(min_m = min(dist, na.rm = TRUE),
                    max_m = max(dist, na.rm = TRUE),
                    range_m = max_m - min_m,
                    mean_m = mean(dist, na.rm = TRUE),
                    std_m = sd(dist, na.rm = TRUE),
                    sum_m = sum(dist, na.rm = TRUE),
                    median_m = median(dist, na.rm = TRUE))


    unburnt_dist_df <- dplyr::bind_rows(unburnt_dist_df, out_df)

  }
}

# make suitable name

nom <- paste0("05_annual_distance_to_", req_ha, "ha_plus_unburnt_patch_metrics.csv")
readr::write_csv(unburnt_dist_df, here::here("statistics", nom))


