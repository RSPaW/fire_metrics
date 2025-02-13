library(terra)
library(dplyr)
library(sf)
library(fs)
library(here)
library(janitor)
library(landscapemetrics)
library(readr)
library(cli)





# Make burnt patch stats --------------------------------------------------


# Look for reporting region masks
msk_paths <- fs::dir_ls(here::here("reporting_masks"))
msk_names <- gsub("_msk", "", tools::file_path_sans_ext(basename(msk_paths)))

# Look for annual fire masks
anfm <- fs::dir_ls(here::here("mosaics", "02_annual_fire_masks"))
res <- terra::res(terra::rast(anfm[1]))

## Empty date frame for results
burnt_patches <- dplyr::tibble()
for(i in seq_along(msk_paths)){
  msk <- terra::rast(msk_paths[i])
  reg_name <- msk_names[i]
  cli::cli_alert_info("Working on {reg_name}...hang tight")
  for(j in seq_along(anfm)){
    # anfm for region
    fmsk <- terra::rast(anfm[j]) |>
      terra::crop(msk, mask = TRUE)
    year <- readr::parse_number(basename(anfm[j]))
    cli::cli_alert_info("Doing {year}...")
    mat <- terra::as.matrix(fmsk, wide = TRUE)
    classes <- landscapemetrics:::get_unique_values_int(mat)
    class_patches <- get_class_patches(mat, classes, directions = 8)
    area_patches <- get_area_patches(class_patches, classes, resolution = res)
    out <- dplyr::tibble(region = reg_name,
                         year = year,
                         burnt_ha = sum(area_patches$`1`),
                         unburnt_ha = sum(area_patches$`0`),
                         proportion = burnt_ha/unburnt_ha,
                         num_patches = length(area_patches$`1`),
                         mean_ha = mean(area_patches$`1`),
                         min_ha = min(area_patches$`1`),
                         max_ha = max(area_patches$`1`),
                         med_ha = median(area_patches$`1`),
                         stdev = sd(area_patches$`1`))
    burnt_patches <- dplyr::bind_rows(burnt_patches, out)
  }
}

readr::write_csv(burnt_patches, here::here("statistics", "01_annual_burnt_patch_metrics.csv"))



