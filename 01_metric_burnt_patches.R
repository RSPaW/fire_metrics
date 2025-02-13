library(terra)
library(tidyterra)
library(dplyr)
library(sf)
library(fs)
library(here)
library(janitor)
library(landscapemetrics)
library(readr)
library(cli)

# User inputs -------------------------------------------------------------

# USER - input file path to reporting shape file
rep_shp_path <- "Z:/DEC/Pilbara_Firescar_Analysis_2018-074/2024/DATA/Metrics/FMAs/FMAs_v3.shp"

# USER - input file path to an already created mosaic which will be used as a template
# for rasterisation
mos_path <- "mosaics/04_time_since_fire/tsf_mos1987.tif"


# Make raster masks for reporting regions ---------------------------------

mos_template <- terra::rast(mos_path)

rep_shp <- sf::st_read(rep_shp_path) |>
  sf::st_transform(3577)

# Make mask folder
cli::cli_alert_info("Creating Mask Folder")
fol_msk <- here::here("reporting_masks")
fs::dir_create(fol_msk)

# Make stats output folder
cli::cli_alert_info("Creating Stats Folder")
fol_stats <- here::here("statistics")
fs::dir_create(fol_stats)

# Make reporting masks
cli::cli_alert_info("Creating Individual Reporting Masks")
regs <- dim(rep_shp)[1]
msk_paths <- vector(mode = "character", length = regs)
msk_names <- vector(mode = "character", length = regs)
for(i in 1:regs){
  reg <- rep_shp[i, ]
  nom <- reg[,'FMA_name'] |>
    sf::st_drop_geometry() |>
    dplyr::pull() |>
    janitor::make_clean_names()
  full_nom <- here::here("reporting_masks", paste0(nom, "_msk.tif"))
  msk <- reg |>
    terra::rasterize(mos_template) |>
    terra::writeRaster(filename = full_nom)
  msk_names[i] <- nom
  msk_paths[i] <- full_nom
}



# Make burnt patch stats --------------------------------------------------

# Make output data frame
df <- dplyr::tibble(region = "",
                    year = 0,
                    burnt_ha = 0,
                    unburnt_ha = 0,
                    proportion = 0,
                    num_patches = 0,
                    avg_ha = 0,
                    min_ha = 0,
                    max_ha = 0,
                    med_ha = 0,
                    stdev = 0)
burnt_patches <- df[0,]

anfm <- fs::dir_ls(here::here("mosaics", "02_annual_fire_masks"))
# anfm_lgth <- length(anfm)
res <- terra::res(mos_template)
for(i in 1:regs){
  msk <- terra::rast(msk_paths[i])
  reg_name <- msk_names[i]
  cli::cli_alert_info("Working on {reg_name}...hang tight")
  for(j in seq_along(anfm)){
    # anfm for region
    fmsk <- terra::rast(anfm[j]) |>
      terra::crop(msk, mask = TRUE)
    mat <- terra::as.matrix(fmsk, wide = TRUE)
    classes <- landscapemetrics:::get_unique_values_int(mat)
    class_patches <- get_class_patches(mat, classes, directions = 8)
    area_patches <- get_area_patches(class_patches, classes, resolution = res)
    out <- dplyr::tibble(region = reg_name,
                         year = readr::parse_number(basename(anfm[j])),
                         burnt_ha = sum(area_patches$`1`),
                         unburnt_ha = sum(area_patches$`0`),
                         proportion = burnt_ha/unburnt_ha,
                         num_patches = length(area_patches$`1`),
                         avg_ha = mean(area_patches$`1`),
                         min_ha = min(area_patches$`1`),
                         max_ha = max(area_patches$`1`),
                         med_ha = median(area_patches$`1`),
                         stdev = sd(area_patches$`1`))
    burnt_patches <- dplyr::bind_rows(burnt_patches, out)
  }
}

readr::write_csv(burnt_patches, here::here("statistics", "01_annual_burnt_patch_metrics.csv"))



