library(terra)
library(sf)
library(here)
library(fs)
library(dplyr)
library(cli)

# User inputs -------------------------------------------------------------

# USER - input file path to reporting shape file
rep_shp_path <- "Z:/DEC/Pilbara_Firescar_Analysis_2018-074/2024/DATA/Metrics/FMAs/FMAs_v3.shp"

# USER - input file path to an already created mosaic which will be used as a template
# for rasterisation
mos_path <- "../R_Development/mosaics/04_time_since_fire/tsf_mos1987.tif"


# Make raster masks for reporting regions ---------------------------------

mos_template <- terra::rast(mos_path)

rep_shp <- sf::st_read(rep_shp_path) |>
  sf::st_transform(3577)

# Make mask folder
cli::cli_alert_info("Creating Mask Folder")
fol_msk <- here::here("reporting_masks")
fs::dir_create(fol_msk)

# Make stats output folder (used for metrics later)
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
  nom <- reg[,'FMA_name'] |> # hard coded attribute name - might need to change if necessary
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
