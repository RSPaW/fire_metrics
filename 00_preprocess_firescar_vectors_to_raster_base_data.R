# Required libraries ------------------------------------------------------
# library(tidyverse)
library(terra)
library(tidyterra)
library(dplyr)
library(sf)
library(glue)
library(fs)
library(here)
library(dismo)
library(readr)
library(cli)




# Required functions ------------------------------------------------------

## Puts a users AOI shape file into a list and projects to EPSG:3577
user_aoi <- function(aoi_path, name){
  shp <- sf::st_read(dsn = aoi_path, quiet = TRUE) |>
    sf::st_make_valid() |>
    sf::st_transform(3577)
  aoi_list <- list(aoi = shp,
                   aoi_name = name)
  return(aoi_list)
}

## Performs checks on AOI shape file
# - makes topology valid
# - ensures EPSG:3577
# - checks if AOI spatial extent is too big to process (avoids maxing memory)
# - if AOI too large then splits into even portions to "chunk" inputs, also
#   writes out "chunk" shape files and retains file paths.
aoi_check <- function(aoi, split = FALSE){
  aoi_alb <- aoi[["aoi"]] |> sf::st_make_valid() |> sf::st_transform(3577) |>
    dplyr::summarise()
  qa <- sum(units::set_units(sf::st_area(aoi_alb), value = km^2))
  is_big <- as.vector(floor(qa / units::set_units(26000, km^2)))
  if(is_big > 1 & isTRUE(split)){
    cli::cli_alert_warning("AOI is too large to process. Splitting into {is_big} chunks")
    wk_aoi <- split_big_aoi(aoi = aoi_alb, n = is_big)
    vector_names <- vector(mode = "character", length = is_big)
    for(i in 1:dim(wk_aoi)[1]){
      name <- aoi[["aoi_name"]]
      vect_fold <- fs::dir_create(here::here(), "vectors")
      v_name <- here::here(vect_fold, glue::glue("{name}{i}.shp"))
      suppressWarnings({
        v <- wk_aoi[i, ] |>
          sf::st_write(dsn = v_name, append = FALSE, quiet = TRUE)
      })
      vector_names[i] <- v_name
    }
    aoi$split <- vector_names
    return(aoi)
  } else if(is_big > 1 & isFALSE(split)){
    cli::cli_alert_warning("AOI is too large to process as is.")
    cli::cli_alert_info("You have two options.")
    cli::cli_bullets(c(
      "*" = "Reduce your AOI to < 26,000 km2",
      "*" = "Use split=TRUE to split your AOI into smaller shape files"
    ))
    return(aoi)
  } else {
    cli::cli_alert_success("AOI is suitable for processing" )
    return(aoi)
  }

}

## Function that splits AOI into smaller "chunks". Is used in above function
split_big_aoi <- function(aoi, n){
  suppressWarnings({
    poly <- aoi
    # create random points
    points_rnd <- sf::st_sample(poly, size = 10000)
    #k-means clustering
    points <- do.call(rbind, sf::st_geometry(points_rnd)) |>
      dplyr::as_tibble() |> setNames(c("x","y"))
    points <- do.call(rbind, sf::st_geometry(points_rnd))
    k_means <- kmeans(points, centers = n)
    # create voronoi polygons
    voronoi_polys <- dismo::voronoi(k_means$centers, ext = poly)
    # clip to poly
    crs(voronoi_polys) <- crs(poly)
    voronoi_sf <- sf::st_as_sf(voronoi_polys)
    equal_areas <- sf::st_intersection(voronoi_sf, poly)
    return(equal_areas)
  })

}





# User inputs -------------------------------------------------------------

# USER - input directory for all annual fire scars
scar_path <- "X:/DATA/FireScarMapping/Pilbara/Fire_mapping_Master/EDITED_CLEAN"

# USER - confirm "year" attribute column name common to all shape files
attribute_year <- "Map_YR"

# USER - provide full file path to a shape file of reporting regions (1 or many regions)
aoi_path <- "Z:/DEC/Pilbara_Firescar_Analysis_2018-074/2024/DATA/Metrics/FMAs/FMAs_v3.shp"

# USER - give project name (only used in naming "tiles")
pname <- "pilbara"


# work --------------------------------------------------------------------

# AOI in and split
aoi <- user_aoi(aoi_path = aoi_path, name = pname)
aoi <- aoi_check(aoi = aoi, split = TRUE)

## Make fire scar data
# Make file paths to all fire scar shape files
fs_path <- scar_path |>  fs::dir_ls(glob = "*.shp")

# Read in fire scar shape files into a list
scar_list <- lapply(fs_path, sf::st_read)

# Combine the list of sf objects into a single object and crop to aoi
full_fire_dat <- do.call(what = sf:::rbind.sf, args = scar_list) |>
  sf::st_transform(3577) |>
  dplyr::mutate(year = as.numeric(!!sym(attribute_year))) |>
  terra::vect() |>
  terra::crop(terra::vect(aoi$aoi)) |> # crops to extent
  dplyr::arrange(year) # ensure ordered by year

# Get unique years on full data set - important as it can be possible that no fires
# occur in one of the "chunks" and a dummy year may need to be inserted to maintain
# proper sequence in time series critical metrics i.e. time since fire
unique_fyrs <- full_fire_dat |>
  tidyterra::pull(var = year) |>
  unique()

for(poly in aoi[["split"]]){
  # setup folder structure --------------------------------------------------
  aoi_n <- fs::path_ext_remove(fs::path_file(poly))
  cli::cli_alert_info("Working on {aoi_n}...hang tight")
  cli::cli_alert_info("Creating Folder Structure")

  # set up folder names
  anf_fol <-here::here(aoi_n, "01_annual_fires")
  anfm_fol <- here::here(aoi_n, "02_annual_fire_masks")
  yob_fol <- here::here(aoi_n, "03_year_of_burn")
  tsf_fol <- here::here(aoi_n, "04_time_since_fire")
  fols <- c(anf_fol, anfm_fol, yob_fol, tsf_fol)
  names(fols) <- c("anf_fol", "anfm_fol", "yob_fol", "tsf_fol")
  fs::dir_create(fols)


  dat <- list(aoi_chunk = sf::st_read(poly), aoi_name = aoi_n)

  # individual burn years ---------------------------------------------------
  cli::cli_alert_info("Annual Burn Year Rasters and Masks")

  # initial data subset to full extent - requires further cropping to chunk
  f_vecs <- full_fire_dat |>
    terra::crop(terra::vect(dat$aoi_chunk)) |>
    dplyr::arrange(year)

  # unique fire years
  u_fyrs <- f_vecs |>
    tidyterra::pull(var = year) |>
    unique()

  template <- terra::rast(dat[["aoi_chunk"]], res = 30)
  aoi_chunk_msk <- dat[["aoi_chunk"]] |>
    terra::vect() |>
    terra::rasterize(, y = template)

  cli::cli_progress_bar("Creating rasters", total = length(u_fyrs))
  for(i in seq_along(u_fyrs)){
    # annual fires
    anf <- f_vecs |>
      tidyterra::filter(year == u_fyrs[i]) |>
      terra::rasterize(template, field = "year") |>
      terra::crop(aoi_chunk_msk, mask = TRUE)
    names(anf) <- u_fyrs[i]
    nom1 <- paste0("anf",  u_fyrs[i], ".tif")
    anf_nom <- fs::path(anf_fol, nom1)
    terra::writeRaster(anf, anf_nom)
    # annual fire masks
    anfm <- terra::classify(anf, cbind(u_fyrs[i], 1))
    anfm[is.na(anfm)] <- 0
    nom2 <- paste0("anfm",  u_fyrs[i], ".tif")
    anfm_nom <- fs::path(anfm_fol, nom2)
    terra::writeRaster(anfm, anfm_nom)
    cli::cli_progress_update()
  }

  # find non burn years and add an infill blank year
  zero_rst <- template
  terra::values(zero_rst) <- NaN # not 0
  zero_yr <- zero_rst |>
    terra::crop(aoi_chunk_msk, mask = TRUE)
  template0 <- template
  template0[] <- 0

  # full aoi years to get padding correct for for chunks
  minyr <- min(unique_fyrs)
  maxyr <- max(unique_fyrs)
  full_fyrs <- minyr:maxyr
  missing <- full_fyrs[!(full_fyrs %in% u_fyrs)]
  cli::cli_progress_bar("Creating infil rasters", total = length(missing))
  for(j in seq_along(missing)){
    nom1 <- glue::glue("anf",  missing[j], ".tif")
    my_nom1 <- fs::path(anf_fol, nom1)
    terra::writeRaster(zero_yr, my_nom1)
    nom2 <- glue::glue("anfm",  missing[j], ".tif")
    my_nom2 <- fs::path(anfm_fol, nom2)
    terra::writeRaster(template0, my_nom2)
    cli::cli_progress_update()
  }

  # yob rasters

  cli::cli_alert_info("Year of Burn")
  yob_noms <- fs::path(yob_fol, gsub("anf", "yob", dir(anf_fol)))

  # start year same as first burn year
  fs::file_copy(path = fs::dir_ls(anf_fol)[1], new_path = yob_noms[1])
  # 2nd yob
  rst1 <- terra::rast(fs::dir_ls(anf_fol)[1])
  rst2 <- terra::rast(fs::dir_ls(anf_fol)[2])
  by_stk <- c(rst1, rst2)
  yob <- terra::app(by_stk, fun ="max", na.rm = TRUE)
  yob_nom <- yob_noms[2]
  terra::writeRaster(yob, yob_nom)

  # subsequent yob
  yob_iter <- length(full_fyrs)-1
  for(i in 2:yob_iter){
    if(i < yob_iter){
      rst1 <- terra::rast(yob_noms[i])
      rst2 <- terra::rast(fs::dir_ls(anf_fol)[i+1])
      by_stk <- c(rst1, rst2)
      yob <- terra::app(by_stk, fun ="max", na.rm = TRUE)
      terra::writeRaster(yob, yob_noms[i+1])
    } else {
      rst1 <- terra::rast(yob_noms[i])
      rst2 <- terra::rast(fs::dir_ls(anf_fol)[i+1])
      by_stk <- c(rst1, rst2)
      yob <- terra::app(by_stk, fun ="max", na.rm = TRUE)
      yob[yob == 0] <- NA
      terra::writeRaster(yob, yob_noms[i+1])
    }

  }

  # year since last burn ----------------------------------------------------
  cli::cli_alert_info("Time Since Fire")
  tsf_noms <- fs::path(tsf_fol, gsub("yob", "tsf", dir(yob_fol)))
  cli::cli_progress_bar("Creating rasters", total = length(tsf_noms))

  for(i in seq_along(tsf_noms)){
    yob <- terra::rast(fs::dir_ls(yob_fol)[i])
    tsf <- full_fyrs[i] - yob
    terra::writeRaster(tsf, tsf_noms[i])
    cli::cli_progress_update()
  }
}


# Mosaic all "tiles" back together ----------------------------------------


cli::cli_alert_info("Creating Mosaic Folder Structure")

# set up folder names
anf_fol_mos <- here::here("mosaics", "01_annual_fires")
anfm_fol_mos <- here::here("mosaics", "02_annual_fire_masks")
yob_fol_mos <- here::here("mosaics", "03_year_of_burn")
tsf_fol_mos <- here::here("mosaics", "04_time_since_fire")
fols <- c(anf_fol_mos, anfm_fol_mos, yob_fol_mos, tsf_fol_mos)
names(fols) <- c("anf_fol_mos", "anfm_fol_mos", "yob_fol_mos", "tsf_fol_mos")
mos_prefix <- c("anf", "anfm", "yob", "tsf")
fs::dir_create(fols)

for(i in seq_along(mos_prefix)){
  reg <- paste0(mos_prefix[i], "[0-9]{4}.tif")
  all_dat <- fs::dir_ls(here::here(), regexp = reg, recurse = TRUE)
  for(j in seq_along(full_fyrs)){
    mos_files <- stringr::str_subset(all_dat, paste0(mos_prefix[i], full_fyrs[j], ".tif"))
    to_mos <- lapply(mos_files, terra::rast)
    collection <- terra::sprc(to_mos)
    mos <- terra::mosaic(collection, fun = "min")
    mos_nom <- paste0(mos_prefix[i], "_mos", full_fyrs[j], ".tif")
    mos_path <- file.path(fols[i], mos_nom)
    terra::writeRaster(mos, mos_path)
  }
}

