library(terra)
library(fs)
library(here)
library(tools)
library(readr)
library(cli)





# Make fire frequency stats -----------------------------------------------

# Look for reporting region masks
msk_paths <- fs::dir_ls(here::here("reporting_masks"))
msk_names <- gsub("_msk", "", tools::file_path_sans_ext(basename(msk_paths)))

# Look for annual fire masks
anfm <- fs::dir_ls(here::here("mosaics", "02_annual_fire_masks"))
years <- readr::parse_number(basename(anfm))

# Confirm resolution for hectare multiplier
res <- terra::res(terra::rast(anfm[1]))
ha_multiplier <- (res[1]*res[2])/10000

# Empty date frame for results
fire_freq_df <- dplyr::tibble()

# Iterations less one as 1st year always a special case
todo <- length(anfm) - 1

for(i in seq_along(msk_paths)){
  reg_name <- msk_names[i]
  cli::cli_alert_info("Working on {reg_name}...hang tight")
  msk <- terra::rast(msk_paths[i])
  # 1st year special case
  an1 <- terra::rast(anfm[i]) |>
    terra::crop(msk, mask = TRUE)
  year <- years[1]
  cli::cli_alert_info("Doing {year}...")
  out_df <- terra::freq(an1) |>
    dplyr::as_tibble() |>
    dplyr::mutate(region = reg_name,
                  year = year,
                  area_ha = count * ha_multiplier) |>
    dplyr::rename(freq = value) |>
    dplyr::select(region, year, freq, count, area_ha)
  fire_freq_df <- dplyr::bind_rows(fire_freq_df, out_df)
  # iterate over rest of annual fire masks
  for(j in 1:todo){
    first <- 1
    last <- j+1
    anfms <- terra::rast(anfm[first:last]) |>
      terra::crop(msk, mask = TRUE) |>
      terra::app(fun = "sum")
    year <- years[j+1]
    cli::cli_alert_info("Doing {year}...")
    out_df <- terra::freq(anfms) |>
      dplyr::as_tibble() |>
      dplyr::mutate(region = reg_name,
                    year = year,
                    area_ha = count * ha_multiplier) |>
      dplyr::rename(freq = value) |>
      dplyr::select(region, year, freq, count, area_ha)
    fire_freq_df <- dplyr::bind_rows(fire_freq_df, out_df)
  }
}

readr::write_csv(fire_freq_df, here::here("statistics", "02_annual_fire_frequency_metrics.csv"))

























