library(terra)
library(fs)
library(here)
library(tools)
library(readr)
library(cli)


# Make YSLB stats ---------------------------------------------------------

# years since last burn is synonymous with time since fire

# Look for reporting region masks
msk_paths <- fs::dir_ls(here::here("reporting_masks"))
msk_names <- gsub("_msk", "", tools::file_path_sans_ext(basename(msk_paths)))

# Look for annual tsf data
tsfs <- fs::dir_ls(here::here("mosaics", "04_time_since_fire"))
years <- readr::parse_number(basename(tsfs))

# Confirm resolution for hectare multiplier
res <- terra::res(terra::rast(tsfs[1]))
ha_multiplier <- (res[1]*res[2])/10000

# Empty date frame for results
tsf_df <- dplyr::tibble()

for(i in seq_along(msk_paths)){
  reg_name <- msk_names[i]
  cli::cli_alert_info("Working on {reg_name}...hang tight")
  msk <- terra::rast(msk_paths[i])
  for(j in seq_along(tsfs)){
    tsf <- terra::rast(tsfs[j]) |>
      terra::crop(msk, mask = TRUE)
    year <- years[j]
    cli::cli_alert_info("Doing {year}...")
    out_df <- terra::freq(tsf) |>
      dplyr::as_tibble() |>
      dplyr::mutate(region = reg_name,
                    year = year,
                    area_ha = count * ha_multiplier)|>
      dplyr::rename(tsf = value) |>
      dplyr::select(region, year, tsf, count, area_ha)
    tsf_df <- dplyr::bind_rows(tsf_df, out_df)

  }
}

readr::write_csv(tsf_df, here::here("statistics", "03_annual_time_since_fire_metrics.csv"))
