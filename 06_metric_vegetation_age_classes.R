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

# Confirm resolution for hectare multiplier
res <- terra::res(terra::rast(tsfs[1]))
ha_multiplier <- (res[1]*res[2])/10000

# # Empty date frame for results
tsf_df <- dplyr::tibble()

for(i in seq_along(tsfs)){
  # grab annual tsf
  tsf <- terra::rast(tsfs[i])
  year <- years[i]
  cli::cli_alert_info("Working on {year}...hang tight")
  # reclassify into age categories
  age <- c(-Inf, 1, 1,
           1, 4, 2,
           4, 7, 3,
           7, 10, 4,
           10, 15, 5,
           15, 20, 6,
           20, 30, 7,
           30, Inf, 8)
  age_rcl <- matrix(age, ncol=3, byrow=TRUE)
  tsf_class <- terra::classify(tsf, age_rcl)
  for(j in seq_along(msk_paths)){
    # cycle over regions
    reg_name <- msk_names[j]
    msk <- terra::rast(msk_paths[j])
    tsf_reg <- tsf_class |>
        terra::crop(msk, mask = TRUE)
    area_patches <- lsm_p_area(tsf_reg) |>
      dplyr::mutate(region = reg_name,
                    year = year) |>
      dplyr::rename(age_class = class,
                    patch_id = id,
                    area_ha = value) |>
      dplyr::mutate(age_class = dplyr::case_when(
        age_class == 1 ~ "0-1",
        age_class == 2 ~ "2-4",
        age_class == 3 ~ "5-7",
        age_class == 4 ~ "8-10",
        age_class == 5 ~ "11-15",
        age_class == 6 ~ "16-20",
        age_class == 7 ~ "21-30",
        age_class == 8 ~ "30+",
        TRUE ~ "NA"
      )) |>
      dplyr::mutate(size_class = dplyr::case_when(
        area_ha > 0 & area_ha <= 20 ~ "0-20",
        area_ha > 20 & area_ha <= 50 ~ "21-50",
        area_ha > 50 & area_ha <= 1000 ~ "51-1000",
        area_ha > 1000 & area_ha <= 20000 ~ "1001-20000",
        area_ha > 20000 ~ "20000+"
      )) |>
      dplyr::select(region, year, age_class, patch_id, area_ha, size_class)

    tsf_df <- dplyr::bind_rows(tsf_df, area_patches)
  }
}

readr::write_csv(tsf_df, here::here("statistics", "annual_patch_age_class_metrics.csv"))



# Example code to extract summary stats -----------------------------------

# read in data
tsf_df <- readr::read_csv(here::here("statistics", "06_annual_patch_age_and_size_class_metrics.csv"))

# number and area of age class by region and year
tsf_df |>
  dplyr::group_by(region, year, age_class) |>
  dplyr::summarise(number = dplyr::n(),
                   area = sum(area_ha))

# number and area of age_class by size class, by region and year
tsf_df |>
  dplyr::group_by(region, year, age_class, size_class) |>
  dplyr::summarise(number = dplyr::n(),
                   area = sum(area_ha))

