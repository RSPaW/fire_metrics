library(terra)
library(fs)
library(here)
library(tools)
library(landscapemetrics)
library(readr)
library(cli)

tictoc::tic("total")

## Distance between USER specified age class and sized patches using Euclidean
## Nearest Neighbour (ENN) i.e. the reported minimum is the minimum distance
## from edge of one patch to it's nearest neighbour and the reported maximum is
## the maximum of those minimums.


## NOTE running this on the current 4 FMA regions covering the Pilbara took
## approximately 13.5 hours.


# Find ENN for specified age and size patches -----------------------------

# USER age class i.e. >= to this number
age <- 6

# USER size class i.e. >= to this size in hectares
size <- 20

# # Look for reporting region masks
msk_paths <- fs::dir_ls(here::here("reporting_masks"))
msk_names <- gsub("_msk", "", tools::file_path_sans_ext(basename(msk_paths)))

# # Look for tsf data
tsfs <- fs::dir_ls(here::here("mosaics", "04_time_since_fire"))
years <- readr::parse_number(basename(tsfs))

# make age reclass matrix
age_vec <- c(-Inf, (age - 1), 0,
             (age - 1), Inf, 1)
age_mat <- matrix(age_vec, ncol=3, byrow = TRUE)

## Empty date frame for results
enn_tsf_df <- dplyr::tibble()

for(i in seq_along(tsfs)){
  year <- years[i]
  tictoc::tic(year)
  cli::cli_alert_info("Working on {year}...hang tight")
  # reclassify tsf to correct age class only
  tsf <- terra::rast(tsfs[i]) |>
    terra::classify(age_mat)
  # move to following year if no patches that age
  # NOTE year will not appear in csv
  if(terra::minmax(tsf)[2] == 0){
    cli::cli_alert_info("No patches that age yet...moving on")
    next
  }
  # make unique IDs aged patches
  tsf_p <- landscapemetrics::get_patches(tsf, class = 1)[[1]][[1]]
  # find area of all aged patches
  tsf_a <-  terra::cellSize(tsf_p, unit="ha") |>
    terra::zonal(tsf_p, sum)
  # find IDs of >= required size of patch
  include_id <- tsf_a$lyr.1[which(tsf_a$area >= size)]
  # move to following year if no patches match size criteria
  # NOTE year will not appear in csv
  if(length(include_id) == 0){
    cli::cli_alert_info("No aged patches that size...moving on")
    next
  }
  # make unique IDs of correct size all 1 and any others 0
  vals_to <- rep(1, length(include_id))
  tsf_out <- terra::subst(tsf_p, from = include_id, to = vals_to, others = 0)
  # now make unique IDs for all correct age and size patches
  classed <- landscapemetrics::get_patches(tsf_out, class = 1)
  # find ENN to patches for all data
  enn <- landscapemetrics::get_nearestneighbour(classed[[1]][[1]])
  for(j in seq_along(msk_paths)){
    reg_name <- msk_names[j]
    cli::cli_alert_info("Working on {reg_name} stats...")
    msk <- terra::rast(msk_paths[j])
    # find IDs that are in current reporting region
    reg_ind <- classed[[1]][[1]] |>
      terra::crop(msk, mask = TRUE) |>
      terra::values() |>
      unique() |>
      as.numeric()
    # remove NAs
    reg_ind <- reg_ind[!is.na(reg_ind)]
    # double check size requirement move to next reporting region if no patches match size criteria
    # NOTE reporting region and year will not appear in csv
    if(length(reg_ind) == 0){
      cli::cli_alert_info("No aged patches that size for {reg_name}...moving on")
      next
    }
    # filter all patch data to reporting region and make sats
    out_df <- suppressMessages(enn |>
      dplyr::filter(id %in% reg_ind) |>
      dplyr::mutate(region = reg_name,
                    year = year,
                    age_class = age) |>
      dplyr::group_by(region, year, age_class) |>
      dplyr::summarise(min_m = min(dist, na.rm = TRUE),
                       max_m = max(dist, na.rm = TRUE),
                       range_m = max_m - min_m,
                       mean_m = mean(dist, na.rm = TRUE),
                       std_m = sd(dist, na.rm = TRUE),
                       sum_m = sum(dist, na.rm = TRUE),
                       median_m = median(dist, na.rm = TRUE)))

    enn_tsf_df <- dplyr::bind_rows(enn_tsf_df, out_df)
  }
  tictoc::toc(log = TRUE)
}
# make suitable name
nom <- paste0("08_annual_euclidean_distance_between_age_class_",age, "plus_of_",size,"ha_plus_patch_metrics.csv")
readr::write_csv(enn_tsf_df, here::here("statistics", nom))

tictoc::toc(log = TRUE)
