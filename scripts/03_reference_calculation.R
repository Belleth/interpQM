# ___________________________________________________________________
# ___________________________________________________________________
# interpQM: Homogenization of snow depth data
# 03_reference_calculation ----
# Author: Gernot Resch
# Date: 26.04.2024
# ___________________________________________________________________
# ___________________________________________________________________

rm(list = ls())

# load packages
library(tidyverse)
library(magrittr)

# load functions
source("scripts/functions.R")

# load config-file
source("homogenization/config.ini")

# load pre-calculated network_builder
load("homogenization/data/02_processed/network_builder.rda")

# load snow depth dataset
load("homogenization/data/02_processed/HS.RData")

# ___________________________________________________________________
# ___________________________________________________________________
# # calculate weighted reference stations ----
# ___________________________________________________________________
# ___________________________________________________________________

# list of all candidate stations with networks
candidate_stations <- network_builder |>
  pull(id_candidate) |>
  unique() |>
  as.character()

# ___________________________________________________________________
# create dataframe of all snowdepth time series in a network ----
# ___________________________________________________________________

i <- 1
for (i in seq_along(candidate_stations)) {
  candidate_station <- candidate_stations[i]

  # get all reference stations for the candidate station
  network <- network_builder |>
    filter(id_candidate == candidate_station)

  # calculate weights for every reference station using the correlation
  if (correlation_weight == "linear") {
    network %<>%
      mutate(weight = round(
        norm_min_max(network$correlation) * 100
      ))
  }

  if (correlation_weight == "exponential") {
    network %<>%
      mutate(
        weight = round(
          norm_min_max(network$correlation) * 10
        )^2
      )
  }

  # ___________________________________________________________________
  # # create reference-dataset ----
  # ___________________________________________________________________

  # filter for stations contained in network
  HS_reference <- HS |>
    # filter for reference stations
    filter(id %in% network$id_reference) |>
    rename(id_reference = id) |>
    # add weights
    left_join(
      select(network, id_reference, weight),
      by = "id_reference"
    )

  # calculate weighted mean for each day
  HS_reference_weighted <- HS_reference |>
    group_by(date) |>
    summarise(
      snow_depth_weighted = weighted.mean(
        snow_depth_orig,
        weight,
        na.rm = T
      )
    )

  # stitch HS_reference-files together for export
  if (i == 1) {
    HS_reference_weighted_export <- HS_reference_weighted |>
      # add id-column
      mutate(id_candidate = candidate_station) |>
      # rename snow_depth_weighted to snow_depth_reference
      rename(snow_depth_reference = snow_depth_weighted)

    # create object for export
    HS_reference_export <- HS_reference_weighted_export
  } else {
    HS_reference_export_to_add <- HS_reference_weighted |>
      # add id-column
      mutate(id_candidate = candidate_station) |>
      # rename snow_depth_weighted to snow_depth_reference
      rename(snow_depth_reference = snow_depth_weighted)

    # bind rows
    HS_reference_export <- bind_rows(
      HS_reference_export,
      HS_reference_export_to_add
    )
  }
}

# rename it so its more clear whats in there and add hydrological year
HS_reference <- HS_reference_export |>
  mutate(
    month = month(date),
    year = year(date),
    hyear = if_else(month <= 9, year - 1, year) |>
      as.integer()
  ) |>
  # get rid of month and year, because they are not needed anymore
  select(-month, -year)

# cleanup
rm(
  HS_reference_weighted_export, HS_reference_export,
  HS_reference_export_to_add, HS_reference_weighted,
  i, candidate_station, network
)

# ___________________________________________________________________
# Load manual reference stations ----
# (in case there are any)
# ___________________________________________________________________

if (file.exists("homogenization/data/01_original/candidate_stations_single.csv")) {
  reference_stations_single <- read_csv(
    "homogenization/data/01_original/candidate_stations_single.csv",
    show_col_types = FALSE
  ) |>
    # make sure its character values
    mutate(
      id_reference = as.character(id_reference),
      id_candidate = as.character(id_candidate)
    )
} else {
  reference_stations_manual <- tibble(
    id_reference = character(),
    id_candidate = character()
  )
}

# add manual reference stations to HS_reference
HS_single <- HS |>
  # filter for manual reference stations
  filter(id %in% reference_stations_single$id_reference) |>
  # rename it so its more clear whats in there
  rename(snow_depth_reference = snow_depth_orig) |>
  # add id-column
  left_join(
    reference_stations_single,
    by = c("id" = "id_reference")
  ) |>
  select(-id)

# combine HS_reference and HS_reference
if (nrow(HS_single) > 0) {
  HS_reference <- bind_rows(
    HS_reference, HS_single
  )
}


# change possibly non-numeric to numeric
HS_reference %<>%
  mutate(
    snow_depth_reference = as.numeric(snow_depth_reference)
  )

# export
HS_reference |>
  save(file = "homogenization/data/02_processed/HS_reference.RData")
