# ___________________________________________________________________
# ___________________________________________________________________
# interpQM homogenization of snow depth data
# 02_network_creation ----
# already prepared networks are put together
# Author: Gernot Resch
# Date: 29.02.2024
# ___________________________________________________________________
# ___________________________________________________________________

rm(list = ls())

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

# list of all candidate stations with already networks
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

  # ___________________________________________________________________
  # calculate weighted mean ----
  # ___________________________________________________________________

  # calculate weights for every reference station using the correlation
  weighted_mean <- if (correlation_weight == "linear") {
    round(
      norm_min_max(network$correlation) * 100
    )
  } else if (correlation_weight == "exponential") {
    round(
      norm_min_max(network$correlation) * 10
    )^2
  }

  # combine id_candidate and corresponding weights
  weight <- tibble(
    id_candidate = network$id_reference,
    weight = weighted_mean
  )

  # create reference-dataset
  HS_reference <- HS |>
    # filter for reference stations
    filter(id %in% network$id_reference) |>
    # add weights
    left_join(
      weight,
      by = c("id" = "id_candidate")
    )

  # calculate weighted mean for each day
  HS_reference_weighted <- HS_reference |>
    group_by(date) |>
    summarise(
      snow_depth_weighted = weighted.mean(
        snow_depth_orig,
        w = weight,
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

# rename it so its more clear whats in there
HS_reference <- HS_reference_export |>
  as_tibble()

# cleanup
rm(
  HS_reference_weighted_export, HS_reference_export,
  HS_reference_export_to_add, HS_reference_weighted,
  i, candidate_station, network, weighted_mean, weight
)

# ___________________________________________________________________
# Load manual reference stations ----
# (in case there are any)
# ___________________________________________________________________

if (file.exists("homogenization/data/01_original/reference_stations_manual.csv")) {
  reference_stations_manual <- read_csv(
    "homogenization/data/01_original/reference_stations_manual.csv",
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
HS_manual <- HS |>
  filter(id %in% reference_stations_manual$id_reference) |>
  rename(snow_depth_reference = snow_depth_orig) |>
  left_join(
    reference_stations_manual,
    by = c("id" = "id_reference")
  ) |>
  select(-id)

# combine HS_reference and HS_reference
HS_reference <- bind_rows(
  HS_reference, HS_manual
)

# change possibly non-integer to integer
HS_reference %<>%
  mutate(
    snow_depth_reference = as.integer(snow_depth_reference)
  )

# export
HS_reference |>
  save(file = "homogenization/data/02_processed/HS_reference.RData")
