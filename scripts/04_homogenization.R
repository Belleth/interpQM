# ___________________________________________________________________
# ___________________________________________________________________
# interpQM homogenization of snow depth data
# Homogenization
# Author: Gernot Resch
# Date: 13.03.2024
# ___________________________________________________________________
# ___________________________________________________________________

rm(list = ls())

library(tidyverse)
library(magrittr)

source("scripts/functions.R")

# ___________________________________________________________________
# load data and configuration files ----
# ___________________________________________________________________

# load reference time series
load("homogenization/data/02_processed/HS_reference.RData")

# load time series
load("homogenization/data/02_processed/HS.RData")

# load configuration file
source("homogenization/config.ini")

# load breakfiles
breakpoints <- read_csv(
  "homogenization/data/02_processed/breakpoints.csv",
  show_col_types = FALSE
)

# load vector "interquantile_subset" that contains the quantiles for the interquantile subset
load("homogenization/data/02_processed/interquantile_subset.RData")

# ___________________________________________________________________
# Homogenization ----
# Homogenize every candidate station
# ___________________________________________________________________

# list of candidate stations to be homogenized
candidate_stations <- HS_reference %$%
  id_candidate |>
  unique()

# iterate through all candidate stations
i <- 1
for (i in seq_along(candidate_stations)) {
  # select candidate station
  candidate <- candidate_stations[i]

  # create dataframe containing the data of candidate station and its reference station
  data <- HS |>
    filter(id == candidate) |>
    rename(id_candidate = id) |>
    mutate(
      snow_depth_reference = HS_reference |>
        filter(id_candidate == candidate) %$%
        snow_depth_reference
    )

  # iterate through breakpoints and get quantiles of candidate and reference station
  breakpoints_candidate <- breakpoints |>
    filter(id_candidate == candidate) %$%
    hyear

  b <- 1
  for (b in seq_along(breakpoints_candidate)) {
    # ___________________________________________________________________
    # interquantile subsets ----
    # of candidate and reference station before and after breakpoint
    # ___________________________________________________________________

    # select breakpoint
    breakpoint <- breakpoints_candidate[b]

    # calculate interquantile subsets for candidate and reference time series
    iqs_candidate <- calculate_iqs(
      data, 
      "snow_depth_orig", 
      interquantile_subset, 
      breakpoint
      )
    
    iqs_reference <- calculate_iqs(
      data, 
      "snow_depth_reference", 
      interquantile_subset, 
      breakpoint
      )

    # ___________________________________________________________________
    # Calculate values for adjustment formula ----
    # ___________________________________________________________________
  }
}
