# ___________________________________________________________________
# ___________________________________________________________________
# interpQM homogenization of snow depth data
# 03_homogenization ----
# Homogenization
# Author: Gernot Resch
# Date: 07.03.2024
# ___________________________________________________________________
# ___________________________________________________________________

rm(list = ls())

library(tidyverse)

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
            hyear <- 
      snow_depth_reference = HS_reference |>
        filter(id_candidate == candidate) %$%
        snow_depth_reference
    ) |>
    as_tibble()

  # iterate through breakpoints and get quantiles of candidate and reference station
  breakpoints_candidate <- breakpoints |>
    filter(id_candidate == candidate) %$%
    hyear

  b <- 1
  for (b in seq_along(breakpoints_candidate)) {
    # select breakpoint
    breakpoint <- breakpoints_candidate[b]
    
    # get quantiles of candidate and reference station before and after breakpoint
    quantiles_candidate_after <- data |> 
      filter(hyear > breakpoint) |>
      pull(snow_depth) |>
      quantile(
        probs = c(qmapping),
        na.rm = TRUE
      ) |>
      as_tibble() |>
      rename(
        quantile_10 = `10%`,
        quantile_50 = `50%`,
        quantile_90 = `90%`
      )
    
    
    
    
  }
}
