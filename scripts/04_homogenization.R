# ___________________________________________________________________
# ___________________________________________________________________
# interpQM homogenization of snow depth data
# Homogenization
# Author: Gernot Resch
# Date: 09.04.2024
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

# load snow depth time series
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

  # create dataframe containing the data of the candidate station and its reference station/time-series
  data <- HS |>
    filter(id == candidate) |>
    rename(id_candidate = id) |>
    mutate(
      snow_depth_reference = HS_reference |>
        filter(id_candidate == candidate) %$%
        snow_depth_reference
    )

  # remember which values are 0. Reset them to 0 after adjustments!
  remember_zero_orig <- which(data$snow_depth_orig == 0)
  remember_zero_reference <- which(data$snow_depth_reference == 0)

  # change 0-values to NA due to their influence on the adjustment-formula (median-calculation)
  data$snow_depth_orig[remember_zero_orig] <- NA
  data$snow_depth_reference[remember_zero_reference] <- NA

  # how many breakpoints are there for this candidate station and when are they?
  breakpoints_candidate <- breakpoints |>
    filter(id_candidate == candidate) %$%
    hyear

  # define time-steps of different breaks
  # A = after all breaks,
  # B1 - Bn = first break to last break (first break is the most recent one)
  data %<>%
    mutate(
      break_steps = {
        # create a vector of the same length as the dataframe
        result <- character(length(hyear))
        # if there is only one break, just make everything before the break B1
        if (length(breakpoints_candidate) == 1) {
          result <- if_else(
            hyear <= breakpoints_candidate,
            "B1",
            "A"
          )
        } else {
          # if there are more breaks, iterate through them
          for (i in seq_along(breakpoints_candidate)) {
            if (i == length(breakpoints_candidate)) {
              result <- if_else(
                hyear > breakpoints_candidate[i],
                "A",
                result
              )
            } else {
              if (i == 1) {
                result <- if_else(
                  hyear <= breakpoints_candidate[i],
                  paste0("B", length(breakpoints_candidate) - i + 1),
                  result
                )
              }
              result <- if_else(
                hyear > breakpoints_candidate[i] &
                  hyear <= breakpoints_candidate[i + 1],
                paste0("B", length(breakpoints_candidate) - i),
                result
              )
            }
          }
        }
        result
      }
    )

  # calculate percentiles per time_step (breaks)
  data %<>%
    group_by(break_steps) %<>%
    mutate(
      # percentiles in the original time series
      percentile_original = ntile(snow_depth_orig, 100),
      # percentiles in the reference time series
      percentile_reference = ntile(snow_depth_reference, 100),
      iqs_original = iqs_subset_calculation(snow_depth_orig, interquantile_subset),
      iqs_reference = iqs_subset_calculation(snow_depth_reference, interquantile_subset)
    ) %<>%
    ungroup()

  # ___________________________________________________________________
  # calculate dataframe with values for adjustment calculation ----
  # ___________________________________________________________________
  # original time series
  df_original <- data |>
    group_by(break_steps, iqs_original) |>
    summarise(
      C = median(snow_depth_orig, na.rm = T),
      .groups = "drop"
    ) |>
    rename(iqs = iqs_original)

  # reference time series
  df_reference <- data |>
    group_by(break_steps, iqs_reference) |>
    summarise(
      R = median(snow_depth_reference, na.rm = T),
      .groups = "drop"
    ) |>
    rename(iqs = iqs_reference)

  # join both dataframes
  df_adjustment_values <- left_join(
    df_original,
    df_reference,
    by = c("break_steps", "iqs")
  ) |>
    na.omit()

  # cleanup
  rm(df_original, df_reference)

  # separate into adjustment values after break and breaks
  df_adjustment_after <- df_adjustment_values |>
    filter(break_steps == "A")

  df_adjustment_breaks <- df_adjustment_values |>
    filter(break_steps != "A")

  # iterate through breaks and calculate adjustments
  break_steps <- df_adjustment_breaks$break_steps |> unique()

  b <- 1
  for (b in seq_along(break_steps)) {
    # ___________________________________________________________________
    # calculate adjustment factors for each interquantile subset
    # and store them in a vector ----
    # ___________________________________________________________________

    # vector for holding the adjustment values of the interquantile subsets
    adjustment_vector <- rep(NA, length(interquantile_subset)) |>
      setNames(interquantile_subset)

    a <- 1
    for (a in seq_along(adjustment_vector)) {
      Ca <- df_adjustment_after |>
        filter(iqs == interquantile_subset[a]) |>
        pull(C)

      Ra <- df_adjustment_after |>
        filter(iqs == interquantile_subset[a]) |>
        pull(R)

      Cb <- df_adjustment_breaks |>
        filter(
          break_steps == break_steps[b],
          iqs == interquantile_subset[a]
        ) |>
        pull(C)

      Rb <- df_adjustment_breaks |>
        filter(
          break_steps == break_steps[b],
          iqs == interquantile_subset[a]
        ) |>
        pull(R)

      # calculate adjustment factor for each interquantile subset
      adjustment_vector[a] <- (Ca / Ra) / (Cb / Rb)
    }

    # cleanup
    rm(Ca, Ra, Cb, Rb, a)


    # ___________________________________________________________________
    # interpolate the adjustment factor along the percentiles ----
    # ___________________________________________________________________
    # Get the midpoints of the interquantile subsets
    # For the first segment (0 to first subset value)
    first_midpoint <- mean(c(0, interquantile_subset[1])) |>
      floor()

    # For the rest of the segments (between subset values)
    other_midpoints <- numeric(length(interquantile_subset) - 1)

    for (m in 1:(length(interquantile_subset) - 1)) {
      other_midpoints[m] <- mean(
        c(
          interquantile_subset[m],
          interquantile_subset[m + 1]
        )
      ) |>
        floor()
    }

    # Combine all midpoints
    midpoints <- c(first_midpoint, other_midpoints)

    # cleanup
    rm(first_midpoint, other_midpoints, m)

    # Interpolate the adjustment factor along the percentiles
    percentile_length <- length(unique(data$percentile_original))
    if (percentile_length > 100) {
      percentile_length <- 100
    }

    # if > 100 unique snow depth values, interpolate along the percentiles
    if (percentile_length == 100) {
      adjustment_vector_lin <- approx(
        x = midpoints,
        y = adjustment_vector,
        xout = 1:percentile_length,
        method = "linear"
      )$y
      # if not, reduce the vector to the length of the unique snow depth values
      # and interpolate accordingly
    } else {
      midpoints <- floor(
        (percentile_length / 100) * midpoints
      )

      adjustment_vector_lin <- approx(
        x = midpoints,
        y = adjustment_vector,
        xout = 1:percentile_length,
        method = "linear"
      )$y
    }


    # fillup head and tail of the linear adjustment-vector
    adjustment_vector_lin[1:midpoints[1]] <- first(adjustment_vector)
    # fillup between the last midpoint and the end of the vector
    adjustment_vector_lin[midpoints[length(midpoints)]:percentile_length] <- last(adjustment_vector)

    # and turn into tibble for joining with the dataset
    df_adjustment_lin <- tibble(
      break_steps = break_steps[b],
      percentile_original = 1:percentile_length,
      adjustment_factor = adjustment_vector_lin
    )

    # join data and adjustment-factors (each percentile gets its own adjustment factor)
    data <- left_join(
      data,
      df_adjustment_lin,
      by = c("break_steps", "percentile_original")
    )
  }

  # ___________________________________________________________________
  # apply adjustments to the time series ----
  # ___________________________________________________________________
  data %<>%
    mutate(
      snow_depth_homogenized = snow_depth_orig * adjustment_factor,
      # add original time series after the break
      snow_depth_homogenized = if_else(
        break_steps == "A",
        snow_depth_orig,
        snow_depth_homogenized
      )
    )

  # ___________________________________________________________________
  # reset 0-values ----
  # ___________________________________________________________________
  # reset values that where 0 prior the homogenization to 0
  data$snow_depth_orig[remember_zero_orig] <- 0
  data$snow_depth_homogenized[remember_zero_orig] <- 0
  data$snow_depth_reference[remember_zero_reference] <- 0

  # ___________________________________________________________________
  # combine homogenized data ----
  # ___________________________________________________________________

  if (i == 1) {
    HS_homogenized <- data
  } else {
    HS_homogenized <- bind_rows(HS_homogenized, data)
  }
}

# ___________________________________________________________________
# save homogenized data to disk----
# ___________________________________________________________________
HS_homogenized |>
  write_csv(
    file = "homogenization/data/03_homogenized/HS_homogenized.csv"
  )

HS_homogenized |>
  save(
    file = "homogenization/data/03_homogenized/HS_homogenized.RData"
  )
