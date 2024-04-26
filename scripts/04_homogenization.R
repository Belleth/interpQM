# ___________________________________________________________________
# ___________________________________________________________________
# interpQM homogenization of snow depth data
# Homogenization
# Author: Gernot Resch
# Date: 26.04.2024
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
interquantile_subset_import <- interquantile_subset
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

  # ___________________________________________________________________
  # define time-steps of different breaks "break_steps" ----
  # ___________________________________________________________________

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


  # ___________________________________________________________________
  # check how many different observations are per time step ----
  # this is important for the adjustment calculation, because if
  # there are not enough observations, the adjustment will be
  # calculated with an interpolation of less than 100 values (and not with
  # percentiles)
  # ___________________________________________________________________

  percentile_check <- data |>
    filter(!is.na(snow_depth_orig)) |>
    group_by(id_candidate, break_steps) |>
    summarise(
      unique_snow_observations = n_distinct(snow_depth_orig),
      unique_years = n_distinct(hyear),
      .groups = "drop"
    )

  # create a dataframe for the export of the percentile_check
  # it contains all unique years and observations per break
  # of all candidate stations
  if (i == 1) {
    percentile_export <- percentile_check
  } else {
    percentile_export <- bind_rows(
      percentile_export,
      percentile_check
    )
  }

  # get the smallest number of unique observations per time step
  unique_snow_minimum <- percentile_check$unique_snow_observations |>
    min()

  # ___________________________________________________________________
  # adapt iqs-subset if necessary ----
  # ___________________________________________________________________
  # adapt the iqs-subset to the number of unique snow observations in percentile_check.
  # This is necessary, if there are less than 100 percentiles available.

  # reset interquantile subset
  interquantile_subset <- interquantile_subset_import
  ntile_length <- 100

  if (unique_snow_minimum < 100) {
    interquantile_subset <- floor((interquantile_subset / 100) * unique_snow_minimum)
    ntile_length <- unique_snow_minimum
  }

  # ___________________________________________________________________
  # calculate percentiles per time_step (breaks) ----
  # ___________________________________________________________________

  # if (unique_snow_minimum >= 100) {
  data %<>%
    group_by(break_steps) %<>%
    mutate(
      # percentiles in the original time series
      percentile_original = ntile(
        snow_depth_orig,
        # use the smallest number of unique snow observations here.
        ntile_length
      ),
      # percentiles in the reference time series
      percentile_reference = ntile(
        snow_depth_reference,
        ntile_length
      ),
      iqs_original = iqs_subset_calculation(snow_depth_orig, interquantile_subset),
      iqs_reference = iqs_subset_calculation(snow_depth_reference, interquantile_subset)
    ) %<>%
    ungroup()
  # }

  # ___________________________________________________________________
  # calculate dataframe with values for adjustment calculation ----
  # ___________________________________________________________________
  # original time series
  df_original <- data |>
    filter(!is.na(iqs_original)) |>
    group_by(break_steps, iqs_original) |>
    summarise(
      C = median(snow_depth_orig, na.rm = T) |>
        round(),
      .groups = "drop"
    ) |>
    rename(iqs = iqs_original)

  # reference time series
  df_reference <- data |>
    filter(!is.na(iqs_reference)) |>
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
  )

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
        filter(iqs == !!interquantile_subset[a]) |>
        pull(C)

      Ra <- df_adjustment_after |>
        filter(iqs == !!interquantile_subset[a]) |>
        pull(R)

      Cb <- df_adjustment_breaks |>
        filter(
          break_steps == !!break_steps[b],
          iqs == !!interquantile_subset[a]
        ) |>
        pull(C)

      Rb <- df_adjustment_breaks |>
        filter(
          break_steps == !!break_steps[b],
          iqs == !!interquantile_subset[a]
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
    adjustment_vector_lin <- approx(
      x = midpoints,
      y = adjustment_vector,
      xout = 1:ntile_length,
      method = "linear"
    )$y

    # fillup head and tail of the linear adjustment-vector
    adjustment_vector_lin[1:midpoints[1]] <- first(adjustment_vector)
    # fillup between the last midpoint and the end of the vector
    adjustment_vector_lin[midpoints[length(midpoints)]:ntile_length] <- last(adjustment_vector)

    # join data and adjustment-factors (each percentile gets its own adjustment factor)
    # only for the corresponding break_time

    df_join <- tibble(
      break_steps = !!break_steps[b],
      percentile_original = 1:ntile_length,
      adjustment_factor = adjustment_vector_lin
    )

    # combine all adjustment factors.
    # This "df_adjustment_candidate" will be used later for joining with the dataset.
    if (b == 1) {
      df_adjustment_candidate <- df_join
    } else {
      df_adjustment_candidate <- bind_rows(
        df_adjustment_candidate,
        df_join
      )
    }
  }

  # cleanup
  rm(
    adjustment_vector, adjustment_vector_lin, midpoints, b,
    df_join
  )

  # ___________________________________________________________________
  # # join adjustment factors with the dataset  ----
  # ___________________________________________________________________
  # join via each break and percentile_original
  data <- left_join(
    data,
    df_adjustment_candidate,
    by = c("break_steps", "percentile_original")
  )

  # ___________________________________________________________________
  # apply adjustments ----
  # ___________________________________________________________________
  data %<>%
    mutate(
      # apply adjustment factor to the original time series
      snow_depth_homogenized = snow_depth_orig * adjustment_factor,
      # and add original time series after the break
      snow_depth_homogenized = if_else(
        break_steps == "A",
        snow_depth_orig,
        snow_depth_homogenized
      )
    )

  # ___________________________________________________________________
  # restore 0-values ----
  # ___________________________________________________________________
  # restore values that where 0 prior the homogenization to 0,
  # so they are not influenced by the adjustment and no snow days are lost or created
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

# export percentile_check-file (how many unique years and observations per break)
percentile_export |>
  write_csv(
    file = "homogenization/data/03_homogenized/percentile_check.csv"
  )
