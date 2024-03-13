# ___________________________________________________________________
# ___________________________________________________________________
# interpQM homogenization of snow depth data
# 01_data preparation ----
# Author: Gernot Resch
# Date: 29.02.2024
# ___________________________________________________________________
# ___________________________________________________________________

rm(list = ls())

# load packages
library(tidyverse)
library(geosphere)
library(magrittr)

# load functions
source("scripts/f_generic.R")

# load information for horizontal and vertical distances
source("homogenization/config.ini")
distance_horizontal <- as.numeric(distance_horizontal)
distance_vertical <- as.numeric(distance_vertical)

# load stations to be homogenized
candidate_stations <- read_csv(
  "homogenization/data/01_original/candidate_stations.csv",
  show_col_types = FALSE,
) |>
  # make sure its character values
  mutate(
    id_candidate = as.character(id_candidate)
  ) |>
  # make sure there are no double-entries
  distinct_all() |>
  # turn into vector
  pull(id_candidate)


# ___________________________________________________________________
# Load metadata ----
# ___________________________________________________________________

# load meta-data
meta <- read_csv(
  "homogenization/data/01_original/meta.csv",
  show_col_types = FALSE
)

# make sure no double-entries exist in meta-file
meta %<>%
  distinct_all()

# ___________________________________________________________________
# # load snow data ----
# ___________________________________________________________________
# import snow data using the file-name from interpqm.ini
HS_import <- read_csv(
  paste0("homogenization/data/01_original/", snow_file),
  show_col_types = FALSE
)

# get rid of possible NAs in id-column
HS_import %<>%
  filter(!is.na(id))

## calculate start and end of snow observations for every station
## for adding to meta-file
meta_begin_end <- HS_import |>
  group_by(id) |>
  filter(!is.na(snow_depth_orig)) |>
  summarise(
    begin_observations = min(date),
    end_observations = max(date)
  )

# add this information to the meta-file
meta <- left_join(
  meta,
  meta_begin_end,
  by = "id"
)

# cleanup
rm(meta_begin_end)

# export new meta-file
meta |>
  write_csv(file = "homogenization/data/02_processed/meta.csv")

# ___________________________________________________________________
# Extend timeseries ----
# Extend each time series to the minimum/maximum of all used time series.
# Makes building of networks, calculating correlations etc. easier.
# ___________________________________________________________________

# get minimum observed data and extend to 1.1. of that year
minimum_date <- min(meta$begin_observations) |>
  # reduce to year
  year() |>
  # and add 1.1.
  make_date(1, 1)

# get maximum observed data and extend to 31.12. of that year
maximum_date <- max(meta$end_observations) |>
  # reduce to year
  year() |>
  # and extend to 31.12.
  make_date(12, 31)

# create dataframe for full time series
unique_dates <- seq(
  minimum_date,
  maximum_date,
  by = "1 day"
) |>
  as.Date()

# create vectors with unique ids and dates
unique_ids <- unique(HS_import$id)

# combine ids and dates to get a full dataframe
all_combinations <- expand.grid(
  id = unique_ids,
  date = unique_dates
)

# combine full dataframe with snow data
HS <- left_join(
  all_combinations,
  HS_import,
  by = c("id", "date")
)

# calculate hydrological year for each date (1999 = 1.10.1998 - 30.09.1999)
HS <- HS |> 
  mutate(
    month = month(date),
    year = year(date),
    hyear = if_else(month <= 9, year - 1, year)
  ) |> 
  # get rid of month and year, because they are not needed anymore
  select(-month, -year) |> 
  # sort output by id and date
  arrange(id, date) |> 
  # turn into tibble
  as_tibble()

# export correct snowdepth file to disk
HS |>
  save(file = "homogenization/data/02_processed/HS.RData")

# cleanup
rm(
  maximum_date, minimum_date, unique_dates,
  all_combinations, unique_ids
)


# ___________________________________________________________________
# ___________________________________________________________________
# Calculate possible reference stations for each station ----
# here, network_builder is created, which is being used
# for putting together reference networks for each station
# ___________________________________________________________________
# ___________________________________________________________________

# ___________________________________________________________________
# calculate vertical distance between stations ----
# ___________________________________________________________________

# calculate horizontal distance in [km] between stations
distances_horizontal <- expand.grid(
  id_reference = meta$id,
  id_candidate = meta$id
) |>
  filter(id_candidate != id_reference) |>
  mutate(
    distance_horizontal = f_horizontal_distance(
      meta$lon[id_reference], meta$lat[id_reference],
      meta$lon[id_candidate], meta$lat[id_candidate]
    )
  ) |>
  as_tibble()

# ___________________________________________________________________
# calculate vertical distance in [m] between stations ----
# ___________________________________________________________________

distances_vertical <- expand.grid(
  id_reference = meta$id,
  id_candidate = meta$id
) |>
  filter(id_candidate != id_reference) |>
  mutate(
    distance_vertical = f_vertical_distance(
      meta$elevation[id_candidate],
      meta$elevation[id_reference]
    )
  ) |>
  as_tibble()

# ___________________________________________________________________
# combine both distance-datasets ----
# ___________________________________________________________________

distances <- left_join(
  distances_horizontal,
  distances_vertical,
  by = c("id_reference", "id_candidate")
) |>
  group_by(id_candidate) |>
  filter(
    # limit to stations to be homogenized
    id_candidate %in% candidate_stations,
    # limit to horizontal distance from interpqm.ini
    distance_horizontal <= !!distance_horizontal,
    # limit to vertical distance from interpqm.ini
    distance_vertical <= !!distance_vertical
  ) |>
  # sort output after horizontal and vertical distance
  arrange(
    distance_horizontal,
    distance_vertical
  ) |>
  ungroup() |>
  # reorder columns for better overview
  select(
    id_candidate,
    id_reference,
    distance_horizontal,
    distance_vertical
  )

# cleanup
rm(
  distances_horizontal,
  distances_vertical
)

# ___________________________________________________________________
# preparations for correlation-loop ----
# ___________________________________________________________________

# list with a table with information for each station to be homogenized
network_builder <- vector(
  mode = "list",
  length(candidate_stations)
)

names(network_builder) <- candidate_stations


# ___________________________________________________________________
# network_builder
# create a data frame for each station that contains the following information: ----
# correlation with all other stations
# horizontal and vertical distance to all other stations
# ___________________________________________________________________

start_time <- Sys.time()

i <- 1
for (i in seq_along(network_builder)) {
  # iterated station
  id_iteration <- names(network_builder)[i]

  # copy distance-dataframe to be filled
  network <- distances |>
    filter(
      id_candidate == id_iteration
    ) |>
    mutate(
      correlation = NA
    )

  if (nrow(network) >= 2) {
    # calculate correlation (spearman) to each station
    # get data from candidate station
    hs_candidate <- HS |>
      filter(id == id_iteration) |>
      pull(snow_depth_orig)

    d <- 1
    for (d in 1:nrow(network)) {
      # get hs-data from each other station
      hs_reference <- HS |>
        filter(id == network$id_reference[d]) |>
        pull(snow_depth_orig)

      # calculate correlation
      network$correlation[d] <- tryCatch(
        expr = {
          cor(
            hs_candidate,
            hs_reference,
            method = "spearman",
            use = "complete.obs"
          )
        },
        error = function(e) {
          return(NA)
        }
      )

      # sort output
      network <- network |>
        arrange(
          desc(correlation),
          distance_horizontal,
          distance_vertical
        )
    }
  } else {
    network <- tibble(
      id_candidate = NA,
      id_reference = NA,
      distance_horizontal = NA,
      distance_vertical = NA,
      correlation = NA
    )
  }

  # write to list of all networks
  network_builder[[i]] <- network
}


# change to long-format
network_builder %<>%
  bind_rows()

# save network_builder to disk
network_builder |>
  save(file = "homogenization/data/02_processed/network_builder.rda")

network_builder |>
  write_csv(file = "homogenization/data/02_processed/network_builder.csv")


# get time when script has finished running
end_time <- Sys.time()

# how long did the script run?
run_time <- round(end_time - start_time, 2)
run_time
# 14min for 40 stations

# cleanup
rm(
  i, d, network, distances,
  HS_import, start_time, end_time, run_time
)

# ___________________________________________________________________
# Logfile-Output ----
# ___________________________________________________________________

network_size <- network_builder |>
  group_by(id_candidate) |>
  summarise(n = n()) |>
  arrange(id_candidate)

network_size |>
  write_csv(file = "homogenization/data/02_processed/network_size.csv")

# ___________________________________________________________________
# prepare breakpoint-file ----
# ___________________________________________________________________

# load breakpoint-data
breakpoints <- read_csv(
  "homogenization/data/01_original/detected_breakpoints.csv",
  show_col_types = FALSE
) |>
  # make sure candidate_ids are character and year of break is numeric
  mutate(
    id_candidate = as.character(station_id),
    hyear = as.numeric(hyear)
  ) |> 
  select(id_candidate, hyear) |> 
  # make sure stations with more than one break are arranged next to each other
  arrange(id_candidate, hyear)

# load network_size for comparing with stations that have breaks
network_size <- read_csv(
  "homogenization/data/02_processed/network_size.csv",
  show_col_types = FALSE
)

# compare network_size with breakpoints
# for checking, if there are stations that are not homogenizable
stations_not_homogenizable <- anti_join(
  network_size,
  breakpoints,
  by = "id_candidate"
) |>
  select(id_candidate)

# export stations that are not homogenizable
stations_not_homogenizable |>
  write_csv(file = "homogenization/data/02_processed/stations_not_homogenizable.csv")

# get rid of non-homogenizable stations in the breakpoint-file
breakpoints %<>%
  filter(id_candidate %in% network_size$id_candidate) |> 
  # get rid of possible double-entries
  distinct_all()

# export cleaned breakpoint-file
breakpoints |>
  write_csv(file = "homogenization/data/02_processed/breakpoints.csv")
