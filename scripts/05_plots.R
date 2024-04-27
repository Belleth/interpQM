# ___________________________________________________________________
# ___________________________________________________________________
# interpQM: Homogenization of snow depth data
# 05_Plots ----
# Author: Gernot Resch
# Date: 26.04.2024
# ___________________________________________________________________
# ___________________________________________________________________

rm(list = ls())

library(tidyverse)

# load functions
source("scripts/functions_plot.R")
theme_set(theme_minimal)

# load snow depth data
load("homogenization/data/03_homogenized/HS_homogenized.RData")

# load percentile-checkfile
percentile_check <- read_csv(
  "homogenization/data/03_homogenized/percentile_check.csv",
  show_col_types = FALSE
)

# load break file
breaks <- read_csv(
  "homogenization/data/02_processed/breakpoints.csv",
  show_col_types = FALSE
)


# ___________________________________________________________________
# Checks of percentiles ----
# Percentiles per time step
# A = After all breaks
# B1 = between break 1 and 2
# B2 = and so on
# ___________________________________________________________________
p <- percentile_check |>
  ggplot(
    aes(
      x = unique_snow_observations,
      y = id_candidate,
      col = break_steps,
      fill = break_steps,
      shape = break_steps
    )
  ) +
  geom_vline(xintercept = 100) +
  geom_point(size = 3) +
  labs(
    title = "Number of unique snow observations per time step",
    x = "Number of unique snow observations",
    y = element_blank()
  ) +
  facet_wrap(
    vars(break_steps),
    scales = "free_x"
  ) +
  theme(
    legend.position = "none"
  )

p |>
  ggsave(
    file = "homogenization/plots/percentile_check_unique_snow_observations.pdf",
    width = 20,
    height = 20,
    dpi = 450,
    units = "cm"
  )

# ___________________________________________________________________
# Break length ----
# Checks of number of years per time step
# A = After all breaks
# B1 = between break 1 and 2
# B2 = and so on
# ___________________________________________________________________
p <- percentile_check |>
  ggplot(
    aes(
      x = unique_years,
      y = id_candidate,
      col = break_steps,
      fill = break_steps,
      shape = break_steps
    )
  ) +
  geom_point(size = 3) +
  labs(
    title = "Number of number of years per time step",
    x = "Number of years",
    y = element_blank()
  ) +
  facet_wrap(
    vars(break_steps),
    scales = "free_x"
  ) +
  theme(
    legend.position = "none"
  )

p |>
  ggsave(
    file = "homogenization/plots/break_length.pdf",
    width = 20,
    height = 20,
    dpi = 450,
    units = "cm"
  )

# ___________________________________________________________________
# Adjustment factors ----
# ___________________________________________________________________

# Calculate adjustment factor per station per break
adjustment_factors <- HS_homogenized |>
  filter(break_steps != "A") |>
  group_by(id_candidate, break_steps, percentile_original) |>
  distinct(adjustment_factor) |>
  arrange(id_candidate, break_steps, percentile_original) |>
  ungroup()

# get stations for iteration
homogenized_stations <- adjustment_factors$id_candidate |>
  unique()

# iterate over stations and make plots
for (i in seq_along(homogenized_stations)) {
  p <- adjustment_factors |>
    filter(
      id_candidate == homogenized_stations[i]
    ) |>
    ggplot(
      aes(
        x = percentile_original,
        y = adjustment_factor,
        col = break_steps
      )
    ) +
    geom_point() +
    labs(
      title = paste0(
        "Adjustment factors for station ",
        homogenized_stations[i]
      ),
      x = "Percentiles",
      y = "Adjustment factor"
    ) +
    theme(
      legend.title = element_blank()
    )

  p |>
    ggsave(
      file = paste0(
        "homogenization/plots/adjustment_factors/adjustment_factors_",
        homogenized_stations[i],
        ".pdf"
      ),
      width = 20,
      height = 10,
      dpi = 450,
      units = "cm"
    )
}


# ___________________________________________________________________
# Seasonal mean snow depth ----
# ___________________________________________________________________

# calculate seasonal mean snow depth
HS_season <- HS_homogenized |>
  group_by(id_candidate, hyear) |>
  summarise(
    homogenized = mean(snow_depth_homogenized, na.rm = TRUE),
    original = mean(snow_depth_orig, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(homogenized, original),
    names_to = "time_series",
    values_to = "snow_depth"
  ) |>
  ungroup()

# get stations for iteration
homogenized_stations <- HS_season$id_candidate |>
  unique()

# iterate over stations and make plots
for (i in seq_along(homogenized_stations)) {
  breaks_station <- breaks |>
    filter(
      id_candidate == homogenized_stations[i]
    ) |>
    pull(hyear)

  p <- HS_season |>
    filter(
      id_candidate == homogenized_stations[i]
    ) |>
    ggplot(
      aes(
        x = hyear,
        y = snow_depth,
        col = time_series
      )
    ) +
    geom_vline(
      xintercept = breaks_station,
      linetype = "dotted"
    ) +
    geom_line() +
    labs(
      title = paste0(
        "Comparison of mean seasonal snow depth for station ",
        homogenized_stations[i]
      ),
      x = element_blank(),
      y = "Snow depth [cm]"
    ) +
    theme(
      legend.title = element_blank()
    )

  p |>
    ggsave(
      file = paste0(
        "homogenization/plots/mean/seasonal_mean_snow_depth_",
        homogenized_stations[i],
        ".pdf"
      ),
      width = 20,
      height = 10,
      dpi = 450,
      units = "cm"
    )
}


# ___________________________________________________________________
# Seasonal maximum snow depth ----
# ___________________________________________________________________
# calculate seasonal maximum snow depth
HS_season <- HS_homogenized |>
  group_by(id_candidate, hyear) |>
  summarise(
    homogenized = max(snow_depth_homogenized, na.rm = TRUE),
    original = max(snow_depth_orig, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(homogenized, original),
    names_to = "time_series",
    values_to = "snow_depth"
  )

# get stations for iteration
homogenized_stations <- HS_season$id_candidate |>
  unique()

# iterate over stations and make plots
for (i in seq_along(homogenized_stations)) {
  breaks_station <- breaks |>
    filter(
      id_candidate == homogenized_stations[i]
    ) |>
    pull(hyear)

  p <- HS_season |>
    filter(
      id_candidate == homogenized_stations[i]
    ) |>
    ggplot(
      aes(
        x = hyear,
        y = snow_depth,
        col = time_series
      )
    ) +
    geom_vline(
      xintercept = breaks_station,
      linetype = "dotted"
    ) +
    geom_line() +
    labs(
      title = paste0(
        "Comparison of maximum seasonal snow depth for station ",
        homogenized_stations[i]
      ),
      x = element_blank(),
      y = "Snow depth [cm]"
    ) +
    theme(
      legend.title = element_blank()
    )

  p |>
    ggsave(
      file = paste0(
        "homogenization/plots/max/seasonal_maximum_snow_depth_",
        homogenized_stations[i],
        ".pdf"
      ),
      width = 20,
      height = 10,
      dpi = 450,
      units = "cm"
    )
}

# ___________________________________________________________________
# Seasonal median snow depth ----
# ___________________________________________________________________
# calculate seasonal median snow depth
HS_season <- HS_homogenized |>
  group_by(id_candidate, hyear) |>
  summarise(
    homogenized = median(snow_depth_homogenized, na.rm = TRUE),
    original = median(snow_depth_orig, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(homogenized, original),
    names_to = "time_series",
    values_to = "snow_depth"
  )

# get stations for iteration
homogenized_stations <- HS_season$id_candidate |>
  unique()

# iterate over stations and make plots
for (i in seq_along(homogenized_stations)) {
  breaks_station <- breaks |>
    filter(
      id_candidate == homogenized_stations[i]
    ) |>
    pull(hyear)

  p <- HS_season |>
    filter(
      id_candidate == homogenized_stations[i]
    ) |>
    ggplot(
      aes(
        x = hyear,
        y = snow_depth,
        col = time_series
      )
    ) +
    geom_vline(
      xintercept = breaks_station,
      linetype = "dotted"
    ) +
    geom_line() +
    labs(
      title = paste0(
        "Comparison of median seasonal snow depth for station ",
        homogenized_stations[i]
      ),
      x = element_blank(),
      y = "Snow depth [cm]"
    ) +
    theme(
      legend.title = element_blank()
    )

  p |>
    ggsave(
      file = paste0(
        "homogenization/plots/median/seasonal_median_snow_depth_",
        homogenized_stations[i],
        ".pdf"
      ),
      width = 20,
      height = 10,
      dpi = 450,
      units = "cm"
    )
}
