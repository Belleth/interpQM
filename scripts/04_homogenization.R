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
# Title ----
# ___________________________________________________________________
