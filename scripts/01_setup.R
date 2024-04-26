# ___________________________________________________________________
# ___________________________________________________________________
# interpQM
# 01_setup ----
# Setup the environment for a new homogenzation-run
# Check library-dependencies, create file-structure and config-file
# Author: Gernot Resch
# Date: 08.03.2024
# ___________________________________________________________________
# ___________________________________________________________________

rm(list = ls())

source("scripts/functions.R")

# ___________________________________________________________________
# package-function for checking and installing needed packages----
# ___________________________________________________________________

# Check and install packages if necessary
install_if_missing("geosphere")
install_if_missing("tidyverse")
install_if_missing("readr")
install_if_missing("magrittr")

message("All necessary packages are installed.")

# ___________________________________________________________________
# ___________________________________________________________________
# # create data-filestructure ----
# ___________________________________________________________________
# ___________________________________________________________________

# delete already existing homogenization-directory
if (file.exists("homogenization")) {
  unlink("homogenization", recursive = TRUE)
}

# and create a fresh directory-structure
dir.create("homogenization")
dir.create("homogenization/data")
dir.create("homogenization/data/01_original")
dir.create("homogenization/data/02_processed")
dir.create("homogenization/data/03_homogenized")
dir.create("homogenization/plots")

# check if directory-structure was created
if (file.exists("homogenization")) {
  message("Directory-structure created.")
} else {
  stop("Directory-structure could not be created.")
}

# ___________________________________________________________________
# Create empty files to be filled ----
# ___________________________________________________________________

create_empty_text_file("homogenization/data/01_original/candidate_stations.csv")
create_empty_text_file("homogenization/data/01_original/candidate_stations_manual.csv")
create_empty_text_file("homogenization/data/01_original/detected_breakpoints.csv")
create_empty_text_file("homogenization/data/01_original/meta.csv")


# ___________________________________________________________________
# Create config-file "config.ini" ----
# ___________________________________________________________________
content <- "########################################
# Command-file for homogenization
# +++++ reference methods:
# 1 (highest correlated series of network),
# mean (unweighted mean of network)
# wmean (weighted mean of network)
# +++++ plots
# yes / no
# +++++ qmapping: percentile-boundaries for quantile-matching
# +++++ correlation_weight: weighting of wmean (linear, exponential)
# snow_file: name of the snow-file in the 01_original-folder
########################################
snow_file = \"HS.csv\"
distance_horizontal = \"100\"
distance_vertical = \"300\"
reference = \"wmean\"
plots = \"yes\"
interquantile_subset = \"0, 0.94, 1\"
correlation_weight = \"linear\""

# Write content to file
writeLines(content, "homogenization/config.ini")

# Check if file was created
if (file.exists("homogenization/config.ini")) {
  message("Configuration file created in homogenization/config.ini")
} else {
  stop("Configuration file could not be created.")
}
