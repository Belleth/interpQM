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

# ___________________________________________________________________
# package-function for checking and installing needed packages----
# ___________________________________________________________________

f_install_if_missing <- function(pkg_name) {
  # Check if package is installed
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    # If not, install it
    install.packages(pkg_name, dependencies = TRUE)

    # Load the package for check
    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      stop("The following package couldn't be installed: ", pkg_name)
    }
  } else {
    message("Package ", pkg_name, " is installed.")
  }
}

# Check and install packages if necessary
f_install_if_missing("geosphere")
f_install_if_missing("tidyverse")
f_install_if_missing("readr")
f_install_if_missing("magrittr")

message("All necessary packages are installed.")

# ___________________________________________________________________
# ___________________________________________________________________
# # create data-filestructure ----
# ___________________________________________________________________
# ___________________________________________________________________

# delete directory
f_dir_clean <- function(directory_name) {
  if (file.exists(directory_name)) {
    unlink(directory_name, recursive = TRUE)
  }
}

# ___________________________________________________________________
# Clean old directory ----
# ___________________________________________________________________

# delete homogenization-directory
if (file.exists("homogenization")) {
  unlink("homogenization", recursive = TRUE)
}

# create directory-structure
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
f_create_empty_text_file <- function(text_file) {
  # Erstelle eine leere Textdatei
  file.create(text_file)

  # Überprüfe, ob die Textdatei erfolgreich erstellt wurde
  if (file.exists(text_file)) {
    writeLines(
      "Please fill me with data.",
      text_file,
      useBytes = TRUE
    )
    message("File created: ", text_file)
  } else {
    stop("Error while creating file: ", text_file)
  }
}

# create empty files
f_create_empty_text_file("homogenization/data/01_original/candidate_stations.csv")
f_create_empty_text_file("homogenization/data/01_original/detected_breaks.csv")
f_create_empty_text_file("homogenization/data/01_original/meta.csv")
f_create_empty_text_file("homogenization/data/01_original/reference_stations_manual.csv")

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
# +++++ InterpQMversion: name of version in qmapping.list
# +++++ correlation_weight: weighting of wmean (linear, exponential)
# snow_file: name of the snow-file in the 01_original-folder
########################################
snow_file = \"HS.csv\"
distance_horizontal = \"100\"
distance_vertical = \"300\"
reference = \"wmean\"
plots = \"yes\"
InterpQMversion = \"0.95\"
correlation_weight = \"linear\""

# Write content to file
writeLines(content, "homogenization/config.ini")

# Check if file was created
if (file.exists("homogenization/config.ini")) {
  message("Configuration file created in homogenization/config.ini")
} else {
  stop("Configuration file could not be created.")
}
