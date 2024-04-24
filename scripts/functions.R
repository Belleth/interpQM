# ___________________________________________________________________
# ___________________________________________________________________
# interpQM-functions ----
# ___________________________________________________________________
# ___________________________________________________________________

# ___________________________________________________________________
# install_if_missing ----
# ___________________________________________________________________

# install package if not already installed
install_if_missing <- function(package_name) {
  # Check if package is installed
  if (!requireNamespace(package_name, quietly = TRUE)) {
    # If not, install it
    install.packages(package_name, dependencies = TRUE)

    # Load the package for check
    if (!requireNamespace(package_name, quietly = TRUE)) {
      stop("The following package couldn't be installed: ", package_name)
    }
  } else {
    message("Package ", package_name, " is installed.")
  }
}

# ___________________________________________________________________
# create_empty_text_file ----
# ___________________________________________________________________

# create empty text files to be filled
create_empty_text_file <- function(text_file) {
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

# ___________________________________________________________________
# horizontal_distance ----
# ___________________________________________________________________

# Calculate horizontal distance between two stations
# needs package geodist -> not loaded in function for reducing HD-load
horizontal_distance <- function(lon1, lat1, lon2, lat2) {
  round(
    distHaversine(
      cbind(lon1, lat1),
      cbind(lon2, lat2)
    ) / 1000 # turn into km
    , 2
  ) # round to 10 meters
}

# ___________________________________________________________________
# vertical distance ----
# ___________________________________________________________________

# Calculate the vertical distance between two stations
vertical_distance <- function(height1, height2) {
  distance <- (height1 - height2)^2 |>
    sqrt()
  return(distance)
}

# ___________________________________________________________________
# norm_min_max ----
# ___________________________________________________________________

# min-max-normalisation
norm_min_max <- function(x) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}


# ___________________________________________________________________
# iqs_subset_calculation ----
# calculates the interquantile subsets of a variable in a dataframe
# ___________________________________________________________________
iqs_subset_calculation <- function(data_vector, interquantile_subset) {
  ntile <- ntile(data_vector, 100)

  result <- rep(NA, length(data_vector))
  for (i in seq_along(interquantile_subset)) {
    result <- case_when(
      is.na(result) & ntile <= interquantile_subset[i] ~ interquantile_subset[i],
      !is.na(result) & ntile <= interquantile_subset[i] ~ result,
      TRUE ~ NA
    )
  }

  return(result)
}
