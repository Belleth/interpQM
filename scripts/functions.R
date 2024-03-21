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
horizontal_distance <- function(lon1, lat1, lon2, lat2)
{
  round(
    distHaversine(
      cbind(lon1, lat1), 
      cbind(lon2, lat2)
    ) / 1000 # turn into km
    , 2) # round to 10 meters
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
# calculate_iqs ----
# ___________________________________________________________________

# Calculate interquantile subset boundaries before and after break
calculate_iqs <- function(data, snow_depth, interquantile_subset, breakpoint) {
  data |>
    mutate(is_after_break = hyear >= breakpoint) |>
    group_by(is_after_break) |>
    summarize(
      across(
        all_of(snow_depth),
        ~ list(
          quantile(., probs = interquantile_subset, na.rm = TRUE) |>
            floor()
        ),
        .names = "iqs"
      )
    ) |>
    unnest(cols = everything()) |>
    ungroup() |>
    mutate(is_after_break = if_else(
      is_after_break == FALSE,
      "before",
      "after"
    )) %$%
    split(iqs, is_after_break) |>
    as_tibble()
}
