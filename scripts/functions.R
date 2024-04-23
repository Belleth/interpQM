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






# ___________________________________________________________________
# calculate_iqs ----
# ___________________________________________________________________

# Calculate interquantile subset boundaries before and after break
calculate_iqs <- function(data, snow_depth, interquantile_subset, breakpoint) {
  data |>
    # add column to indicate if year is after breakpoint
    mutate(is_after_break = hyear >= breakpoint) |>
    # group after that column
    group_by(is_after_break) |>
    # calculate interquantile subsets contained in "interquantile_subset" for each group
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
    # unnest the list column
    unnest(cols = everything()) |>
    # ungroup it
    ungroup() |>
    # add column to indicate if year is after breakpoint
    mutate(is_after_break = if_else(
      is_after_break == FALSE,
      "before",
      "after"
    )) %$%
    # split the iqs column into a matrix
    split(iqs, is_after_break) |>
    # transpose it to get the iqs as columns
    as.data.frame() |>
    t() |>
    as.data.frame() |>
    # change colnames to interquantile subset
    setNames(as.character(interquantile_subset)) |>
    # add column to indicate if after/before a breakpoint
    rownames_to_column(var = "after_before") ->> iqs

  return(iqs)
}

# ___________________________________________________________________
# Adjustment formula ----
# calculate median Ca, Cb, Ra and Rb of snow depth for candidate and reference station
# ___________________________________________________________________

# prepare data
adjustment_factor <- function(data, breakpoint) {
  # calculate median Ca, Cb, Ra and Rb of snow depth for candidate and reference station
  df_median <- data |>
    mutate(
      after_break = hyear >= breakpoint
    ) |>
    group_by(after_break) |>
    summarize(
      C = median(snow_depth_orig, na.rm = TRUE),
      R = median(snow_depth_reference, na.rm = TRUE)
    )

  # Calculate variables for adjustment
  # Candidate after break
  Ca <- df_median |>
    filter(after_break == TRUE) |>
    pull(C)

  # Candidate before break
  Cb <- df_median |>
    filter(after_break == FALSE) |>
    pull(C)

  # Reference after break
  Ra <- df_median |>
    filter(after_break == TRUE) |>
    pull(R)

  # Reference before break
  Rb <- df_median |>
    filter(after_break == FALSE) |>
    pull(R)

  # Calculate adjustment-factor
  adjustment_factor <- (Ca / Ra) / (Cb / Rb)

  return(adjustment_factor)
}
