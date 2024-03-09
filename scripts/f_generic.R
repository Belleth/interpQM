# ___________________________________________________________________
# ___________________________________________________________________
# interpQM-functions ----
# ___________________________________________________________________
# ___________________________________________________________________

# function for calculating horizontal distance between two stations
# needs package geodist -> not loaded in function for reducing HD-load
f_horizontal_distance <- function(lon1, lat1, lon2, lat2)
{
  round(
    distHaversine(
      cbind(lon1, lat1), 
      cbind(lon2, lat2)
    ) / 1000 # turn into km
    , 2) # round to 10 meters
}

# function for calculating the vertical distance between two stations
f_vertical_distance <- function(height1, height2) {
  distance <- (height1 - height2)^2 |> 
    sqrt()
  return(distance)
}

# min-max-normalisation
f_norm_min_max <- function(x) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}

  
