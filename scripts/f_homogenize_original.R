###############################################################################
############################## Project Hom4Snow ###############################
###############################################################################
###############################################################################
# +++++ Homogenization Functions
###############################################################################
# +++++ create objects for homogenization
f_homPrepare <- function(stations, qmap_version, snow_data) {
  # select InterpQM-Variant
  qmapping <<- qmap_version %>% as.numeric(.) # define vector for calculating quantiles. select 8 (0, 95, 100)
  names_qmap <<- names(qmap_version) %>% # pretty names for analysis-object
    .[-length(.)]


  # prepare output-object for homogenized data
  output_homogenized <- vector("list", length(stations))
  names(output_homogenized) <- stations
  output_homogenized <<- output_homogenized

  # prepare object for correction factors
  corr_factor <- array(NA, c(
    length(stations),
    1 + length(names_qmap)
  )) %>% # 1 + length(names_qmap)
    as.data.frame()
  colnames(corr_factor) <- c("station", names_qmap)
  corr_factor$station <- stations
  corr_factor <<- corr_factor

  corr_factor_lin <- data.frame(array(NA, c(
    length(stations),
    101
  )))
  colnames(corr_factor_lin) <- c("station", paste0(seq(1, 100), "%"))
  corr_factor_lin$station <- stations
  corr_factor_lin <<- corr_factor_lin
  # +++++ gathering-objects
  homogenized <- array(NA, c(
    nrow(snow_data[[1]]),
    length(stations) + 1
  )) %>%
    as.data.frame()
  colnames(homogenized) <- c("date", stations)
  homogenized$date <- snow_data[[1]]$date
  homogenized <<- homogenized

  # hs.gathering <- homogenized

  # # +++++ parameter gathering objects
  # interp.homogenized.parameters <- vector(mode = "list",
  #                                         length(parameter.short))
  # names(interp.homogenized.parameters) <- parameter.short
  # qmap.homogenized.parameters <- interp.homogenized.parameters

  reference_stations <- rep(NA, length(stations)) # prepare export object for used reference stations
  names(reference_stations) <- stations
  reference_stations <<- reference_stations

  # +++++ gathering-object for number of observations per quantile
  # candidate after/before break, reference after/before break
  quantile_count_can <- vector("list", length(stations))
  names(quantile_count_can) <- stations
  quantile_count_can <<- quantile_count_can
  quantile_count_ref <<- quantile_count_can
}

# define Min-Max normalization function
f_norm_min_max <- function(x) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}

# f.weighted <- function(x, y){
#   sum(x * y, na.rm = T) / sum(y)
# }

# +++++ create breakpoint-objects
f_breakpoints <- function(breaks, candidate, l) {
  # +++++ create breakpoint-array
  breakpoints <- array(NA, c(length(breaks[[l]]) + 1, 2)) %>%
    as.data.frame()
  colnames(breakpoints) <- c("start", "end")
  breakpoints$start <- as.Date(breakpoints$start)
  breakpoints$end <- as.Date(breakpoints$end)

  breakpoints[1, 1] <- candidate$date[1] # start observations
  breakpoints[nrow(breakpoints), 1] <- first(breaks[[l]]) # start homogeneous period
  breakpoints[nrow(breakpoints), 1] <- breakpoints[nrow(breakpoints), 1] + 1 # start with next day
  breakpoints[nrow(breakpoints), 2] <- candidate$date[length(candidate$date)] # end homogeneous period

  breaks.rev <- rev(breaks[[l]]) # reverse breaks for
  # easier calculations
  b <- 1
  for (b in 1:length(breaks.rev)) {
    breakpoints[b, 2] <- breaks.rev[b] # end break
    if (b < length(breaks.rev)) {
      breakpoints[c(b + 1), 1] <- breaks.rev[b] # start break
      breakpoints[c(b + 1), 1] <- breakpoints[c(b + 1), 1] + 1 # start with next day
    }
  }
  breakpoints$start <- as.character(breakpoints$start)
  breakpoints$end <- as.character(breakpoints$end)

  breakpoints <<- breakpoints
}

# +++++ Count number of observations in quantiles
f_qcount <- function(candidate, reference, breakpoints, station, qmapping) {
  # input: candidate (array: date, hs), reference(array: date, hs)
  # breakpoints (character, start, end): if only one breakpoint, than start of series (1895. correct later!)
  # station: station-id (1800)
  # output: homogenized series and corr-factor
  # qmapping: vector with quantiles (c(0, .25, .50, .75, .95, 1))

  # +++++ make sure everything is named right
  colnames(candidate) <- c("date", "candidate")
  candidate$date <- as.Date(candidate$date)
  colnames(reference) <- c("date", "reference")
  reference$date <- as.Date(reference$date)

  # +++++ set 0 to NA because of median-calculation. store and refill after homogenization
  c_zero <- which(candidate$candidate == 0)
  candidate$candidate[c_zero] <- NA
  r_zero <- which(reference$reference == 0)
  reference$reference[r_zero] <- NA

  # +++++ join series to get same dates
  data <- left_join(candidate, reference, by = "date")

  # +++++ create quantile count object
  qcount_can <- breakpoints
  qcount_can[, 3:c(2 + c(length(qmapping) - 1))] <- NA
  colnames(qcount_can)[3:c(2 + c(length(qmapping) - 1))] <- paste0(qmapping[2:length(qmapping)] * 100, "%")
  qcount_ref <- qcount_can


  # ++++ count
  qmap <- as.numeric(qmapping)
  q <- 2
  for (q in 2:c(length(qmap) - 1)) {
    qmap[q] <- qmap[q] - 0.01
  }
  qlength <- length(qmap) - 1

  b <- 1
  for (b in 1:c(nrow(breakpoints) - 1)) {
    # +++++ define time periods
    start <- which(data$date == breakpoints[b, 1]) # start correctionperiod
    end <- which(data$date == breakpoints[b, 2]) # end correctionperiod
    hom_start <- which(data$date == # start homogeneous period
      breakpoints[nrow(breakpoints), 1])
    hom_end <- which(data$date == # end homogenous period
      breakpoints[nrow(breakpoints), 2])

    q_can_a <- data$candidate[hom_start:hom_end] %>% # quantiles for candidate series after break
      quantile(na.rm = T, probs = qmap)

    q_can_b <- data$candidate[start:end] %>% # quantiles for candidate series before break
      quantile(na.rm = T, probs = qmap)

    q_ref_a <- data$reference[hom_start:hom_end] %>% # quantiles for reference series after break
      quantile(na.rm = T, probs = qmap)

    q_ref_b <- data$reference[start:end] %>% # quantiles for reference series before break
      quantile(na.rm = T, probs = qmap)

    c <- 1
    for (c in 1:qlength) {
      Ca <- data$candidate[hom_start:hom_end] %>% # Candidate after break
        .[. >= q_can_a[c] & . < q_can_a[c + 1]] %>% # select only values within quantile span
        .[!is.na(.)] %>%
        length()

      Cb <- data$candidate[start:end] %>% # Candidate before break
        .[. >= q_can_b[c] & . < q_can_b[c + 1]] %>% # select only values within quantile span
        .[!is.na(.)] %>%
        length()

      Ra <- data$reference[hom_start:hom_end] %>% # Reference after break
        .[. >= q_ref_a[c] & . < q_ref_a[c + 1]] %>% # select only values within quantile span
        .[!is.na(.)] %>%
        length()

      Rb <- data$reference[start:end] %>% # Reference before break
        .[. >= q_ref_b[c] & . < q_ref_b[c + 1]] %>% # select only values within quantile span
        .[!is.na(.)] %>%
        length()

      qcount_can[b, (2 + c)] <- Cb
      qcount_can[nrow(qcount_can), (2 + c)] <- Ca
      qcount_ref[b, (2 + c)] <- Rb
      qcount_ref[nrow(qcount_ref), (2 + c)] <- Ra
    }
    qcount_can <<- qcount_can # export to main R
    qcount_ref <<- qcount_ref
  }
}

# +++++ InterQM homogenization using daily values
f_InterpQM_daily <- function(candidate, reference, breakpoints, station, qmapping) {
  # input: candidate (array: date, hs), reference(array: date, hs)
  # breakpoints (character, start, end): if only one breakpoint, than start of series (1895. correct later!)
  # station: station-id (1800)
  # output: homogenized series and corr-factor
  # qmapping: vector with quantiles (c(0, .25, .50, .75, .95, 1))

  # +++++ make sure everything is named right
  colnames(candidate) <- c("date", "candidate")
  candidate$date <- as.Date(candidate$date)
  colnames(reference) <- c("date", "reference")
  reference$date <- as.Date(reference$date)

  # +++++ set 0 to NA because of median-calculation. store and refill after homogenization
  c_zero <- which(candidate$candidate == 0)
  candidate$candidate[c_zero] <- NA
  r_zero <- which(reference$reference == 0)
  reference$reference[r_zero] <- NA

  # +++++ join series to get same dates
  data <- left_join(candidate, reference, by = "date")

  # +++++ calculate and apply corrections
  data$qmap <- data$candidate

  # +++++ calculation of quantiles
  qmap <- qmapping # quantiles: 0th - 94th, 95th - 100th

  # qmap <- as.numeric(qmapping)
  q <- 2
  for (q in 2:c(length(qmap) - 1)) {
    qmap[q] <- qmap[q] - 0.01
  }
  qlength <- length(qmap) - 1

  b <- 1
  for (b in 1:c(nrow(breakpoints) - 1)) {
    # +++++ define time periods
    start <- which(data$date == breakpoints[b, 1]) # start correctionperiod
    end <- which(data$date == breakpoints[b, 2]) # end correctionperiod
    hom_start <- which(data$date == # start homogeneous period
      breakpoints[nrow(breakpoints), 1])
    hom_end <- which(data$date == # end homogenous period
      breakpoints[nrow(breakpoints), 2])

    q_can_a <- data$candidate[hom_start:hom_end] %>% # quantiles for candidate series after break
      quantile(na.rm = T, probs = qmap)

    q_can_b <- data$candidate[start:end] %>% # quantiles for candidate series before break
      quantile(na.rm = T, probs = qmap)

    q_ref_a <- data$reference[hom_start:hom_end] %>% # quantiles for reference series after break
      quantile(na.rm = T, probs = qmap)

    q_ref_b <- data$reference[start:end] %>% # quantiles for reference series before break
      quantile(na.rm = T, probs = qmap)

    # +++++ calculate values for correction formula
    corr_factor_qmap <- array(NA, c(length(station), qlength))
    rownames(corr_factor_qmap) <- station
    colnames(corr_factor_qmap) <- names(q_can_a)[-length(q_can_a)]

    c <- 1
    for (c in 1:c(qlength - 1)) {
      Ca <- data$candidate[hom_start:hom_end] %>% # Candidate after break
        .[. >= q_can_a[c] & . < q_can_a[c + 1]] %>% # select only values within quantile span
        .[!is.na(.)] %>%
        as.numeric() %>%
        median(na.rm = T)

      Cb <- data$candidate[start:end] %>% # Candidate before break
        .[. >= q_can_b[c] & . < q_can_b[c + 1]] %>% # select only values within quantile span
        .[!is.na(.)] %>%
        as.numeric() %>%
        median(na.rm = T)

      Ra <- data$reference[hom_start:hom_end] %>% # Reference after break
        .[. >= q_ref_a[c] & . < q_ref_a[c + 1]] %>% # select only values within quantile span
        .[!is.na(.)] %>%
        as.numeric() %>%
        median(na.rm = T)

      Rb <- data$reference[start:end] %>% # Reference before break
        .[. >= q_ref_b[c] & . < q_ref_b[c + 1]] %>% # select only values within quantile span
        .[!is.na(.)] %>%
        as.numeric() %>%
        median(na.rm = T)

      corr_factor_qmap[c] <- (Ca / Ra) / (Cb / Rb) # calculate corr_factor
    }

    # +++++ last quantile subset
    c <- qlength
    Ca <- data$candidate[hom_start:hom_end] %>% # Candidate after break
      .[. > q_can_a[c] & . <= q_can_a[c + 1]] %>% # select only values within quantile span
      .[!is.na(.)] %>%
      as.numeric() %>%
      median(na.rm = T)

    Cb <- data$candidate[start:end] %>% # Candidate before break
      .[. > q_can_b[c] & . <= q_can_b[c + 1]] %>% # select only values within quantile span
      .[!is.na(.)] %>%
      as.numeric() %>%
      median(na.rm = T)

    Ra <- data$reference[hom_start:hom_end] %>% # Reference after break
      .[. > q_ref_a[c] & . <= q_ref_a[c + 1]] %>% # select only values within quantile span
      .[!is.na(.)] %>%
      as.numeric() %>%
      median(na.rm = T)

    Rb <- data$reference[start:end] %>% # Reference before break
      .[. > q_ref_b[c] & . <= q_ref_b[c + 1]] %>% # select only values within quantile span
      .[!is.na(.)] %>%
      as.numeric() %>%
      median(na.rm = T)

    corr_factor_qmap[c] <- (Ca / Ra) / (Cb / Rb) # calculate correction-factor


    # +++++ calculate linear interpolation and adapt quantiles
    # make sure if > 100 unique observations are there. if yes: quantile mapping. if no: value-wise-mapping
    qmapping_lin <- rep(NA, length(qmap) + 1) # adapted quantile subsets
    qmapping_lin[c(1, length(qmapping_lin))] <- c(0, 1)

    c <- 2
    for (c in 2:length(qmap)) {
      qmapping_lin[c] <- round(
        ((qmap[c] - qmap[c - 1]) / 2) +
          qmap[c - 1], 2
      )
    }

    lc <- data$candidate[start:end] %>%
      .[!is.na(.)] %>%
      .[. != 0] %>%
      unique() %>%
      sort()

    if (length(lc) < 100) {
      qmapping_lin <- round((qmapping_lin * 100 / 100) * length(lc)) # shrink to smaller area if necessary
      corr_factor_qmap_lin <- rep(NA, length(lc))
      names(corr_factor_qmap_lin) <- seq(1, length(lc))

      # +++++ fill vector with values
      corr_factor_qmap_lin[1:c(qmapping_lin[2])] <- corr_factor_qmap[1] # fixed first
      sel <- c(
        qmapping_lin[c(length(qmapping_lin) - 1)],
        last(qmapping_lin)
      )
      corr_factor_qmap_lin[sel[1]:sel[2]] <- corr_factor_qmap[length(corr_factor_qmap)] # fixed last

      c <- 2
      for (c in 2:length(corr_factor_qmap)) {
        q_start <- c(qmapping_lin[c])
        q_end <- c(qmapping_lin[c + 1])
        corr_factor_qmap_lin[q_start:q_end] <-
          seq(corr_factor_qmap[c - 1], corr_factor_qmap[c], length = c(q_end - q_start + 1))
      }
    } else {
      corr_factor_qmap_lin <- rep(NA, 100)
      names(corr_factor_qmap_lin) <- seq(1, 100)

      # +++++ fill vector with values
      corr_factor_qmap_lin[1:c(qmapping_lin[2] * 100)] <- corr_factor_qmap[1] # fixed first
      sel <- c(
        qmapping_lin[c(length(qmapping_lin) - 1)],
        last(qmapping_lin)
      ) * 100
      corr_factor_qmap_lin[sel[1]:sel[2]] <- corr_factor_qmap[length(corr_factor_qmap)] # fixed last

      c <- 2
      for (c in 2:length(corr_factor_qmap)) {
        q_start <- c(qmapping_lin[c]) * 100
        q_end <- c(qmapping_lin[c + 1]) * 100
        corr_factor_qmap_lin[q_start:q_end] <-
          seq(corr_factor_qmap[c - 1], corr_factor_qmap[c], length = c(q_end - q_start + 1))
      }
    }

    # +++++ apply corrections
    candidate_find <- data$candidate

    data_sort <- seq(min(candidate_find[start:end], na.rm = T),
      max(candidate_find[start:end], na.rm = T),
      length.out = length(corr_factor_qmap_lin)
    )

    # first to secondlast percentile
    c <- 1
    for (c in 1:c(length(corr_factor_qmap_lin) - 2)) {
      sel <- which(candidate_find[start:end] >= data_sort[c] &
        candidate_find[start:end] < data_sort[c + 1])

      data$qmap[start:end][sel] <- data$candidate[start:end][sel] * corr_factor_qmap_lin[c]
    }

    # last percentile
    c <- length(corr_factor_qmap_lin) - 1
    sel <- which(candidate_find[start:end] >= data_sort[c] &
      candidate_find[start:end] <= data_sort[c + 1])
    data$qmap[start:end][sel] <- data$candidate[start:end][sel] * corr_factor_qmap_lin[c]
  }

  # +++++ take care of falsely corrected days with low snow (good for physical consistency, no days with snow can get lost)
  data$qmap[data$qmap < 1] <- 1

  # +++++ re-apply zeros
  data$qmap[c_zero] <- 0
  data$candidate[c_zero] <- 0
  data$reference[r_zero] <- 0

  # +++++ export to main environment
  qmap_output <- vector(mode = "list", length = 1)
  qmap_output[[1]] <- data
  names(qmap_output) <- station

  output_qmap <<- qmap_output
  corr_factor_qmap <<- corr_factor_qmap

  # make sure there are 100 values on corr_factor_qmap_lin
  if (length(corr_factor_qmap_lin < 100)) {
    corrlength <- length(corr_factor_qmap_lin)
    test <- rep(NA, 100)
    test[1:corrlength] <- corr_factor_qmap_lin
    test[corrlength:100] <- corr_factor_qmap_lin[length(corr_factor_qmap_lin)]

    corr_factor_qmap_lin <- test
  }
  corr_factor_qmap_lin <<- corr_factor_qmap_lin
}

f_corr_factor <- function(corr_factor, corr_factor_lin, names_qmap, corr_factor_qmap, qmapping) {
  # interpolate corr_factor to corr_factor_lin ()
  corr_factor[l, 2:c(1 + length(names_qmap))] <<- corr_factor_qmap # write corrfactor to gathering object

  qm_lin <- rep(NA, length(qmapping) + 1) # adapted quantile subsets
  qm_lin[c(1, length(qm_lin))] <- c(0, 1)

  c <- 2
  for (c in 2:length(qmapping)) {
    qm_lin[c] <- round(
      ((qmapping[c] - qmapping[c - 1]) / 2) +
        qmapping[c - 1], 2
    )
  }

  corr_factor_export <- rep(NA, 100)
  names(corr_factor_export) <- seq(1, 100)

  # +++++ fill vector with values
  corr_factor_export[1:c(qm_lin[2] * 100)] <- corr_factor_qmap[1] # fixed first
  sel <- c(
    qm_lin[c(length(qm_lin) - 1)],
    last(qm_lin)
  ) * 100
  corr_factor_export[sel[1]:sel[2]] <- corr_factor_qmap[length(corr_factor_qmap)] # fixed last

  c <- 2
  for (c in 2:length(corr_factor_qmap)) {
    q_start <- c(qm_lin[c]) * 100
    q_end <- c(qm_lin[c + 1]) * 100
    corr_factor_export[q_start:q_end] <-
      seq(corr_factor_qmap[c - 1], corr_factor_qmap[c], length = c(q_end - q_start + 1))
  }

  corr_factor_lin[l, 2:c(length(corr_factor_export) + 1)] <<- corr_factor_export # write corr_factor to gathering object
}
