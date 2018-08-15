res          <- 0    # 1 = high, 0 = low
sampling     <- 1    # fractions of hours, i.e., half hour = 0.5, 10 mins = 1/6
max_readings <- 8192 # maximum number of readings at low res
# in seconds (-1 day, to make it a little bit conservative)
lifeSpan   <- (max_readings / (1 + res)) * (3600 * sampling) - (3600 * 24)
x$endDates <- x$last + lifeSpan

sh    <- x$sh
shore <- x$shore

info <- tibble(
  sh       = sh, 
  yes      = FALSE, 
  workDate = as.POSIXct(origin),
  valiDate = FALSE,
  tide     = 1)
