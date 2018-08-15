########################-
## collect tide data ###-
########################-
# xy = list(lon = lon, lat = lat)
# t_range = POSIXct date:time range (2 values)
# t_res = every 't_res' seconds
########################-
fes.tides <- function(lat, lon, t_range, t_res) {
  Sys.setenv(HDF5_DISABLE_VERSION_CHECK = "2")
  # prepare
  ORIGIN  <- ymd("1950-01-01")
  T_RANGE <- (t_range + c(0, t_res)) %>%
    julian(origin = ORIGIN) %>%
    as.numeric %>%
    formatC(format = "f")
  T_RES <- t_res / 60
  CALL  <- str_c("fes_slev", lat, lon, T_RANGE[1], T_RANGE[2], T_RES, sep = " ")
  # run fes_slev
  tides <- system(CALL, intern = TRUE)[-(1:2)] %>%
    str_split(",") 
  # extract timestamps
  times <- map_chr(tides, 1) %>% 
    as.numeric %>% 
    "*"(., (24 * 3600)) %>% 
    round
  times <- (times - (times %% 60)) %>% 
    as.POSIXct(origin = ORIGIN)
  times <- times - (as.numeric(times) %% 60)
  steady <- diff(times) %>%
    unique %>%
    length %>%
    "=="(., 1)
  if (!steady) stop("the period of 'times' is irregular")
  # extract tide elevation
  tides <- map_chr(tides, 2) %>% as.numeric
  # combine
  tides <- tibble(time = times, tide = tides)
  # filter to ensure that the data return does not exceed the t_range supplied
  tides <- filter(tides, time %within% interval(t_range[1], t_range[2]))
  # return
  tides
}
########################-
########################-
########################-

########################-
## collect sunrise and sunset data ###-
########################-
# xy = list(lon = lon, lat = lat)
# t_range = POSIXct date:time range (2 values)
########################-
rise.set <- function(lat, lon, t_range) {
  LOC  <- SpatialPoints(cbind(lon, lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
  TSEQ <- seq.POSIXt(first(t_range), last(t_range), by = "days")
  RISE <- sunriset(LOC, TSEQ, "sunrise", POSIXct.out = TRUE)[,2]
  SET  <- sunriset(LOC, TSEQ, "sunset",  POSIXct.out = TRUE)[,2]

  tibble(day = as.Date(RISE), rise = RISE, set = SET)
} 
########################-
########################-
########################-

########################-
## gather tide and sun data ###-
########################-
# xy = list(lon = lon, lat = lat)
# t_range = POSIXct date:time range (2 values)
# t_res = every 't_res' seconds
########################-
tide.and.sun <- function(lat, lon, t_range, t_res) {
  TID <- fes.tides(lat, lon, t_range, t_res)
  SUN <- rise.set( lat, lon, t_range)
  
  ind <- match(as_date(TID$time), SUN$day)
  TID$rise <- SUN$rise[ind]
  TID$set  <- SUN$set[ ind]
  
  TID$light <- TID$time >= TID$rise & TID$time <= TID$set
  
  TID$low   <- c(FALSE, FALSE, rollapply(TID$tide, 5, function(x) which.min(x) == 3), FALSE, FALSE)
  
  STAT <- filter(TID, low) %>% summarise(q05 = quantile(tide, 0.05), q25 = quantile(tide, 0.25), avg = mean(tide))
  
  x <- list(tide = select(TID, -rise, -set), sun = SUN, stats = STAT)
  x
}
########################-
########################-
########################-
