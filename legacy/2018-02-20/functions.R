get.fes.low.tides <- function(lat, lon, dates, freq) {
  # WARNING - this function is not built to be completely accurate!!!!!
  # when using lower "freq" values time steps asked and returned may shift by one towards the end of the time span asked
  # this is dealt here in a very simplistic way
  tspan <- dates %>% 
    str_sub(1, 14) %>%
    str_c("00:00") %>%
    as.POSIXct
  times <- seq.POSIXt(tspan[1], tspan[2], freq * 60)
  tspan <- tspan %>%
    as.POSIXct %>%
    julian(origin = as.Date("1950-01-01")) %>%
    as.numeric %>%
    formatC(format = "f")
  
  call <- str_c("export HDF5_DISABLE_VERSION_CHECK=2\nfes_slev", lat, lon, tspan[1], tspan[2], freq, sep = " ")
  # the expression "export HDF5_DISABLE_VERSION_CHECK=2\n" is inserted into the call because RS mac mini has a mismatch between the library and the header files; setting this in the system environmental variables does not work because R opens its own shell; the version mismatch does not appear to affect the tide data, but the gathering of tide data should, in the end, be run in another machine where this issue is not present
  
  tid  <- system(call, intern = TRUE)[-(1:2)] %>%
    str_split(",")
  
  # this is the rough hack to make the times match
  l0 <- length(tid)
  l1 <- length(times)
  if(l0 < l1) times <- times[1:l0]
  if(l1 < l0) tid   <- tid[1:l1]
  
  tid <- tid %>%
    sapply("[[", 2) %>%
    as.numeric %>%
    xts(times)
  
  flag_lo <- rollapply(zoo(tid), 5, function(x) which.min(x) == 3, fill = FALSE)
  
  tid <- tid[flag_lo]
  colnames(tid) <- "tide"
  
  tid
}


# find sunrise and sunset times for the work period
get.rise.set <- function(lat, lon, dates, extend=0)
{
  location <- SpatialPoints(matrix(c(lon, lat), nrow=1), proj4string=CRS("+proj=longlat +datum=WGS84"))
  if(length(dates) != 2) dates <- as.POSIXct(dates)
  if(length(dates) == 2) dates <- seq.POSIXt(as.POSIXct(dates[1]), as.POSIXct(dates[2]), by="days")
  if(extend == 0)
  {
    rises <- sunriset(location, dates, "sunrise", POSIXct.out=T)[,2]
    sets  <- sunriset(location, dates, "sunset", POSIXct.out=T)[,2]
  }
  if(extend == 1)
  {
    # Astronomical dawn (solarDep=18), Nautical dawn (solarDep=12), Civil dawn (solarDep=6)
    # at higher latitudes solarDep > 10 will result in crepuscules in a different day, thus outputing NA and crashing the script
    rises <- crepuscule(location, dates, solarDep=6, "dawn", POSIXct.out=T)[,2]
    sets  <- crepuscule(location, dates, solarDep=6, "dusk", POSIXct.out=T)[,2]
  }
  if(extend == 2)
  {
    # from 00:00 to 23:59
    rises <- dates
    sets  <- dates + 23*3600 + 3599
  }  
  data.frame(rise=rises, set=sets, date=as.Date(rises))
} 

with.light <- function(x, r, s, buffer.mins = 0)
{
  if(buffer.mins < 0) stop('buffer.mins must be positive')
  if(any(class(x) %in% c('zoo', 'xts'))) x <- index(x)
  
  light <- c()
  crepuscule <- c()
  for(i in 1:length(x))
  {
    #i <- 1
    this_rise <- r[i]
    this_set  <- s[i]
    light <- c(light, x[i] >= this_rise & x[i] <= this_set)
    
    this_rise <- r[i] - (buffer.mins * 60)
    this_set  <- s[i] + (buffer.mins * 60)
    crepuscule <- c(crepuscule, x[i] >= this_rise & x[i] <= this_set)
  }
  if(buffer.mins > 0) data.frame(light=light, crepuscule=crepuscule) else light
}

