library(xts)
library(maptools)
library(tidyverse)
library(stringr)
library(lubridate)

rm(list=ls())
Sys.setenv(TZ = "UTC")
source('other/functions.R')

# date range
d <- list(
  year0 = 2010 - 3,
  year1 = as.numeric(str_sub(Sys.Date(), 1, 4)) + 5) 
# add 5 years in order to improve the establishement of the mean lowtide height

d$dates = as.POSIXct(c(str_c(d$year0, '-01-01 00:00'), str_c(d$year1, '-12-31 23:59')))

# collect dates of previous fieldworks
load("~/Dropbox/RS/BIO_shared/EuropeanScale/logger_info/robolimpet_servicing_dates.RData")
colnames(x) <- c("sh", "shore", tail(colnames(x), -2))
tides <- x

# collect tide and daylight data, and organise
cat("--> COLLECTING TIDE AND DAYLIGHT DATA FOR", nrow(tides), "SITES\n")
cat("----> collecting low tide times and heights\n          this step takes a long time to complete...\n")
tides <- add_column(tides, low_tides = map2(tides$lat, tides$lon, ~get.fes.low.tides(.x, .y, d$dates, 20)))

cat("----> collecting sunrise and sunset times\n")
tides <- add_column(tides, sun = map2(map2(tides$lon, tides$lat, c), map(tides$low_tides, ~.x$time %>% as.Date), ~get.rise.set(.x, .y, FALSE)))
tides$low_tides <- map2(tides$low_tides, tides$sun, ~with.light(.x, .y, 30))

tides$avg <- map_dbl(tides$low_tides, ~.x$height %>% mean %>% round(2))
tides$q25 <- map_dbl(tides$low_tides, ~.x$height %>% quantile(0.25) %>% round(2))
tides$q50 <- map_dbl(tides$low_tides, ~.x$height %>% quantile(0.50) %>% round(2))
tides$hlf <- map_dbl(tides$low_tides, ~.x$height %>% min %>% "+"(0.5) %>% round(2)) # 0.5 cm above the lowest tide in the tide range

cat("----> identifying tide height during past fieldwork\n")
for(s in 1:nrow(tides)) {
  prev   <- tibble(day = tides$servDates[[s]] %>% as.Date, time = tides$servDates[[s]], height = 0)
  stides <- tides$low_tides[[s]]
  
  prev2 <- filter(stides, as.Date(time) %in% prev$day)
  for(i in 1:nrow(prev)) {
    DAY <- prev$day[i]
    tmp <- filter(prev2, day == DAY)
    if(nrow(tmp) > 1) {
      val <- sum(tmp$light)
      # only one tide during day, choose that one
      if(val == 1) tmp <- tmp[tmp$light,]
      # both tides during day, choose the lowest
      if(val == 2) tmp <- tmp[which.min(tmp$height),]
      
      # both tides not during the day, check if any during crepuscule
      if(!val) {
        val <- sum(tmp$crepuscule)
        # only one tide during crepuscule, choose that one
        if(val == 1) tmp <- tmp[tmp$crepuscule,]
        # both tides during crepuscule, choose the lowest
        if(val == 2) tmp <- tmp[which.min(tmp$height),]
        # both tides during the night, choose the lowest
        if(!val)     tmp <- tmp[which.min(tmp$height),]
      }
    }
    prev$time[i]   <- tmp$time
    prev$height[i] <- tmp$height
  }
  tides$servDates[[s]] <- prev
}
tides$prev_avg  <- map_dbl(tides$servDates, ~.x$height %>% mean %>% round(2))
tides$prev_lst  <- map_dbl(tides$low_tides, ~.x$height %>% last %>% round(2))
tides$prev_bad  <- map_dbl(tides$low_tides, ~.x$height %>% quantile(0.75) %>% round(2))
tides$prev_wrst <- map_dbl(tides$low_tides, ~.x$height %>% max %>% round(2))

cat("----> adding more info\n")
for(s in 1:nrow(tides)) {
  x <- tides$low_tides[[s]]
  
  # if there are NO low shore loggers left in this shore, allow for an extra 50 cm of tide in the classification of "best" and "ok" tides
  has_lows <- ifelse(tides$has_lows[s], 0, -0.5)
  
  x <- x %>% mutate(
    is_avg = (x$height + has_lows) <= tides$avg[s],
    is_q25 = (x$height + has_lows) <= tides$q25[s],
    is_q50 = (x$height + has_lows) <= tides$q50[s],
    is_hlf = (x$height + has_lows) <= tides$hlf[s],
    
    is_prev_avg = x$height <= tides$prev_avg[s],
    is_prev_lst = x$height <= tides$prev_lst[s],
    is_prev_bad = x$height <= tides$prev_bad[s])
  
  x <- mutate(x, best = ((x$is_hlf + x$is_prev_avg) >= 1) & x$light)
  x <- mutate(x, ok   = ((x$is_q25 + x$is_prev_bad) >= 1) & x$light & !x$best)
  
  x <- mutate(x, crep  = ((x$is_hlf + x$is_prev_avg) >= 1) & !x$light & x$crepuscule)
  x <- mutate(x, other = !(x$best | x$ok | x$crep))
  x <- mutate(x, cols  = ifelse(x$best, 1, ifelse(x$ok, 2, ifelse(x$crep, 3, ifelse(x$other, 4, NA)))))
  if(any(is.na(x$cols))) stop()
  
  tides$low_tides[[s]] <- x
}

save(tides, file = str_c("other/lowtides_", d$year0, "_", d$year1, ".RData"))
