library(xts)
library(maptools)
library(tidyverse)
library(stringr)
library(lubridate)
rm(list=ls())
Sys.setenv(TZ = "UTC")

sites <- data.frame(
  sh=c("Ev", "Al", "Sl", "Mi", "Mo", "To", "Lc", "Sv", "Bi", "Rn", "Cc", "La", "We", "Mc", "An", "Em", "Sc"),
  shore=c("Evaristo", "Alteirinhos", "SLourenco", "Mindelo", "Moledo", "Tourinan", "LaCaridad", "SVBarquera", "Biarritz", "Royan", "LeCroisic", "Landunvez", "Wembury", "MCastle", "Anglesey", "Emlagh", "SCairn"),
  lat=c(37.0742, 37.5192, 39.0135, 41.3104, 41.8399, 43.0429, 43.5654, 43.3936, 43.4844, 45.6256, 47.2899, 48.5431, 50.3134, 52.1249, 53.3194, 53.7511, 54.9721),
  lon=c(-8.3037, -8.8126, -9.4224, -8.7424, -8.8751, -9.2900, -6.8281, -4.4403, -1.5632, -1.0626, -2.5424, -4.7514, -4.1071, -10.1100, -4.6616, -9.9067, -5.1796), stringsAsFactors=F)

# date range
year0 <- 2010 - 3
year1 <- as.numeric(substr(Sys.Date(), 1, 4)) + 5 # 5 years in order to improve the establishement of the mean lowtide height
dates <- as.POSIXct(c(paste0(year0, '-01-01 00:00'), paste0(year1, '-12-31 23:59')))

# collect dates of previous fieldworks
load("~/Dropbox/BIO_shared/EuropeanScale/logger_info/robolimpet_servicing_dates.RData")
prev_tides <- lapply(x$all, rev)
names(prev_tides) <- sites$sh[match(names(prev_tides), tolower(sites$shore))]
prev_tides <- prev_tides[match(sites$sh, names(prev_tides))]

# collect tide data, and organise
allSites <- paste(sites$sh, collapse=' ')
source('functions.R')
tides <- list()
for(s in 1:nrow(sites))
{
  #s <- 1
  
  thisSite <- gsub(sites$sh[s], '00', allSites)
  thisSite <- gsub('[^0-9]',  ' ', thisSite)
  thisSite <- gsub('00', sites$sh[s], thisSite)
  print(allSites)
  
  x <- get.fes.low.tides(sites$lat[s], sites$lon[s], dates, 20) / 100
  if(any(x > 0)) stop()
  #x <- x[x < 0]
  y <- get.rise.set(sites$lat[s], sites$lon[s], as.Date(index(x)), 0)
  z <- with.light(x, y$rise, y$set, 30)
  x <- xts(data.frame(tide = coredata(x), z), index(x))
  
  info <- list()
  info$sh  <- as.character(sites$sh[s])
  info$lat <- sites$lat[s]
  info$lon <- sites$lon[s]
  info$rise <- y$rise
  info$set <- y$set
  info$avg <- round(mean(x$tide), 2)
  info$q25 <- as.numeric(round(quantile(x$tide, 0.25), 2))
  info$q50 <- as.numeric(round(quantile(x$tide, 0.50), 2))
  info$hlf <- round(min(x$tide) + 0.5, 2) # 0.5 cm above the lowest tide in the tide range
  
  # previous tides
  prev <- x[as.character(prev_tides[[info$sh]])]
  prev <- split(prev, as.Date(index(prev)))
  for(p in 1:length(prev))
  {
    tmp <- prev[[p]]
    if(nrow(tmp) > 1)
    {
      # both night or all day: choose lowest tide
      if(sum(tmp[,2:3]) == 0 | sum(tmp[,2:3]) == 4) prev[[p]] <- tmp[which.min(tmp$tide)] 
      # sum == 1 means one tide during crepuscule, the other during night: choose the one during crepuscule
      if(sum(tmp[,2:3]) == 1) prev[[p]] <- tmp[which(tmp$crepuscule == 1)]
      # if sum == 2 and sum of light == 0 it means both tides occurred during crepuscule: choose lowest tide
      if(sum(tmp[,2:3]) == 2 & sum(tmp[,2]) == 0) prev[[p]] <- tmp[which.min(tmp$tide)] 
      # the only 2 cases left are one during day and one during night, and one during day and the other during crepuscule: in both cases, select the one during the day
      if((sum(tmp[,2:3]) == 2 & sum(tmp[,2]) != 0) | sum(tmp[,2:3]) == 3) prev[[p]] <- tmp[which(apply(tmp[,2:3], 1, sum) == 2)]
    }
  }
  info$prev <- xts(data.frame(height=sapply(prev, '[[', 1)), as.POSIXct(sapply(prev, index), origin='1970-01-01'))
  info$prev_avg <- round(mean(info$prev), 2)
  info$prev_lst <- round(as.numeric(tail(info$prev, 1)), 2)
  info$prev_bad <- round(quantile(info$prev, 0.75), 2)
  info$prev_wrst <- round(max(info$prev), 2)
  
  #x <- x[as.logical(x$crepuscule)]
  x$avg <- x$tide <= info$avg
  x$q25 <- x$tide <= info$q25
  x$q50 <- x$tide <= info$q50
  x$hlf <- x$tide <= info$hlf
  x$prev_avg <- x$tide <= info$prev_avg
  x$prev_lst <- x$tide <= info$prev_lst
  x$prev_bad <- x$tide <= info$prev_bad
  best <- (apply(x[,c('hlf', 'prev_avg')], 1, sum) >= 1)
  ok   <- (apply(x[,c('q25', 'prev_bad')], 1, sum) >= 1)
  x$best  <- x$light & best
  x$ok    <- x$light & !x$best & ok
  x$crep  <- !x$light & x$crepuscule & best
  x$other <- !(x$best | x$ok | x$crep)
  x$cols  <- ifelse(x$best, 1, ifelse(x$ok, 2, ifelse(x$crep, 3, ifelse(x$other, 4, NA))))
  if(any(is.na(x$cols))) stop()
  
  info$tides <- x
  
  tides[[sites$sh[s]]] <- info
  
  print(thisSite)
}

save(tides, file=paste0('lowtides_', year0, '_', year1, '.RData'))