library(tidyverse)
library(xts)
library(stringr)

rm(list=ls())
Sys.setenv(TZ = "UTC")
source('functions.R')

tides <- tibble(
  shore   = c("Easky", "Roonagh", "Spiddal", "Kilkee", "Smerwick_Harbour", "Murphys_Cove", "Viana_do_Castelo"),
  country = c(rep("Ireland", 6), "Portugal"),
  lat     = c(54.292652, 53.762524, 53.242921, 52.684082, 52.186683, 51.487872, 41.69943),
  lon     = c(-8.956733, -9.905105, -9.301441, -9.651712, -10.512499, -9.264645, -8.854797))

# date range
d <- list(
  year0 = 2000,
  year1 = 2030)
d$dates = as.POSIXct(c(str_c(d$year0, '-01-01 00:00'), str_c(d$year1, '-12-31 23:59')))

# collect tide data
cat("--> COLLECTING TIDE DATA FOR", nrow(tides), "SITES\n")
cat("----> collecting low tide times and heights\n")
x <- list()
for(i in 1:nrow(tides)) {
  cat("         ", tides$shore[i], "\n")
  x[[i]] <- get.fes.low.tides(tides$lat[i], tides$lon[i], d$dates, 20)
}
tides <- add_column(tides, low_tides = x)

save(tides, file = str_c("extracted_lowtides_", d$year0, "_", d$year1, ".RData"))
