Sys.setenv(TZ = "UTC") 
library(tidyverse)
load(lowtides)

# choose a target shore using the short version of shore name (column "sh")
print(tides)
target <- "mo"

# tides here are recorded as height difference to mean water level
# tide tables, however, are usually referenced to some version of a low low water level (depends on the standard chosen by each country, but typically is a few cm bellow the lowest tide for a given long period of time)
# thus, while tide heights in tide tables are always positive, fes low tides are always negative
# because of this you first need to find the correspondence between Viana 0.6 tides and the water level at each of these sites
# to do that you have to compile a list of dates corresponding to tides equal or below 0.6 in Viana, and then use those dates to get the average tide height corresponding to that benchmark
# only then you can check for tides equivalent to Viana 0.6

# below the code uses made up tide dates, you have to replace these dates with proper dates
VIANA.0.6 <- as.POSIXct(c("2007-01-01 19:30", "2007-01-03 08:40", "2007-01-04 21:40"))

# isolate tides for the target shore
x <- filter(tides, sh == target)
x <- x$low_tides[[1]] %>% select(time, height)

# grab tide height at dates closest to VIANA.0.6
heights <- vector(length = length(VIANA.0.6))
for(i in 1:length(heights)) {
  heights[i] <- x$height[which.min(abs(x$time - VIANA.0.6[i]))]
}
ref.height <- mean(heights)

# select tides equal or below ref.height
x <- x[x$height <= ref.height,]

# do what you want with the data
# either plot it ...
ggplot(x) + geom_line(aes(time, height))

# subset it ...
x2 <- filter(x, time >= as.POSIXct("2018-01-01") & time < as.POSIXct("2019-01-01"))
ggplot(x2) + geom_line(aes(time, height), col = "grey90") + geom_point(aes(time, height))

# and so on