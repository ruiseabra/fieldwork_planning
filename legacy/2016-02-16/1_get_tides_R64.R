# this code DOES NOT run in RStudio, must use R ide
###################################################-

library(foreach)
library(xts)

Sys.setenv(TZ = "UTC")

setwd("~/Documents/WORK/PhD/FIELDWORK_N_TRIPS/_planning/fetch_tides/")

rm(list=ls())

#height tollerance (diff of future tides relative to previous mean tide height, in cm)
tol <- 0.15

start.work <- "05:00"
end.work   <- "23:00"

#set dates
start.date <- "2013-05-01 00:00"
end.date	 <- "2013-06-01 00:00"

time <- list()
time$range <- as.POSIXct(c(start.date, end.date), tz="UTC")
time$work <- c(start.work, end.work)
  
INFO <- list(time=time, tol=tol)

#read previous fieltrips' dates
previous <- read.csv("robolimpet_servicing_dates.csv", sep=";", stringsAsFactors=F)
previous.sh <- previous[, 1]
previous <- previous[, 4:ncol(previous)]
previous <- lapply(split(previous, 1:nrow(previous)), as.character)
names(previous) <- previous.sh
with.data <- lapply(lapply(previous, nchar), function(x) which(x == 10))
previous <- foreach(i = 1:length(previous)) %do% previous[[i]][with.data[[i]]]
names(previous) <- previous.sh
previous.1 <- lapply(previous, paste, start.work)
previous.2 <- lapply(previous, paste, end.work)

#read tidestation data
shores <- read.csv("tide_stations.csv", sep=";")
shores$tidestation <- gsub("%", "\'", shores$tidestation)

#list shores per filedtrip
fieldtrips <- list(pts = c("Ev", "Al", "Sl"), ptn = c("Mi", "Mo"), sp = c("To", "Lc", "Sv", "Bi"), fr = c("Rn", "Cc", "La", "We"), ukie = c("Sc", "An", "Mc", "Em"))
fieldtrips.short <- unlist(fieldtrips)
names(fieldtrips.short) <- fieldtrips.short

#get the tide stations for each shore
tidestations <- sapply(fieldtrips.short, function(x) shores$tidestation[shores$sh == x][1])

#list shores for previous tides
previous.stations <- foreach(i = 1:length(tidestations)) %do% rep(tidestations[i], max(with.data[[i]]))

#build the system call to get tides for each shore
tidestations <- sapply(tidestations, function(x) paste0("tide -z y -tf \"%H:%M\" -em pSsMm -l \"", x, "\" -b \"", start.date, "\" -e \"", end.date, "\""))

#build the system call to get historic tide heights for each shore
previous <- foreach(i = 1:length(previous.stations)) %do% paste0("tide -z y -tf \"%H:%M\" -em pSsMm -l \"", previous.stations[[i]], "\" -b \"", previous.1[[i]], "\" -e \"", previous.2[[i]], "\"")

#get previous tides for each shore using 'Xtides'
heights <- foreach(i = seq(previous)) %do% sapply(previous[[i]], function(x) system(x, intern=T))
names(heights) <- previous.sh

#clean data (some entries come as list, others as matrix)
which.matrix <- which(lapply(heights, class) == "matrix")
heights2 <- list()
for(i in seq(heights)){
	mat <- i %in% which.matrix
	tmp <- heights[[i]]
	if(mat){
		tmp <- tmp[-(1:3), ]
		tmp <- tmp[grep("Low", tmp)]
		tmp.dates <- as.POSIXct(substr(tmp, start=1, stop=16), tz="UTC")
		tmp.heights <- as.numeric(substr(tmp, start=20, stop=23))
	}else{
		tmp <- sapply(tmp, function(x) x[-(1:3)])
		names(tmp) <- 1:length(tmp)
		tmp.low <- sapply(tmp, grep, pattern="Low")
		tmp <- foreach(j = 1:length(tmp)) %do% tmp[[j]][tmp.low[[j]]]
		tmp.dates <- as.POSIXct(unlist(sapply(tmp, substr, start=1, stop=16)), tz="UTC")
		tmp.heights <- as.numeric(unlist(sapply(tmp, substr, start=20, stop=23)))
	}
	same.day <- which(diff(tmp.dates) < 24)
	if(length(same.day)){
		tmp.dates <- tmp.dates[-(same.day+1)]
		tmp.heights <- tmp.heights[-(same.day+1)]
	}
	tmp <- zoo(tmp.heights, tmp.dates)
	heights2[[i]] <- tmp
}
names(heights2) <- previous.sh
heights <- heights2	

#compute average previous tide heights
#height.av <- lapply(lapply(heights, mean), round, 2)
height.av1 <- lapply(lapply(lapply(heights, quantile), round, 2), "[[", 2)
height.av2 <- lapply(lapply(lapply(heights, quantile), round, 2), "[[", 3)
	
#get future tides for each shore using 'Xtides'
tides <- sapply(tidestations, function(x) system(x, intern=T))
tides <- sapply(tides, function(x) x[-(1:3)])

#find indexes for low tides, and select only the low tide entries 
lows <- sapply(tides, grep, pattern="Low")
lows <- foreach(i = 1:length(tides)) %do% tides[[i]][lows[[i]]]
names(lows) <- names(tides)

#extract dates and heights for each low tide
dates <- sapply(lows, function(x) as.POSIXct(substr(x, 1, 16), tz="UTC"))
heights <- sapply(lows, function(x) as.numeric(substr(x, 20, 23)))

#select tides during working hours
hours <- sapply(dates, function(x) as.POSIXct(paste("1970-01-01", substr(x, 12, 16)), tz="UTC"))
full.start <- as.POSIXct(paste("1970-01-01", start.work), tz="UTC")
full.end <- as.POSIXct(paste("1970-01-01", end.work), tz="UTC")
within.work <- sapply(hours, function(x) which(x > full.start & x < full.end))
dates <- foreach(i = 1:length(dates)) %do% dates[[i]][within.work[[i]]]
heights <- foreach(i = 1:length(heights)) %do% heights[[i]][within.work[[i]]]

#select tides bellow the respective previous mean tide height (+ tollerance)
###best
within.height <- foreach(i = 1:length(heights)) %do% which(heights[[i]] <= height.av1[[i]]+tol)
dates.best <- foreach(i = 1:length(dates)) %do% dates[[i]][within.height[[i]]]
heights.best <- foreach(i = 1:length(heights)) %do% heights[[i]][within.height[[i]]]
###ok
within.height <- foreach(i = 1:length(heights)) %do% which(heights[[i]] > height.av1[[i]]+tol & heights[[i]] <= height.av2[[i]]+tol)
dates.ok <- foreach(i = 1:length(dates)) %do% dates[[i]][within.height[[i]]]
heights.ok <- foreach(i = 1:length(heights)) %do% heights[[i]][within.height[[i]]]

#save data
previous.tides <- list(quant1=height.av1, quant2=height.av2, tides=heights2)
rm(list=ls()[which(!ls() %in% c("previous.tides", "heights.best", "heights.ok", "dates.best", "dates.ok", "previous.sh", "INFO"))])
save.image()