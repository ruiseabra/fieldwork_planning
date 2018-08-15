#robolimpet resolution and sampling frequence (in fractions of hours)
res <- F # T = high, F = low
sampling <- 1
max.readings <- 8192

#make final zoos
tides.best <- foreach(i = 1:length(dates.best)) %do% xts(data.frame(height=heights.best[[i]]), dates.best[[i]])
names(tides.best) <- previous.sh

tides.ok <- foreach(i = 1:length(dates.ok)) %do% xts(data.frame(height=heights.ok[[i]]), dates.ok[[i]])
names(tides.ok) <- previous.sh

#save future tides
future.tides <- list(best=tides.best, ok=tides.ok)

#add height range to INFO
INFO$hrange <- range(unlist(future.tides))

#get date of memory end
time.left <- (max.readings/(1+res))*(3600*sampling)
INFO$time$last <- lapply(lapply(lapply(previous.tides$tides, tail, 1), index), function(x) x+time.left)

#save data
rm(list=ls()[which(!ls() %in% c("previous.tides", "future.tides", "INFO"))])
save.image()