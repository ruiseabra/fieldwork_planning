rm(list=ls())
library(foreach)
library(xts)
library(xlsx)
library(chron)
library(maps)
library(maptools)
library(sp)
Sys.setenv(TZ = "UTC")

########################################################-
## begin USER INPUT

# workable period color code (best on top)
# blue = best tides, during daytime
# black = best tides, during crepuscule
# green = ok tides, during daytime
# yellow = ok tides, during crepuscule
# red = best and ok tides, during nighttime

# choose the timespan for which tides will be retrieved
work_dates <- as.Date(c("2016-02-15", "2016-03-01"))

# returning to previous sites to service loggers?
SERVICING <- T

# plotting 1st evaluation (F) or final version, with fieldwork tides already chosen (T)? 
final <- F

# use standard sites or latlon
standardSITES <- T

# which shores to plot (either "all", or a vector of shores)
# plot_shores <- "all"
# plot_shores <- c("Em", "Sc", "Mc", "An", "We", "La", "Cc", "Rn", "Bi", "Sv", "Lc", "To", "Mo")
# plot_shores <- c("Em", "Sc", "Mc", "An", "We", "La", "Cc", "Rn", "Bi", "Sv", "Lc", "To", "Mo", "Sl", "Al", "Ev")
# plot_shores <- c("Em", "Mc", "An", "Sc")
# plot_shores <- c("Sc", "An", "Em", "Mc")
# plot_shores <- c("Bi", "Rn", "Cc", "We", "La")
# plot_shores <- c("Rn", "Cc", "We", "La")
# plot_shores <- c("We", "La", "Cc", "Rn", "Bi")
# plot_shores <- c("To", "Lc", "Sv", "Bi")
# plot_shores <- c("Lc", "Sv")
# plot_shores <- c("Al", "Ev")
# plot_shores <- c("Em", "Mc", "An", "Sc", "Al", "Ev")
# plot_shores <- c("Sl", "Al", "Ev")
# plot_shores <- c("Rn", "Cc", "La", "We")
# plot_shores <- c("Rn", "Bi")
 plot_shores <- "We"
# plot_shores <- c("Lc", "Sv", "Bi")
# plot_shores <- c("La", "Cc", "Rn")
 plot_shores <- c("Mo", "To")

# if "plot_shores <- NA" then provide a lat and lon 
## (date will be used only if SERVICING == TRUE and must de in the format 'YYYY-MM-DD HH')
## if available, provide the date ['YYYY-MM-DD'] of the previous tide worked at the location; otherwise use NA
latlon <- list(lat=38.5, lon=-28.8, name="azores", date="2015-07-04 09", prev=NA)

# if SERVICING == TRUE, edit 'fieldwork_dates' only up to the hour of the desired tide
fieldwork_dates <- c(
  Ev = "2015-02-22 10",
  Al = "2015-02-21 09",
  Sl = "2015-02-06 09",
  Mi = "2014-07-24 09",
  Mo = "2014-05-27 10",
  To = "2015-03-06 09",
  Lc = "2016-01-25 10",
  Sv = "2016-01-26 10",
  Bi = "2016-01-27 11",
  Rn = "2016-01-27 11",
  Cc = "2016-01-26 10",
  La = "2016-01-25 11",
  We = "2015-07-04 13",
  Mc = "2015-03-20 10",
  An = "2015-03-21 16",
  Em = "2015-03-19 10",
  Sc = "2015-03-22 18")

## end USER INPUT
########################################################-

# logger resolution and sampling frequence
# only important if SERVICING == TRUE
# these parameters are tunned for loggers based on ibuttons
res <- 0 # 1 = high, 0 = low
sampling <- 1 # fractions of hours, i.e., half hour = 0.5, 10 mins = 1/6
max_readings <- 8192 # maximum number of readings at low res

get.fes.tides <- function(lat, lon, dates, freq, just_lows=F)
{
  dates <- formatC(as.numeric(julian(c(dates, dates+3600), origin=as.Date("1950-01-01"))), format="f")
  # pasting PATH to the system call is necessary for Yosemite, and is redundant for all other OS
  call  <- paste0("PATH=", Sys.getenv("PATH"), paste(" gettide", lat, lon, dates[1], dates[2], freq))
  tides <- strsplit(system(call, intern=T)[-(1:2)], ",")
  times <- as.POSIXct(as.numeric(sapply(tides, "[[", 1))*24*3600, origin="1950-01-01")
  tides <- zoo(as.numeric(sapply(tides, "[[", 2)), times)
  if(just_lows) tides <- tides[which(rollapply(tides, 3, function(x) which.min(x) == 2))]
  tides
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

with.light <- function(MYdata, rise, set, go_dark=F, warn=F)
{
  dates_MYdata <- as.character(unique(as.Date(index(MYdata))))
  for(d in dates_MYdata)
  {
    indexes   <- which(as.Date(index(MYdata)) == d)
    this_rise <- rise[as.Date(rise) == d]
    this_set  <- set[as.Date(set) == d]
    in_light  <- index(MYdata[indexes]) >= this_rise & index(MYdata[indexes]) <= this_set
    if(all(!in_light))
    {
      if(go_dark)
      {
        r <- as.numeric(this_rise) - as.numeric(sort(index(MYdata[indexes]))[1])
        s <- as.numeric(sort(index(MYdata[indexes]))[2]) - as.numeric(this_set)
        MYdata <- MYdata[-indexes[which.max(c(r, s))]]
      }else{
        if(warn) warning(paste("data for", d, "was removed"))
        MYdata <- MYdata[-indexes]  	  	
      }
    }else{
      if(any(!in_light)) MYdata <- MYdata[-indexes[!in_light]]
    }
  }
  MYdata
}

blank <- function(lim1=0, lim2=1) plot(NULL, type="n", axes=F, ann=F, xlim=c(lim1, lim2), ylim=c(lim1, lim2))

## end FUNCTIONS
########################################################-
## begin SCRIPT

if(!standardSITES)
{  
  shore_names <- latlon$name
  seq_shores  <- 1
  shore_lats  <- latlon$lat
  shore_lons  <- latlon$lon
  fieldwork_dates <- latlon$date
  servicing <- tides_prev <- latlon$prev
}else{
  if(length(plot_shores) != 1 || plot_shores != "all") fieldwork_dates <- fieldwork_dates[plot_shores]

  # read previous servicing dates and shore latlon
  tmp <- if(SERVICING)
  {
    x <- read.xlsx('~/Dropbox/BIO/EuropeanScale/logger_info/logger_servicing_dates.xlsx', 1)
    x <- x[, !apply(x, 2, function(x) all(is.na(x)))]
    colnames(x) <- gsub('NA..', '', colnames(x))
    x
  }else{
    read.csv('fieldsites.csv', sep=";", stringsAsFactors=F)
  }
  
  if(length(plot_shores) != 1 || plot_shores != "all")
  {
    tmp <- tmp[tmp[,1] %in% plot_shores,]	
    ord <- seq(nrow(tmp))
    names(ord) <- tmp[, 1]
    tmp <- tmp[ord[plot_shores],]
  }
  shore_names <- tmp[, 1]
  seq_shores  <- seq(shore_names)
  shore_lats  <- as.numeric(tmp[, 2])
  shore_lons  <- as.numeric(tmp[, 3])
  if(SERVICING)
  {
    servicing   <- tmp[, -(1:3)]
    tides_prev  <- servicing <- foreach(i = seq(nrow(servicing))) %do% as.POSIXct(as.Date(rev(unlist(servicing[i, !is.na(servicing[i, ])]))))
    tides_prev <- lapply(tides_prev, function(x) xts(x, x))
  }
}

rises_sets_day <- list()
for(i in seq_shores) rises_sets_day <- c(rises_sets_day, list(get.rise.set(shore_lats[i], shore_lons[i], work_dates, 0)))
rises_sets_crep <- list()
for(i in seq_shores) rises_sets_crep <- c(rises_sets_crep, list(get.rise.set(shore_lats[i], shore_lons[i], work_dates, 1)))

if(SERVICING & all(!is.na(tides_prev)))
{
  # grab low tide height for past servicing days
  for(i in seq_shores) 
  {
    t <- c()
    for(j in seq(length(tides_prev[[i]])))
    {
      #i <- j <- 1
      dates <- as.POSIXct(paste(servicing[[i]][j], c("00:00", "23:59")))
      tides <- get.fes.tides(shore_lats[i], shore_lons[i], dates, 1, T)
      rise_set <- get.rise.set(shore_lats[i], shore_lons[i], servicing[[i]][j], 0)
      tides <- with.light(tides, rise_set$rise, rise_set$set, go_dark=T)
      t <- c(t, as.numeric(tides[which.min(tides)]))
    }
    coredata(tides_prev[[i]]) <- t
  }

  OUT <- F
  if(OUT)
  {
    # remove outliers from the list of previous tides
    for(i in seq_shores)
    {
    	out <- boxplot(tides_prev[[i]], plot=F)$out
    	if(length(out) == 1) tides_prev[[i]] <- tides_prev[[i]][tides_prev[[i]] != out]
    	if(length(out) == 2) for(j in out) tides_prev[[i]] <- tides_prev[[i]][tides_prev[[i]] != j]
    }
  }
}

# compute percentiles
if(SERVICING & standardSITES)
{
  # compute reference heights from previous tide heights
  height_r1 <- vapply(tides_prev, max, 1)
  height_r2 <- vapply(tides_prev, function(x) round(quantile(x, 0.75), 2), 1)
}else{
  # compute reference heights from 3 years of low tides centered in today
  halfspan <- 365*3*24*3600/2
  dates <- Sys.time() + c(- halfspan, halfspan)
  height_r1 <- height_r2 <- vector()
  for(i in seq_shores)
  {
    tides <- get.fes.tides(shore_lats[i], shore_lons[i], dates, 15, T)
    height_r1 <- c(height_r1, round(quantile(tides, 0.5), 2))
    height_r2 <- c(height_r2, round(quantile(tides, 0.25), 2))
  }
}
if(!standardSITES)
{
  dates <- as.POSIXct(paste(Sys.Date() + c(-730, 730), c("00:00", "23:59")))
  tides <- get.fes.tides(shore_lats, shore_lons, dates, 10, T)
  rise_set <- get.rise.set(shore_lats, shore_lons, dates, 0)
  tides <- as.xts(with.light(tides, rise_set$rise, rise_set$set, go_dark=T))
  tides <- apply.daily(tides, min)
  tides <- tides[tides < quantile(tides, 0.25)]
  tide_history <- tides
}  
	
# grab future low tides for each shore
tides_future <- list()
for(i in seq_shores)
{
  tides <- get.fes.tides(shore_lats[i], shore_lons[i], work_dates, 15, T)
  tides_future <- c(tides_future, list(tides))
}

# select tides bellow the respective previous mean tide height (+ tollerance)
### ok
#night
tides_ok <- foreach(i = seq_shores) %do% as.xts(tides_future[[i]][tides_future[[i]] <= height_r1[i]])
names(tides_ok) <- shore_names
#crepuscule
tides_ok_crep <- tides_ok
for(i in seq(shore_names)) tides_ok_crep[[i]] <- with.light(tides_ok_crep[[i]], rises_sets_crep[[i]]$rise, rises_sets_crep[[i]]$set)
#day
tides_ok_day <- tides_ok
for(i in seq(shore_names)) tides_ok_day[[i]] <- with.light(tides_ok_day[[i]], rises_sets_day[[i]]$rise, rises_sets_day[[i]]$set)
### best
#night
tides_best <- foreach(i = seq_shores) %do% as.xts(tides_future[[i]][tides_future[[i]] <= height_r2[i]])
names(tides_best) <- shore_names
#crepuscule
tides_best_crep <- tides_best
for(i in seq(shore_names)) tides_best_crep[[i]] <- with.light(tides_best_crep[[i]], rises_sets_crep[[i]]$rise, rises_sets_crep[[i]]$set)
#day
tides_best_day <- tides_best
for(i in seq(shore_names)) tides_best_day[[i]] <- with.light(tides_best_day[[i]], rises_sets_day[[i]]$rise, rises_sets_day[[i]]$set)

# get date of logger memory end
if(SERVICING)
{
  if(all(!is.na(tides_prev)))
  {
    seconds_left <- (max_readings/(1+res))*(3600*sampling)
    last_servicing <- vector()
    for(i in seq_shores) last_servicing <- c(last_servicing, tail(servicing[[i]], 1))
    names(last_servicing) <- NULL
    last_servicing <- as.POSIXct(as.Date(as.POSIXct(last_servicing) + seconds_left))
    names(last_servicing) <- shore_names
  
    # get the exact times of chosen tides
    fieldwork_tides <- list()
    for(i in seq_shores)
    {
      fieldwork_tides <- c(fieldwork_tides, list(tides_future[[i]][substr(index(tides_future[[i]]), 1, 13) == fieldwork_dates[i]]))
    }
  }else{
    fieldwork_tides <- list(tides_future[[1]][substr(index(tides_future[[1]]), 1, 13) == fieldwork_dates])
  }
}

## PLOT
big <- length(plot_shores) > 10

# colors for best and ok tides
colbest <- "blue"
colokall <- "yellow"
colokcrep <- "red"
colokday <- "cyan"

TCEX <- 1 #0.7 #1
PCEX <- if(big) 0.8 else 1 #0.5 #1.2
PCH  <- 21
# other colors
qr2 <- "orange"
robo_end <- "red"
last_tide <- "cornflowerblue"
work_tide <- "chartreuse3"

# start plot device
pdf("tides.pdf")

if(big)
{
  layout(matrix(c(rep(1:2, each=3), (1:(max(seq_shores)*3))+2, rep(0, 3)), max(seq_shores)+3, 3, byrow=T), heights=c(0.4, 0.4, rep(1, max(seq_shores)), 1), widths=c(1, 0.05, 0.15))
}else{
  if(length(plot_shores) == 1)
  {
    layout(rbind(
      c(1,2,2), 
      c(3,2,2),
      c(4,5,6),
      c(0,0,0)), 
      heights=c(0.3, 0.25, rep(1, max(seq_shores)), 0.5), widths=c(1, 0.05, 0.15))
  }else{
    layout(matrix(c(rep(1:2, each=3), (1:(max(seq_shores)*3))+2, rep(0, 3)), max(seq_shores)+3, 3, byrow=T), heights=c(0.3, 0.25, rep(1, max(seq_shores)), 0.5), widths=c(1, 0.05, 0.15))
  }
}

par(mar=c(0,4,0,0))

# title
blank()
text(0.5, 0.5, paste0(paste(work_dates, collapse=" / "), "   -   ALL TIMES IN UTC!"), cex=1.2)

# map
if(length(plot_shores) == 1)
{
  box()
  
  par(mar=c(0,0,0,4))
  G2 <- "grey60"
  G3 <- "grey85"
  bufx <- 1.5
  bufy <- 2*bufx
  if(!standardSITES)
  {
    lat <- with(latlon, c(floor(lat-bufy), ceiling(lat+bufy)))
    lon <- with(latlon, c(floor(lon-bufx), ceiling(lon+bufx)))
  }else{
    lat <- c(floor(shore_lats-bufy), ceiling(shore_lats+bufy))
    lon <- c(floor(shore_lons-bufx), ceiling(shore_lons+bufx))
  }
  
  shoreline_path <- "/Users/ruiseabra/Documents/WORK/PhD/RESOURCES/World_vector_shoreline/gshhs/gshhs_f.b"
  
  if(sum(lon<0) == 1)
  {
    coastE <- Rgshhs(shoreline_path, xlim=lon, ylim=lat, level=1, no.clip=T)$SP
    coastW <- Rgshhs(shoreline_path, xlim=lon+360, ylim=lat, level=1, no.clip=T, shift=T)$SP
  }else{
    if(any(lon>0))
    {
      coast  <- Rgshhs(shoreline_path, xlim=lon, ylim=lat, level=1, no.clip=T)$SP
    }else{
      coast  <- Rgshhs(shoreline_path, xlim=lon+360, ylim=lat, level=1, no.clip=T, shift=T)$SP
    } 
  }
  
  plot(NULL, type="n", xlim=lon, ylim=lat, xlab="", ylab="", las=1, main="", bty="n", xaxs="i", yaxs="i", axes=F)
  
  if(sum(lon<0) == 1)
  {
    plot(coastE, col=G3, border=G2, xlim=lon, ylim=lat, axes=F, add=T, lwd=1)
    plot(coastW, col=G3, border=G2, xlim=lon+360, ylim=lat, axes=F, add=T, lwd=1)
  }else{
    if(any(lon>0))
    {
      plot(coast,  col=G3, border=G2, xlim=lon, ylim=lat, axes=F, add=T, lwd=1)
    }else{
      plot(coast,  col=G3, border=G2, xlim=lon+360, ylim=lat, axes=F, add=T, lwd=1)
    }
  }
  
  with(latlon, points(lon, lat, cex=0.8, pch=21, bg="red"))
  box()
}

# legend
if(length(plot_shores) == 1) par(mar=c(0,4,0,0)) else par(mar=c(0,4,0,4))
blank()
if(SERVICING)
{
	legend(0.5, 0.5, c("best", "day", "crepuscule  ", "night", "work"), bty="n", pch=c(rep(PCH, 4), 21), pt.bg=c(colbest, colokday, colokcrep, colokall, work_tide), cex=1.3, xjust=0.5, yjust=0.5, horiz=T)
}else{
	legend(0.5, 0.5, c("best", "day", "crepuscule  ", "night"), bty="n", pch=PCH, pt.bg=c(colbest, colokday, colokcrep, colokall), cex=1.3, xjust=0.5, yjust=0.5, horiz=T)
}
box()

Dates  <- as.POSIXct(work_dates)
Dates2 <- seq.POSIXt(Dates[1], Dates[2], by = "day")
Dates3 <- seq.POSIXt(Dates[1], Dates[2], by = "week")
Dates4 <- seq.POSIXt(Dates[1], Dates[2], by = "month")
Wdates <- c()

# for each shore...
for(i in seq_shores)
{
  # get yy ranges
  this_range <- range(tides_best[[i]], tides_ok[[i]])
  if(SERVICING & all(!is.na(tides_prev))) this_range <- range(this_range, tides_prev[[i]])
  # ...and yy label positions (2 per plot)
  this_range2 <- this_range
  this_range2[1] <- floor(this_range2[1] / 10) * 10
  this_range2[2] <- ceiling(this_range2[2] / 10) * 10
  this_range3 <- this_range2 + c(-5,5)
  if(!standardSITES) this_range3 <- c(floor(min(tide_history)), ceiling(max(tide_history)))
  
  par(mar=c(0,4,0,0))
  plot(NULL, axes=F, ann=F, xlim=Dates + if(big) c(-3600*24*difftime(Dates[2], Dates[1], units="days")/20, 0) else 0, ylim=this_range3)
  axis(2, at=this_range2, las=1)
  
  if(length(Dates4) > 2)
  {
    if(i == max(seq_shores)) axis.POSIXct(1, at = Dates4, format = "%b", las=2)
    abline(v=Dates4, lty=3)
  }else if(length(Dates3) > 2){
    if(i == max(seq_shores)) axis.POSIXct(1, at = Dates3, format = "%b %d", las=2)
    abline(v=Dates3, lty=3)
  }else{
    if(i == max(seq_shores)) axis.POSIXct(1, at = Dates2, format = "%b %d", las=2)
    abline(v=Dates2, lty=3)
  }
  if(i == max(seq_shores) & SERVICING & final) axis.POSIXct(1, at = as.POSIXct(sapply(fieldwork_tides, index), origin="1970-01-01"), format = "(%d) %H:%M", las=2, cex.axis=0.8, col.axis=work_tide, col.ticks=work_tide)
  
  # add light hours if Dates is less than 30 days
  if(difftime(Dates[2], Dates[1], units="days") <= 30)
  {
    RS1 <- as.numeric(rises_sets_day[[i]][,1])
    RS2 <- as.numeric(rises_sets_day[[i]][,2])
    for(rs in 1:length(RS1)) 
    {
      polygon(c(RS1[rs], RS2[rs], RS2[rs], RS1[rs]), c(rep(this_range3[1], 2), rep(this_range3[2], 2)), border=NA, col="lightyellow")
    }
  }
  
  # add line of height during last work
  if(all(!is.na(tides_prev))) abline(h=tail(tides_prev[[i]], 1), col=last_tide)
  
  # add min & max of tides_prev
  if(!final & all(!is.na(tides_prev))) abline(h=range(tides_prev[[i]]), col="grey")
  
  # add chosen work tide vertical lines
  if(final & SERVICING) 
  {
    Wdates <- c(Wdates, index(fieldwork_tides[[i]]))
    abline(h=fieldwork_tides[[i]], col=work_tide)    
    abline(v=Wdates, col=work_tide, lwd=3)
    axis(2, at=fieldwork_tides[[i]], labels=round(fieldwork_tides[[i]]), col.axis=work_tide, las=1, cex.axis=0.8, col.ticks=work_tide)
  }
  
  # add ok tides
  points(tides_ok[[i]], pch=PCH, bg=colokall, cex=PCEX)
  points(tides_ok_crep[[i]], pch=PCH, bg=colokcrep, cex=PCEX)
  points(tides_ok_day[[i]], pch=PCH, bg=colokday, cex=PCEX)
  # add best tides
  points(tides_best[[i]], pch=PCH, bg=colokall, cex=PCEX)
  points(tides_best_crep[[i]], pch=PCH, bg=colbest, cex=PCEX)
  points(tides_best_day[[i]], pch=PCH, bg=colbest, cex=PCEX)
  
  # add line of robolimpet end
  if(SERVICING & all(!is.na(tides_prev))) abline(v=last_servicing[i], col=robo_end)
  
  if(final & SERVICING) 
  {
    # add chosen work tide point
    points(fieldwork_tides[[i]], pch=21, bg=work_tide, cex=2)
    # add height diff between last and next fieldwork
    DIFF <- -as.numeric(round(tail(tides_prev[[i]], 1)) - round(fieldwork_tides[[i]]))
    DIFF <- paste0(if(DIFF > 0) "+", DIFF, " cm")
    mtext(DIFF, 1, -2, adj = 0.95, col=last_tide, cex=0.5)
  }    
  box()
  
  # add end date at bottomright corner
  if(SERVICING & all(!is.na(tides_prev))) mtext(paste("end =", as.Date(last_servicing[i])), 1, -1, adj = 0.95, col=robo_end, cex=0.5)
  
  # shore name
  mtext(shore_names[i], 1, -1, adj=0.01, cex=2)

  # add tide_prev boxplot
  par(mar=c(0,0,0,0))
  if(all(!is.na(tides_prev)))
  {
    plot(rep(1, length(tides_prev[[i]])), tides_prev[[i]], axes=F, ann=F, ylim=this_range3, pch=21, bg="red")
    abline(h=tail(tides_prev[[i]], 1), col=last_tide)    
    if(final & SERVICING) abline(h=fieldwork_tides[[i]], col=work_tide)    
  }else{
    plot(rep(1, nrow(tide_history)), coredata(tide_history), axes=F, ann=F, ylim=this_range3, pch=21, bg="red")
    abline(h=tail(tides_prev[[i]], 1), col=last_tide)
    if(final & SERVICING) abline(h=fieldwork_tides[[i]], col=work_tide)    
  }
  box()
  
  par(mar=c(0,0,0,4))
  if(standardSITES)
  {
    boxplot(tides_prev[[i]], axes=F, ylim=this_range3)
    abline(h=tail(tides_prev[[i]], 1), col=last_tide)    
    if(final & SERVICING) abline(h=fieldwork_tides[[i]], col=work_tide) 
    axis(4, at=tail(tides_prev[[i]], 1), labels=round(tail(tides_prev[[i]], 1)), col.axis=last_tide, las=1, cex.axis=0.8, col.ticks=last_tide)
  }else{
    boxplot(coredata(tide_history), axes=F, ylim=this_range3)
    abline(h=tail(tides_prev[[i]], 1), col=last_tide)    
    if(final & SERVICING) abline(h=fieldwork_tides[[i]], col=work_tide) 
    if(is.na(tides_prev))
    {
      axis(4, las=1, cex.axis=0.8)
    }else{
      axis(4, at=tail(tides_prev[[i]], 1), labels=round(tail(tides_prev[[i]], 1)), col.axis=last_tide, las=1, cex.axis=0.8, col.ticks=last_tide)
    }
  }
  box()
}

dev.off()

if(!final)
{
  print(diff(work_dates))
  if(SERVICING & standardSITES) print(last_servicing)
  cat("
      check tides_best and tides_ok
      for each shore and chose the
      date and time for next servicing
      
      example commands:
      tides_future
      tides_best$Ev
      tides_ok_day$Bi
      do.call(cbind, tides_ok_day)
      do.call(cbind, tides_best_day)
      do.call(cbind, lapply(tides_best_day, function(x) xts(x, as.POSIXct(paste0(substr(index(x), 1, 13), \":00\")))))[, c(\"Sc\", \"Em\")][\"2014-03/2014-05\"]
      do.call(cbind, lapply(tides_ok_crep, function(x) xts(x, as.Date(index(x)))))[, plot_shores][\"2014-03/2014-05\"] 
      
      edit the 'fieldwork_dates' object
      to include the chosen dates and
      run the script once more
      (don't forget to change 'final' to TRUE)
      ")
}