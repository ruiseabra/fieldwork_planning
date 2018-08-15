res          <- 0    # 1 = high, 0 = low
sampling     <- 1    # fractions of hours, i.e., half hour = 0.5, 10 mins = 1/6
max_readings <- 8192 # maximum number of readings at low res
# in seconds (-1 day, to make it a little bit conservative)
lifeSpan   <- (max_readings / (1 + res)) * (3600 * sampling) - (3600 * 24)
x$endDates <- x$last + lifeSpan

cols <- set_names(c('blue', 'cyan', 'yellow', 'white', 'green'), c('best', 'ok', 'crepuscule', 'other', 'work'))

for(i in 1:nrow(x)) x$low_tides[[i]]$cols2 <- cols[x$low_tides[[i]]$cols]

# yes <- TRUE; wkD <- Sys.Date() + 4; tid <- TRUE; dtR <- c(Sys.Date(), Sys.Date() + 7)
tidePlot <- function(i, yes, wkD, tid, dtR) {
  layout(mat = rbind(1:3), widths = c(0.22, 0.75, 0.03))
  MAR <- c(0,0,0,0)
  
  sho <- x$sh[i]
  
  # update workDate and tide (first or second tide in the day)
  wkD <- as.POSIXct(wkD, origin = origin)
  tid <- as.numeric(tid)
  end <- x$endDates[i]
  
  # find workDate time and tide height
  tmp  <- x$low_tides[[i]] %>% filter(day == wkD %>% as.Date)
  if(nrow(tmp) == 1) tid <- FALSE
  wkDt <- tmp$time[tid + 1]
  wkDh <- tmp$height[tid + 1]
  
  # compute xlim and xlim_range
  xlim <- as.POSIXct(dtR, origin = origin)
  xlim_range <- difftime(xlim[2], xlim[1], units = "days") %>% abs %>% as.numeric
  
  ### PLOTS ###
  ## TXT ##-------------------------------------------------#
  # end date
  dif1 <- difftime(end, Sys.Date(), units = "days") %>% round
  due  <- str_c(as.Date(end), " (", abs(dif1), " d)")
  
  # work date
  if(yes) {
    dif2 <- difftime(as.Date(wkDt), end, units = 'day') %>% round
    wrk1 <- str_sub(wkDt, 1, 16)
    wrk2 <- str_c(round(wkDh, 2), " m")
    wrk3 <- str_c(abs(dif2), " d")
  }else{
    wrk1 <- wrk2 <- wrk3 <- "----"
  }
  
  # combine in tibble
  col1 <- ifelse(!yes, "black", ifelse(dif2 <= 0, "green", "red"))
  col2 <- ifelse(dif1 >= 0, "green", "red")
  
  TXT <- tibble(
    txt   = c(toupper(sho), "work", wrk1, wrk2, wrk3, "end", due),
    col   = c("black", "black", "black", "black", col1, "black", col2),
    x     = c(0, 0.33, 0.335, 0.80, 0.80, 0.33, 0.335),
    pos   = c(4, 2, 4, 4, 4, 2, 4),
    y     = c(0.4, 0.7, 0.7, 0.7, 0.25, 0.25, 0.25),
    size  = c(3, rep(1.5,6)),
    face  = c(2, 1, 1, 1, yes+1, 1, 2))
  
  par(mar = MAR)
  plot(NULL, type = "n", xlim = 0:1, ylim = 0:1, axes = FALSE)
  text(TXT$x, TXT$y, labels = TXT$txt, col = TXT$col, cex = TXT$size, font = TXT$face, pos = TXT$pos)
  box()
  
  ## TID ##-------------------------------------------------#
  h  <- x$low_tides[[i]] %>% filter(time >= xlim[1], time <= xlim[2])
  h2 <- tibble(h = x$prev_lst[i], col = "lightgrey", size = 5)
  h3 <- tibble(h = wkDh, v = as.numeric(wkDt))
  ylim <- range(c(h$height, x$prev_wrst[i], x$servDates[[i]]$height)) + c(-0.1,0.1)
  
  
  par(mar = MAR)
  plot(NULL, type = "n", axes = FALSE, xlim = xlim, ylim = ylim, bty = "n", yaxs = "i")
  if(xlim_range <= 15) rect(xleft = h$rise, xright = h$set, ybottom = ylim[1], ytop = ylim[2], col = "cornsilk1", lty = 0)
  abline(h = h2$h, col = h2$col, lwd = h2$size)
  if(end >= xlim[1] & end <= xlim[2]) abline(v = end, col = "red", lwd = 10)
  if(yes) {
    abline(h = h3$h, col = cols["work"], lwd = 5)
    abline(v = h3$v, col = cols["work"], lwd = 5)
  }
  lines( x = h$time, y = h$height)
  points(x = h$time, y = h$height, pch = 21, bg = h$cols2, cex = 2)
  box()
  
  ## BOX ##-------------------------------------------------#
  h <- x$servDates[[i]]
  par(mar = MAR)
  plot(NULL, type = "n", axes = FALSE, xlim = c(0.5,1.5), ylim = ylim, bty = "n", xaxs = "i", yaxs = "i")
  abline(h = h2$h, col = h2$col, lwd = h2$size)
  if(yes) abline(h = h3$h, col = cols["work"], lwd = 5)
  boxplot(h$height, add = TRUE, col = cols["work"], axes = FALSE)
  box()
}

### TOP ROW ### (legend)
t1 <- tibble(x = seq(1, 3, length.out = 5), y = 0.2, col = cols, nm = names(cols))
t2 <- tibble(x = 0, y = 0.5, nm = "all times in UTC")
top_row <- ggplot(t1) + 
  geom_point(aes(x, y, fill = as.character(x)), shape = 21, size = 3) + scale_fill_manual(values = as.character(cols)) + 
  geom_text(aes(x, y + 0.5, label = nm)) + geom_text(data = t2, aes(x, y, label = nm), size = 5, fontface = "bold") + 
  ylim(c(0,1)) + xlim(c(-2,max(t1$x)+2)) + guides(fill = FALSE) + theme_void()

### BOTTOM ROW ### (xaxis)
XaxisPlot <- function(dtR) {
  layout(mat = rbind(c(0,1,0), c(0,0,0)), widths = c(0.22, 0.75, 0.03), heights = c(0.1, 1))
  
  # compute xlim and xlim_range
  xlim <- as.POSIXct(dtR, origin = origin)
  
  ## AXIS ##-------------------------------------------------#
  h  <- x$low_tides[[1]] %>% filter(time >= xlim[1], time <= xlim[2])
  
  par(mar = c(0,0,0,0))
  plot(NULL, type = "n", axes = FALSE, xlim = xlim, ylim = 0:1, bty = "n", yaxs = "i", xlab = "")
  axis.POSIXct(1, h$time, cex.axis = 1.5)
}
