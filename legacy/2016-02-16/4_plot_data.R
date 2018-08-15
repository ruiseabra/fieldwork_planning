#plot width A4 (mm-margin)*inch
pw.mm<-(210-10*2)
pw.inch<-pw.mm*0.0393700787
#plot height A4 (mm-margin)*inch
ph.mm<-(297-10*2)
ph.inch<-ph.mm*0.0393700787

# colors for best and ok tides
colbest <- "blue"
colok <- "red"

# other colors
q25 <- "orange"
robo.end <- "red"
last.tide <- "cornflowerblue"
work.tide <- "chartreuse3"

# start plot device
pdf("tides.pdf", paper="a4", width=pw.inch, height=ph.inch, pagecentre=T)

# length of tick marks
par(tck=-0.1)
    
# a layout with n+1 rows (1st one is for title)
layout(matrix(1:(length(future.tides[[1]])+1), length(future.tides[[1]])+1, 1), heights=c(0.5, rep(1, length(future.tides[[1]]))))

# no margins for title
par(mar=c(0,0,0,0))

# empty plot
plot(1, type="n", xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(0,1), ylim=c(0,1), main="", bty="n")
# text title
text(0.5, 0.5, paste0("Tide table for upcoming fieldwork   -   ", paste(INFO$time$range, collapse=" / "), "   -   ", paste(INFO$time$work, collapse=" / "), " ALL TIMES IN UTC!"))

# set new margins
par(mar=c(1.5,4,0,0.3))

# for each shore...
for(i in 1:length(future.tides[[1]])){
  # get yy ranges
  this.range <- range(range(future.tides$best[[i]]), range(future.tides$ok[[i]]))
  # ...and yy label positions (2 per plot)
  this.range2 <- this.range
  this.range2[1] <- ifelse(round(this.range2[1], 1) < this.range2[1], round(this.range2[1], 1)+0.1, round(this.range2[1], 1))
  this.range2[2] <- ifelse(round(this.range2[2], 1) > this.range2[2], round(this.range2[2], 1)-0.1, round(this.range2[2], 1))
  
  # plot blank
  plot(future.tides$ok[[i]], ylim=this.range, xlim=INFO$time$range, type="n", yaxt="n", major.format="%m/%d", cex.axis=0.5, ylab=names(future.tides$best[i]), las=1, main="", auto.grid=F, mgp=c(2.5,0.1,0), bty="n")
  # add yy axis
  axis(2, at=round(this.range2, 1), las=1, cex.axis=0.7, mgp=c(3,0.7,0))
  
  # add line of height during last work
  abline(h=tail(previous.tides$tides[[i]], 1), lty=3, lwd=0.5, col=last.tide)

  # add line of quantile 1
  abline(h=tail(previous.tides$quant1[[i]], 1), lty=3, lwd=0.5, col=q25)
  
  # add best tides
  points(future.tides$best[[i]], pch=3, col=colbest, cex=0.6)
  # add ok tides
  points(future.tides$ok[[i]], pch=3, col=colok, cex=0.6)
  
  # add line of robolimpet end
  abline(v=INFO$time$last[[i]], col=robo.end)

  # add lines for all work days
  abline(v=as.POSIXct(unlist(lapply(INFO$time$chosen, index)), tz="UTC"), lty=3, col=work.tide, lwd=0.5)
  
  # add chosen work tide
  points(as.POSIXct(index(INFO$time$chosen[[i]]), tz="UTC"), INFO$time$chosen[[i]], pch=15, col=work.tide, cex=0.8)
  
  # add average previous tide height value, and robolimpet end date
  par(new=T)
  plot(1, type="n", xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(0,1), ylim=c(0,1), main="", bty="n")
  text(-0.03, 0.75, paste0("q25: ", previous.tides$quant1[[i]]), col=q25, cex=0.5, adj=0)
  text(-0.03, 0.55, paste0("lst: ", tail(previous.tides$tides[[i]], 1)), col=last.tide, cex=0.5, adj=0)
  text(-0.03, 0.35, paste0("end: ", substr(INFO$time$last[[i]], 1, 10)), col=robo.end, cex=0.5, adj=0)
  text(-0.03, 0.15, paste0("wrk: ", substr(index(INFO$time$chosen[[i]]), 1, 16), " (", INFO$time$chosen[[i]], ")"), col=work.tide, cex=0.5, adj=0)
}

dev.off()