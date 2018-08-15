shinyServer(function(input, output, session) { 
  
  plotInput <- reactive({

    shores2plot <- c(input$shoreSc, input$shoreAn, input$shoreEm, input$shoreMc, input$shoreWe, input$shoreLa, input$shoreCc, input$shoreRn, input$shoreBi, input$shoreSv, input$shoreLc, input$shoreTo, input$shoreMo, input$shoreSl, input$shoreAl, input$shoreEv)
    names(shores2plot) <- c("Sc", "An", "Em", "Mc", "We", "La", "Cc", "Rn", "Bi", "Sv", "Lc", "To", "Mo", "Sl", "Al", "Ev")
    
    workDates <- as.POSIXct(c(input$dateSc, input$dateAn, input$dateEm, input$dateMc, input$dateWe, input$dateLa, input$dateCc, input$dateRn, input$dateBi, input$dateSv, input$dateLc, input$dateTo, input$dateMo, input$dateSl, input$dateAl, input$dateEv), origin='1970-01-01')
    names(workDates) <- names(shores2plot)
    validWdate <- as.Date(workDates) != Sys.Date()
    
    tidesOrder <- c(input$tideSc, input$tideAn, input$tideEm, input$tideMc, input$tideWe, input$tideLa, input$tideCc, input$tideRn, input$tideBi, input$tideSv, input$tideLc, input$tideTo, input$tideMo, input$tideSl, input$tideAl, input$tideEv)
    names(tidesOrder) <- names(shores2plot)
    
    workDatesOrder <- list()
    for(i in names(workDates)) workDatesOrder[[i]] <- tides[[i]]$tide$tide[as.character(as.Date(workDates[i]))][if(tidesOrder[i]) 1 else 2]
    
    endDates <- unlist(lapply(tides, function(x) as.POSIXct(as.Date(index(tail(x$prev, 1)) + lifeSpan))))
    endDates <- as.POSIXct(endDates, origin='1970-01-01')
    
    nshores <- sum(shores2plot)
    if(!nshores) 
    {
      plot(NULL, xlim=0:1, ylim=0:1, ann=F, axes=F)
      text(x=0, y=0.5, 'step 1: set the desired date range\n\nstep 2: choose 1 or more shores\n\nstep 3: for each of the selected shores, set a date for fieldwork\n\nstep 4: if available, choose between the 1st (default) or 2nd low tide\n\nstep 5: save the plot', cex=2, adj=0)
    }else{
      xlim <- as.POSIXct(input$dateRange, origin='1970-01-01')
      # nshores <- 3; xlim <- as.POSIXct(c('2016-02-17 00:00', '2016-03-05 23:59')); s <- 1; shores2plot <- c(rep(T, 3), rep(F, 13)); names(shores2plot) <- c("Sc", "Em", "An", "Mc", "We", "La", "Cc", "Rn", "Bi", "Sv", "Lc", "To", "Mo", "Sl", "Al", "Ev"); workDates <- c(Sys.Date(), as.POSIXct('2016-03-12'), as.POSIXct('2016-03-13'), rep(Sys.Date(), 13)); names(workDates) <- names(shores2plot); tidesOrder <- rep(T, 16); names(tidesOrder) <- names(shores2plot); validWdate <- workDates != Sys.Date(); endDates <- unlist(lapply(tides, function(x) as.POSIXct(as.Date(index(tail(x$prev, 1)) + lifeSpan)))); endDates <- as.POSIXct(endDates, origin='1970-01-01')
      
      spanDays <- difftime(xlim[2], xlim[1], units='day')
      if(spanDays < 15) 
      {
        spanLevel <- 1 
        vlines <- as.POSIXct(seq(as.Date(xlim[1]), as.Date(xlim[2])+1, by='day'))
      }
      if(spanDays >= 15 & spanDays < 30) 
      {
        spanLevel <- 2
        d0 <- as.Date(xlim[1])-(6:0)
        d0 <- d0[weekdays(d0) == 'Monday']
        vlines <- as.POSIXct(seq(d0, as.Date(xlim[2])+7, by='week'))
      }
      if(spanDays >= 30 & spanDays < 180)
      {
        spanLevel <- 3
        d0 <- paste0(substr(as.Date(xlim[1]), 1, 8), '01')
        vlines <- as.POSIXct(seq(as.Date(d0), as.Date(xlim[2])+30, by='month'))
      }
      if(spanDays > 180)
      {
        spanLevel <- 4
        d0 <- paste0(substr(as.Date(xlim[1]), 1, 5), '01-01')
        vlines <- as.POSIXct(seq(as.Date(d0), as.Date(xlim[2])+365, by='quarter'))
      }
      
      cols <- c('blue', 'cyan', 'yellow', 'white', 'green')
      names(cols) <- c('best', 'ok', 'crepuscule', 'other', 'work')
      
      plotLayout <- rbind(rep(1, 3))
      for(i in 1:nshores) {
        if(nshores < 9) plotLayout <- rbind(plotLayout, c(rep(max(plotLayout)+1, 2), 0))
        plotLayout <- rbind(plotLayout, c(max(plotLayout)+1, max(plotLayout)+2, 0))
      }
      plotLayout <- rbind(plotLayout, rep(0, 3))
      
      hh <- c(0.03, rep(c(0.03, (0.92/nshores)-0.03), nshores), 0.05)
      if(nshores >= 9) hh <- c(0.03, rep(0.92/nshores, nshores), 0.05)
      
      p <- function(DUE = T) {
        layout(plotLayout, widths=c(0.92, 0.03, 0.05), heights=hh)
        par(mar=c(0,0,0,0))
        plot(NULL, xlim=0:1, ylim=0:1, ann=F, axes=F)
        text(0, 0.5, 'all times in UTC', adj=0, font=2, cex=2)
        points(set.range(1:5, 0.2, 0.9), rep(0.5, 5), pch=21, bg=cols, cex=2)
        text(set.range(1:5, 0.2, 0.9)+0.015, rep(0.5, 5), names(cols), adj=0, cex=2)
        
        CEX <- set.range(c(1, nshores, length(tides)), 2, 1.4)[2]
        
        for(s in which(shores2plot))
        {
          sh <- names(shores2plot[s])
          TMP  <- tides[[sh]]
          tmp  <- TMP$tide
          ylim <- c(min(tmp), max(c(TMP$avg, TMP$q50 + 0.5, TMP$prev_wrst)))
          tmp  <- tmp[paste0(as.Date(xlim[1]), '/', as.Date(xlim[2])+1)]
          rise <- unique(index(as.xts(TMP$rise)[paste0(as.Date(xlim[1]), '/', as.Date(xlim[2])+1)]))
          set  <- unique(index(as.xts(TMP$set )[paste0(as.Date(xlim[1]), '/', as.Date(xlim[2])+1)]))
          
          # header
          if(nshores < 9)
          {
            par(mar=c(0,5,0,0))
            plot(NULL, xlim=0:1, ylim=0:1, ann=F, axes=F)
            polygon(c(-0.1, 1.1, 1.1, -0.1), c(-0.1, -0.1, 1.1, 1.1), border=F, col='grey90')
            text(0.01, 0.5, SHORES[sh], adj=c(0.1, 0.5), font=2, cex=2)
            
            x <- 'work date: '
            if(validWdate[sh]) 
            {
              d <- index(workDatesOrder[[sh]])
              x <- paste0(x, substr(d, 1, 16), ' (', round(as.numeric(workDatesOrder[[sh]]), 2), ') (XX)')
              dif <- difftime(as.Date(d), endDates[sh], units='day')
              if(dif == 0) x <- gsub('XX', 'during last day', x)
              if(dif <  0) x <- gsub('XX', paste(abs(dif), ifelse(abs(dif) == 1, 'day', 'days'), 'before memory end'), x)
              if(dif >  0) x <- gsub('XX', paste(dif, ifelse(abs(dif) == 1, 'day', 'days'), 'after memory end'), x)
            }else{
              x <- paste0(x, '----')
            }
            text(0.25, 0.5, x, adj=c(0, 0.5), col='darkgreen', cex=1.5)
            
            dif <- difftime(endDates[sh], Sys.Date(), units='day')
            due <- if(dif == 0) 'due TODAY' else if(dif > 0) paste('due in', abs(dif), 'days') else paste('overdue by', abs(dif), 'days')
            due2 <- if(DUE) paste0(' (', due, ')') else ''
            text(0.99, 0.5, paste0('end date: ', endDates[sh], due2), adj=c(1, 0.5), col='red', cex=1.5)
            box()
          }
          
          # main plot
          par(mar=c(0,5,0,0))
          plot(tmp$tide, las=2, axes=F, auto.grid=F, type='n', main='', xlim=xlim, ylim=ylim, xaxs='i', yaxs='i')
          if(spanLevel == 1) for(p in 1:length(rise)) polygon(c(rise[p], set[p], set[p], rise[p]), c(ylim[1], ylim[1], ylim[2], ylim[2]), border=F, col='lightyellow')
          if(spanLevel == 1) for(p in 1:length(rise)) polygon(c(rise[p], set[p], set[p], rise[p]), c(ylim[1], ylim[1], ylim[2], ylim[2]), border=F, col='lightyellow')
          abline(h=TMP$prev_lst, lty=1, col='grey')
          if(nshores < 10) abline(h=TMP$prev_avg, lty=1, col='grey')
          abline(h=c(TMP$q25, TMP$q50), lty=1, col='grey')
          abline(h=TMP$prev_wrst, lty=2, col='black')
          abline(v=vlines, col='grey')
          axis(2, c(TMP$prev_lst, TMP$q25),las=2, cex.axis=CEX-0.1)
          abline(v=endDates[sh], col='red', lwd=CEX+2)
          if(validWdate[sh]) 
          {
            abline(v=index(workDatesOrder[[sh]]), col=cols['work'], lwd=CEX+2)
            abline(h=as.numeric(workDatesOrder[[sh]]), col=cols['work'], lwd=CEX)
          }
          points(tmp$tide, pch=21, bg=cols[tmp$cols], cex=CEX)
          #if(validWdate[sh]) points(workDatesOrder[[sh]], pch=21, bg=cols['work'], cex=CEX+0.2)
          if(s == tail(which(shores2plot), 1))
          {
            f <- c("%Y %b %d\n%a", "%Y %b %d\n%a", "%Y %b", "%Y %b")
            axis.POSIXct(1, at=vlines, format=f[spanLevel], cex.axis=1.3, padj=0.9)
          }
          if(nshores >= 9) legend('bottomleft', legend=sh, bty='n', text.font=2, cex=2.5)
          box()
          
          # history
          par(mar=c(0,0,0,0))
          plot(NULL, xlim=c(0,2), ylim=ylim, axes=F, ann=F, yaxs='i')
          abline(h=TMP$prev_lst, lty=1, col='grey')
          if(nshores < 10) abline(h=TMP$prev_avg, lty=1, col='grey')
          abline(h=c(TMP$q25, TMP$q50), lty=1, col='grey')
          abline(h=TMP$prev_wrst, lty=2, col='black')
          if(validWdate[sh]) abline(h=as.numeric(workDatesOrder[[sh]]), col=cols['work'], lwd=CEX)
          boxplot(coredata(TMP$prev), add=T, axes=F, ann=F, col='green', outpch=21, outbg='green', cex=CEX-0.5, boxwex=2)
          box()
          if(nshores < 10)  axis(4, c(TMP$prev_lst, TMP$prev_avg, TMP$q25, TMP$q50), c('wLast', 'wAvg', 'q25', 'q50'), las=2, cex.axis=CEX-0.5)
          if(nshores >= 10) axis(4, c(TMP$prev_lst, TMP$q25, TMP$q50), c('wLast', 'q25', 'q50'), las=2, cex.axis=CEX-0.2)
        }
      }
      
      p()
      pdf(paste0(dirname(getwd()), '/plot_', input$downloadPlot+1, '.pdf'), 15, 12)
      p(F)
      dev.off()
    }
  })
  
  output$plot <- renderPlot({
    plotInput()
  })
})

