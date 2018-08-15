shinyServer(function(input, output, session) { 
  
  plotInput <- reactive({
    for(i in 1:nrow(info)) {
      sho <- sh[i]
      info$yes[i]      <- input[[str_c("sh", sho)]]
      info$workDate[i] <- input[[str_c("dt", sho)]] %>% as.POSIXct(origin = origin)
      info$valiDate[i] <- as.Date(info$workDate[i]) != Sys.Date()
      info$tide[i]     <- input[[str_c("td", sho)]] %>% as.numeric
    }

    info$workDateTime <- info$workDate
    info$workDateTide <- 0
    for(i in 1:nrow(info)) {
      tmp <- filter(x, sh == info$sh[i])$low_tides[[1]] %>% filter(day == info$workDate[i] %>% as.Date)
      info$workDateTime[i] <- tmp$time[info$tide[i] + 1]
      info$workDateTide[i] <- tmp$height[info$tide[i] + 1]
    }
    
    info <- filter(info, yes)
    nshores <- nrow(info)

      
    if(!nshores) {
      plot(NULL, xlim = 0:1, ylim = 0:1, ann = FALSE, axes = FALSE)
      text(x = 0, y = 0.5, 'step 1: set the desired date range\n\nstep 2: choose 1 or more shores\n\nstep 3: for each of the selected shores, set a date for fieldwork\n\nstep 4: if available, choose between the 1st (default) or 2nd low tide\n\nstep 5: save the plot', cex = 2, adj = 0)
    }else{
      xlim <- as.POSIXct(input$dateRange, origin = origin)
      
      spanDays <- difftime(xlim[2], xlim[1], units = 'day')
      if(spanDays < 15) {
        spanLevel <- 1 
        vlines <- as.POSIXct(seq(as.Date(xlim[1]), as.Date(xlim[2]) + 1, by = 'day'))
      }
      if(spanDays >= 15 & spanDays < 30) {
        spanLevel <- 2
        d0 <- as.Date(xlim[1]) - (6:0)
        d0 <- d0[weekdays(d0) == 'Monday']
        vlines <- as.POSIXct(seq(d0, as.Date(xlim[2]) + 7, by = 'week'))
      }
      if(spanDays >= 30 & spanDays < 180) {
        spanLevel <- 3
        d0 <- paste0(substr(as.Date(xlim[1]), 1, 8), '01')
        vlines <- as.POSIXct(seq(as.Date(d0), as.Date(xlim[2]) + 30, by = 'month'))
      }
      if(spanDays > 180) {
        spanLevel <- 4
        d0 <- paste0(substr(as.Date(xlim[1]), 1, 5), '01-01')
        vlines <- as.POSIXct(seq(as.Date(d0), as.Date(xlim[2]) + 365, by = 'quarter'))
      }
      
      cols <- set_names(c('blue', 'cyan', 'yellow', 'white', 'green'), c('best', 'ok', 'crepuscule', 'other', 'work'))
      
      plotLayout <- rbind(rep(1, 3))
      for(i in 1:nshores) {
        if(nshores < 9) plotLayout <- rbind(plotLayout, c(rep(max(plotLayout) + 1, 2), 0))
        plotLayout <- rbind(plotLayout, c(max(plotLayout) + 1, max(plotLayout) + 2, 0))
      }
      plotLayout <- rbind(plotLayout, rep(0, 3))
      
      hh <- c(0.03, rep(c(0.03, (0.92 / nshores) - 0.03), nshores), 0.05)
      if(nshores >= 9) hh <- c(0.03, rep(0.92 / nshores, nshores), 0.05)
      
      p <- function(DUE = TRUE) {
        layout(plotLayout, widths = c(0.92, 0.03, 0.05), heights = hh)
        par(mar = c(0,0,0,0))
        plot(NULL, xlim = 0:1, ylim = 0:1, ann = F, axes = F)
        text(0, 0.5, 'all times in UTC', adj = 0, font = 2, cex = 2)
        points(rescale(1:5, to = c(0.2, 0.9)), rep(0.5, 5), pch = 21, bg = cols, cex = 2)
        text( rescale(1:5,  to = c(0.2, 0.9)) + 0.015, rep(0.5, 5), names(cols), adj = 0, cex = 2)
        CEX <- rescale(c(1, nshores, length(tides)), to = c(2, 1.4))[2]
        
        for(s in 1:nshores) {
          sho  <- info$sh[s]
          xord <- which(x$sh == sho)
          TMP  <- x$low_tides[[xord]]
          tmp  <- xts(select(TMP, height, cols), TMP$time)
          ylim <- c(min(tmp$height), max(c(x$avg[xord], x$q50[xord] + 0.5, x$prev_wrst[xord])))
          tmp  <- tmp[str_c(as.Date(xlim[1]), "/", as.Date(xlim[2]) + 1)]
          print(head(tmp))
          rise <- unique(index(as.xts(TMP$rise, TMP$time)[paste0(as.Date(xlim[1]), '/', as.Date(xlim[2])+1)]))
          set  <- unique(index(as.xts(TMP$set , TMP$time)[paste0(as.Date(xlim[1]), '/', as.Date(xlim[2])+1)]))
          
          # header
          if(nshores < 9) {
            par(mar = c(0,5,0,0))
            plot(NULL, xlim = 0:1, ylim = 0:1, ann = FALSE, axes = FALSE)
            polygon(c(-0.1, 1.1, 1.1, -0.1), c(-0.1, -0.1, 1.1, 1.1), border = FALSE, col = 'grey90')
            text(0.01, 0.5, sho, adj = c(0.1, 0.5), font = 2, cex = 2)
            
            z <- 'work date: '
            if(info$valiDate[s]) {
              d <- info$workDateTime[s]
              h <- info$workDateTide[s]
              z <- str_c(z, str_sub(d, 1, 16), ' (', round(h, 2), ') (XX)')
              dif <- difftime(as.Date(d), x$endDates[xord], units = 'day') %>% round
              if(dif == 0) z <- str_replace(z, 'XX', 'during last day')
              if(dif <  0) z <- str_replace(z, 'XX', str_c(abs(dif), ifelse(abs(dif) == 1, ' day ', ' days '), 'before memory end'))
              if(dif >  0) z <- str_replace(z, 'XX', str_c(dif,      ifelse(abs(dif) == 1, ' day ', ' days '),  'after memory end'))
            }else{
              z <- str_c(z, '----')
            }
            text(0.25, 0.5, z, adj = c(0, 0.5), col = 'darkgreen', cex = 1.5)
            
            dif  <- difftime(x$endDates[xord], Sys.Date(), units = 'day') %>% round
            due  <- if(dif == 0) 'due TODAY' else if(dif > 0) str_c('due in ', abs(dif), ' days') else str_c('overdue by ', abs(dif), ' days')
            due2 <- if(DUE) str_c(' (', due, ')') else ''
            text(0.99, 0.5, str_c('end date: ', x$endDates[xord], due2), adj = c(1, 0.5), col = 'red', cex = 1.5)
            box()
          }
          
          # main plot
          par(mar = c(0,5,0,0))
          plot(tmp$height, las = 2, xlim = xlim, ylim = ylim, yaxis.left = FALSE, yaxis.right = FALSE, labels.col = "white")
          if(spanLevel == 1) for(p in 1:length(rise)) polygon(c(rise[p], set[p], set[p], rise[p]), c(ylim[1], ylim[1], ylim[2], ylim[2]), border = FALSE, col = 'lightyellow')
          abline(h = x$prev_lst[xord], lty = 1, col = 'grey')
          if(nshores < 10) abline(h = x$prev_avg[xord], lty = 1, col = 'grey')
          abline(h = c(x$q25[xord], x$q50[xord]), lty = 1, col = 'grey')
          abline(h = x$prev_wrst[xord], lty = 2, col = 'black')
          abline(v = vlines, col = 'grey')
          axis(2, c(x$prev_lst[xord], x$q25[xord]),las = 2, cex.axis = CEX - 0.1)
          abline(v = x$endDates[xord], col = 'red', lwd = CEX + 2)
          if(info$valiDate[s]) {
            abline(v = d, col = cols['work'], lwd = CEX + 2)
            abline(h = h, col = cols['work'], lwd = CEX)
          }
          points(tmp$height, pch = 21, bg = cols[tmp$cols], cex = CEX)
          
          if(s == nshores) {
            f <- c("%Y %b %d\n%a", "%Y %b %d\n%a", "%Y %b", "%Y %b")
            axis.POSIXct(1, at = vlines, format = f[spanLevel], cex.axis = 1.3, padj = 0.9)
          }
          if(nshores >= 9) legend('bottomleft', legend = sho, bty = 'n', text.font = 2, cex = 2.5)
          box()
          
          # history
          par(mar = c(0,0,0,0))
          plot(NULL, xlim = c(0,2), ylim = ylim, axes = FALSE, ann = FALSE, yaxs = 'i')
          abline(h = x$prev_lst[xord], lty = 1, col = 'grey')
          if(nshores < 10) abline(h = x$prev_avg[xord], lty = 1, col = 'grey')
          abline(h = c(x$q25[xord], x$q50[xord]), lty = 1, col = 'grey')
          abline(h = x$prev_wrst[xord], lty = 2, col = 'black')
          if(info$valiDate[s]) abline(h = h, col = cols['work'], lwd = CEX)
          boxplot(x$servDates[xord][[1]]$height, add = TRUE, axes = FALSE, ann = FALSE, col = 'green', outpch = 21, outbg = 'green', cex = CEX - 0.5, boxwex = 2)
          box()
          if(nshores < 10)  axis(4, c(x$prev_lst[xord], x$prev_avg[xord], x$q25[xord], x$q50[xord]), c('wLast', 'wAvg', 'q25', 'q50'), las = 2, cex.axis = CEX - 0.5)
          if(nshores >= 10) axis(4, c(x$prev_lst[xord], x$q25[xord], x$q50[xord]), c('wLast', 'q25', 'q50'), las = 2, cex.axis = CEX - 0.2)
        }
      }
      
      p()
      pdf(str_c(dirname(getwd()), '/plot_', input$savePlot + 1, '.pdf'), 15, 12)
      p(FALSE)
      dev.off()
    }
  })
  
  output$plot <- renderPlot({
    plotInput()
  })
})

