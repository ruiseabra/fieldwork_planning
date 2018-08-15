shinyServer(function(input, output, session) { 
  
  plotInput <- reactive({
    # update/reset  yes, workDate and tide (first or second tide in the day)
    for(i in 1:nrow(x)) {
      sho <- sh[i]
      x$yes[i]      <- input[[str_c("sh", sho)]]
      x$workDate[i] <- input[[str_c("dt", sho)]] %>% as.POSIXct(origin = origin)
      x$tide[i]     <- input[[str_c("td", sho)]] %>% as.numeric
    }
    
    # add workDate time and tide height
    x$workDateTime <- x$workDate
    x$workDateTide <- 0
    for(i in 1:nrow(x)) {
      tmp <- filter(x, sh == x$sh[i])$low_tides[[1]] %>% filter(day == x$workDate[i] %>% as.Date)
      x$workDateTime[i] <- tmp$time[x$tide[i] + 1]
      x$workDateTide[i] <- tmp$height[x$tide[i] + 1]
    }
    
    # compute xlim and xlim_range
    xlim <- as.POSIXct(input$dateRange, origin = origin)
    xlim_range <- difftime(xlim[2], xlim[1], units = "days") %>% abs %>% as.numeric
    
    # set par
    par(xpd = NA, # switch off clipping, necessary to always see axis labels
        bg  = "transparent", # switch off background to avoid obscuring adjacent plots
        mar = c(0,0,0,0)) 
    
    ### TOP ROW ### (legend)
    t1 <- tibble(x = 2+(1:5), y = 0.5, col = cols, nm = names(cols))
    t2 <- tibble(x = 1, y = 0.5, nm = "all times in UTC")
    p <- ggplot(t1) + 
      geom_point(aes(x, y, fill = as.character(x)), shape = 21, size = SIZE) + scale_fill_manual(values = as.character(cols)) + 
      geom_text(aes(x, y + 0.4, label = nm)) + geom_text(data = t2, aes(x, y, label = nm), size = 5, fontface = "bold") + 
      ylim(c(0,1)) + xlim(c(0,max(t1$x))) + guides(fill = FALSE) + theme_void()
    top_row <- p
    
    ### TIDE PLOTS ### for each shore:
    #                       --> (txt) name and text related to workDate
    #                       --> (tid) low tides
    #                       --> (box) servDates summary (boxplot)
    rows <- list()
    for(i in 1:nrow(x)) {
      ## TXT ##-------------------------------------------------#
      # end date
      dif1 <- difftime(x$endDates[i], Sys.Date(), units = "days") %>% round
      due  <- str_c("end:  ", as.Date(x$endDates[i]), " (", abs(dif1), " d)")
      
      # work date
      txt <- "work: "
      if(x$yes[i]) {
        d <- x$workDateTime[i]
        h <- x$workDateTide[i]
        dif2 <- difftime(as.Date(d), x$endDates[i], units = 'day') %>% round
        DIF <- if(dif2 <=  0) {
          str_c(abs(dif2), ifelse(abs(dif2) == 1, " day ", " days "), "before memory ends")
        }else{
          str_c(abs(dif2), ifelse(abs(dif2) == 1, " day ", " days "), "after memory ends")
        }
        DIF <- str_c(str_sub(d, 1, 16), " (", round(h, 2), ", ", dif2, " d)")
      }else{
        DIF <- "----"
      }
      txt <- str_c(txt, DIF, "\n")
      
      # combine in tibble
      TXT <- tibble(
        txt  = c(x$sh[i], due, txt),
        col  = c("black", ifelse(dif1 >= 0, "green", "red"), ifelse(!x$yes[i], "black", ifelse(dif2 <= 0, "green", "red"))),
        x    = c(0.2, 0.3, 0.3),
        y    = c(0.5, 0.5, 0.45),
        size = c(10,4,4),
        face = c("bold", "plain", "plain"))
      
      
      # save plot
      p <- ggplot(TXT) + 
        geom_text(aes(x, y, label = txt), size = TXT$size, fontface = TXT$face, family = "Courier", col = TXT$col, hjust = 0) + 
        ylim(0:1) + xlim(0:1) + theme_void()
      p1 <- p
      
      ## TID ##-------------------------------------------------#
      dat1 <- x$low_tides[[i]] %>% filter(time >= xlim[1], time <= xlim[2])
      dat2 <- tibble(h = c(x$prev_lst[i], x$prev_wrst[i]), linetype = c(1,2), col = c("lightgrey", "black"), size = 2:1)
      dat3 <- tibble(h = x$workDateTide[i], v = as.numeric(x$workDateTime[i]))
      ylim <- range(c(dat1$height, dat2$h))
      
      p <- ggplot(dat1) + 
        xlim(xlim) + ylim(ylim) + guides(fill = FALSE) + xlab("") + ylab("") + theme_minimal() + theme(axis.text.y = element_blank(), plot.margin = margin(-1,0,-1,0, unit = "cm"))
      if(i != nrow(x)) p <- p + theme(axis.text.x = element_blank())
      
      if(xlim_range <= 15) p <- p + geom_rect(aes(ymin = ylim[1], ymax = ylim[2], xmin = rise, xmax = set), fill = "cornsilk1") 
      p <- p + geom_hline(data = dat2, aes(yintercept = h), linetype = dat2$linetype, col = dat2$col, size = dat2$size)
      if(x$yes[i]) p <- p + geom_hline(data = dat3, aes(yintercept = h), col = cols["work"], size = 2) + geom_vline(data = dat3, aes(xintercept = v), col = cols['work'], size = 2)
      p <- p + geom_line(aes(time, height)) + geom_point(aes(time, height), shape = 21, size = 3, fill = dat1$cols2)
      p2 <- p 
      
      ## BOX ##-------------------------------------------------#
      dat1 <- x$servDates[[i]]
      p <- ggplot(dat1) + xlab("") + ylab("") + theme_minimal() + theme(axis.text = element_blank()) + ylim(ylim)
      p <- p + geom_hline(data = dat2, aes(yintercept = h), linetype = dat2$linetype, col = dat2$col, size = dat2$size)
      p <- p + geom_boxplot(aes(x = 1, y = height), fill = cols["work"])
      p3 <- p
      
      rows[[i]] <- plot_grid(p1, p2, p3, align = "h", ncol = 3, rel_widths = c(0.15, 0.85, 0.1))
    }
    
    p <- plot_grid(plotlist = rows, nrow = nrow(x))
    p
    if(input$savePlot != oldsave) {
      ggsave(filename = str_c(dirname(getwd()), "/plot.pdf"), plot = p, device = "pdf")
      oldsave <- input$savePlot
    }
  })
  
  output$plot <- renderPlot({
    plotInput()
  })
})

