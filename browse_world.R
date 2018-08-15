suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(maps))
suppressPackageStartupMessages(library(maptools))
suppressPackageStartupMessages(library(xts))
Sys.setenv(TZ = "UTC") 
source("functions.R")

# for the map
world <- map_data("world")

t0 <- Sys.Date()

# lat <- -24.0
# lon <- -45.8

# UI ####
ui <- fluidPage(title = "Global tide viewer",
                fluidRow(
                  column(2,
                         column(6, numericInput("lat",  label = "lat", value = 41.1, step = 0.1)),
                         column(6, numericInput("lon",  label = "lon", value = -8.7, step = 0.1)),
                         numericInput("tbuf", label = "time buffer (yrs)", value = 1),
                         dateRangeInput("t_range",
                                       label = "target date range",
                                       start = Sys.Date(),
                                       end   = Sys.Date() + 7),
                         
                         br(),
                         plotOutput(outputId = "map1", height = "250px"),
                         plotOutput(outputId = "map2", height = "250px")),

                  # Plot
                  mainPanel(
                           plotOutput(outputId = "plot1", height = "300px"),
                           plotOutput(outputId = "plot2", height = "500px"))
                    ))

# SERVER ####
# input <- list(lat = 41.181457, lon = -8.724782, tbuf = 5, t_range = c(Sys.Date(), Sys.Date() + 7))
server <- function(input, output) {
  
  MAP <- reactive({
    P <- suppressWarnings(
      ggplot() + 
        theme_void() +
        theme(panel.border = element_rect(colour = "black", fill = NA)) +
        geom_map(data = world, map = world, aes(x = long, y=lat, map_id=region), fill = "lightgrey", col = "black", size = 0.25) +
        geom_point(data = tibble(x = input$lon, y = input$lat), aes(x, y), alpha = 0.5, size = 4, shape = 21, fill = "blue"))
    P
  })
  
  output$map1 <- renderPlot({
    w <- 1
    P <- MAP()
    P <- P +
      coord_cartesian(xlim = input$lon + c(-1,1) * w, ylim = input$lat + c(-1,1) * w * 1.5)
    P
  })
  
  output$map2 <- renderPlot({
    w <- 12
    P <- MAP()
    P <- P +
      coord_cartesian(xlim = input$lon + c(-1,1) * w, ylim = input$lat + c(-1,1) * w * 1.5)
    P
  })
  
  TRANGE <- reactive({ 
    as.POSIXct(t0 + (input$tbuf * 365 * c(-1, 1)))
  })
  
  TIDES_and_SUNRISESET <- reactive({ 
    t_range   <- TRANGE()
    tide.and.sun(input$lat, input$lon, t_range, t_res = 900)
  })
  
  PREPARE <- reactive({
    t_range   <- TRANGE()
    vals      <- TIDES_and_SUNRISESET()
    
    tid     <- filter(vals$tide, low) %>% select(-low)
    tid$col <- as.character(tid$light + 1)
    
    xlim_range <- input$t_range[2] - input$t_range[1]
    
    sun <- vals$sun %>% select(-day)
    sun$ymin <- min(vals$tide$tide)
    sun$ymax <- max(vals$tide$tide)
    
    rect1 <- tibble(xmin = as.POSIXct(input$t_range[1]), 
                    xmax = as.POSIXct(input$t_range[2]), 
                    ymin = min(tid$tide),
                    ymax = max(tid$tide))
    
    list(TID = vals$tide, t_range = t_range, stats = as.numeric(vals$stats), tid = tid, xlim_range = xlim_range, sun = sun, rect1 = rect1)
  })
  
  PLOT <- reactive({
    x <<- PREPARE()
    P <- ggplot(x$tid) +
      labs(x = "", y = "") +
      # ylim(range(x$tid$tide)) +
      scale_y_continuous(labels = round(x$stats), breaks = x$stats) +
      theme_classic() +
      theme(axis.text = element_text(size = 15), 
            panel.border = element_rect(colour = "black", fill = NA))
    
    list(x = x, P = P)
  })
  
  output$plot1 <- renderPlot({
    P <- PLOT()
    x <- P$x
    P <- P$P +
      coord_cartesian(xlim = x$t_range) +
      geom_rect(data = x$rect1, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), col = "red", fill = "red", alpha = 0.5) +
      geom_hline(yintercept = x$stats, col = c("blue", "lightblue", "grey"), size = 3) +
      geom_point(aes(x = time, y = tide), size = 0.5)
    P
  })
  
  output$plot2 <- renderPlot({
    P <- PLOT()
    x <- P$x
    P <- P$P +
      coord_cartesian(xlim = as.POSIXct(input$t_range)) +
      scale_fill_manual(values = c("darkgrey", "yellow")) +
      guides(fill = FALSE)
    
    if (x$xlim_range <= 15) {
      P <- P +
        geom_rect(data = x$sun, aes(xmin = rise, xmax = set, ymin = ymin, ymax = ymax), fill = "cornsilk1")
    }
    
    P <- P +
      geom_hline(yintercept = x$stats, col = c("blue", "lightblue", "grey"), size = 3) +
      geom_line(data = x$TID, aes(x = time, y = tide), size = 0.5) +
      geom_point(aes(x = time, y = tide, fill = col), shape = 21, size = 4)
    
    P
  })
}

# APP ####
shinyApp(ui = ui, server = server)

