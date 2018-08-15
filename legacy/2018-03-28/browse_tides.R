Sys.setenv(TZ = "UTC") 

library(tidyverse)
library(stringr)
library(lubridate)
library(shiny)
library(scales)
library(xts)

# load previous fieldwork data
lowtides <- dir(pattern = "lowtides")
if(!length(lowtides))    stop("run 'update_tides_database.R' first")
if(length(lowtides) > 1) stop("too many lowtides files, there can be only one")
load(lowtides)
x <- filter(tides, shore != "mindelo")

# delete previous automatic plots
invisible(file.remove(dir(pattern = "plot_", full.names = TRUE)))


runApp("App_browse")
