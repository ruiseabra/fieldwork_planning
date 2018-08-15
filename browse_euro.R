Sys.setenv(TZ = "UTC") 

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(xts))
suppressPackageStartupMessages(library(cowplot))

# load previous fieldwork data
lowtides <- dir("other/", pattern = "lowtides", full.names = TRUE)
if(!length(lowtides))    stop("run 'update_tides_database.R' first")
if(length(lowtides) > 1) stop("too many lowtides files, there can be only one")
load(lowtides)
x <- filter(tides, shore != "mindelo")

runApp("browse_euro")

