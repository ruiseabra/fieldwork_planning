rm(list=ls())
Sys.setenv(TZ = "UTC") 

library(xts)
library(shiny)

source('functions.R')
lowtides <- dir(pattern = 'lowtides')
if(!length(lowtides)) stop("run 'update_tides_database.R' first")
if(length(lowtides) > 1) stop("too many lowtides files, there can be only one")
load(lowtides)
tides <- tides[names(tides) != 'Mi']

runApp("App_browse")
