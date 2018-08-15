# all lines of 'browse_tides.R' except the one to run the app
# ------------------
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
# ------------------

source("App_browse/global.R")
DUE = TRUE
input <- list()

input$dateRange <- as.POSIXct(c("2015-10-01", "2015-10-31"))

shs <- c(rep(TRUE, 5), rep(FALSE, nrow(x) - 5))
dts <- rep(as.POSIXct("2015-10-20"), nrow(x))
tds <- rep(0, nrow(x))

for(i in 1:nrow(x)) {
  input[[str_c("sh", x$sh[i])]] <- shs[i]
  input[[str_c("dt", x$sh[i])]] <- dts[i]
  input[[str_c("td", x$sh[i])]] <- tds[i]
}
