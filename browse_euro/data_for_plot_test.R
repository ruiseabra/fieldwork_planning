input <- list()

input$dateRange <- as.POSIXct(c("2015-10-20", "2015-10-31"))

shs <- c(rep(TRUE, 5), rep(FALSE, nrow(x) - 5))
dts <- rep(as.POSIXct("2015-10-20"), nrow(x))
tds <- rep(0, nrow(x))

for(i in 1:nrow(x)) {
  input[[str_c("sh", x$sh[i])]] <- shs[i]
  input[[str_c("dt", x$sh[i])]] <- dts[i]
  input[[str_c("td", x$sh[i])]] <- tds[i]
}
