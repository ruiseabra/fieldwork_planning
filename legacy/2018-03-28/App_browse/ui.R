shinyUI(fluidPage(

  titlePanel("fieldwork planner"),

  fluidRow(
    column(3,
           fluidRow(
             column(9, 
                    # date range
                    dateRangeInput("dateRange",
                                   label = NULL,
                                   start = Sys.Date(),
                                   end   = Sys.Date() + 7)),
             column(3,
                    # save the plot as pdf
                    actionButton("savePlot", label = "SAVE"))),

           hr(),
           
           # shore list
           fluidRow(
             column(5, div(style = "height: 27px;", checkboxInput(str_c("sh", x$sh[1]), x$shore[1], FALSE))),
             column(5, div(style = "height: 27px;", dateInput(    str_c("dt", x$sh[1]), NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput(str_c("td", x$sh[1]), NULL, TRUE)))),
           fluidRow(
             column(5, div(style = "height: 27px;", checkboxInput(str_c("sh", x$sh[2]), x$shore[2], FALSE))),
             column(5, div(style = "height: 27px;", dateInput(    str_c("dt", x$sh[2]), NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput(str_c("td", x$sh[2]), NULL, TRUE)))),
           fluidRow(
             column(5, div(style = "height: 27px;", checkboxInput(str_c("sh", x$sh[3]), x$shore[3], FALSE))),
             column(5, div(style = "height: 27px;", dateInput(    str_c("dt", x$sh[3]), NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput(str_c("td", x$sh[3]), NULL, TRUE)))),
           fluidRow(
             column(5, div(style = "height: 27px;", checkboxInput(str_c("sh", x$sh[4]), x$shore[4], FALSE))),
             column(5, div(style = "height: 27px;", dateInput(    str_c("dt", x$sh[4]), NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput(str_c("td", x$sh[4]), NULL, TRUE)))),
           fluidRow(
             column(5, div(style = "height: 27px;", checkboxInput(str_c("sh", x$sh[5]), x$shore[5], FALSE))),
             column(5, div(style = "height: 27px;", dateInput(    str_c("dt", x$sh[5]), NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput(str_c("td", x$sh[5]), NULL, TRUE)))),
           fluidRow(
             column(5, div(style = "height: 27px;", checkboxInput(str_c("sh", x$sh[6]), x$shore[6], FALSE))),
             column(5, div(style = "height: 27px;", dateInput(    str_c("dt", x$sh[6]), NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput(str_c("td", x$sh[6]), NULL, TRUE)))),
           fluidRow(
             column(5, div(style = "height: 27px;", checkboxInput(str_c("sh", x$sh[7]), x$shore[7], FALSE))),
             column(5, div(style = "height: 27px;", dateInput(    str_c("dt", x$sh[7]), NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput(str_c("td", x$sh[7]), NULL, TRUE)))),
           fluidRow(
             column(5, div(style = "height: 27px;", checkboxInput(str_c("sh", x$sh[8]), x$shore[8], FALSE))),
             column(5, div(style = "height: 27px;", dateInput(    str_c("dt", x$sh[8]), NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput(str_c("td", x$sh[8]), NULL, TRUE)))),
           fluidRow(
             column(5, div(style = "height: 27px;", checkboxInput(str_c("sh", x$sh[9]), x$shore[9], FALSE))),
             column(5, div(style = "height: 27px;", dateInput(    str_c("dt", x$sh[9]), NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput(str_c("td", x$sh[9]), NULL, TRUE)))),
           fluidRow(
             column(5, div(style = "height: 27px;", checkboxInput(str_c("sh", x$sh[10]), x$shore[10], FALSE))),
             column(5, div(style = "height: 27px;", dateInput(    str_c("dt", x$sh[10]), NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput(str_c("td", x$sh[10]), NULL, TRUE)))),
           fluidRow(
             column(5, div(style = "height: 27px;", checkboxInput(str_c("sh", x$sh[11]), x$shore[11], FALSE))),
             column(5, div(style = "height: 27px;", dateInput(    str_c("dt", x$sh[11]), NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput(str_c("td", x$sh[11]), NULL, TRUE)))),
           fluidRow(
             column(5, div(style = "height: 27px;", checkboxInput(str_c("sh", x$sh[12]), x$shore[12], FALSE))),
             column(5, div(style = "height: 27px;", dateInput(    str_c("dt", x$sh[12]), NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput(str_c("td", x$sh[12]), NULL, TRUE)))),
           fluidRow(
             column(5, div(style = "height: 27px;", checkboxInput(str_c("sh", x$sh[13]), x$shore[13], FALSE))),
             column(5, div(style = "height: 27px;", dateInput(    str_c("dt", x$sh[13]), NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput(str_c("td", x$sh[13]), NULL, TRUE)))),
           fluidRow(
             column(5, div(style = "height: 27px;", checkboxInput(str_c("sh", x$sh[14]), x$shore[14], FALSE))),
             column(5, div(style = "height: 27px;", dateInput(    str_c("dt", x$sh[14]), NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput(str_c("td", x$sh[14]), NULL, TRUE)))),
           fluidRow(
             column(5, div(style = "height: 27px;", checkboxInput(str_c("sh", x$sh[15]), x$shore[15], FALSE))),
             column(5, div(style = "height: 27px;", dateInput(    str_c("dt", x$sh[15]), NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput(str_c("td", x$sh[15]), NULL, TRUE)))),
           fluidRow(
             column(5, div(style = "height: 27px;", checkboxInput(str_c("sh", x$sh[16]), x$shore[16], FALSE))),
             column(5, div(style = "height: 27px;", dateInput(    str_c("dt", x$sh[16]), NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput(str_c("td", x$sh[16]), NULL, TRUE))))),

    column(9,
      plotOutput("plot", height = 750)
    ))
))
