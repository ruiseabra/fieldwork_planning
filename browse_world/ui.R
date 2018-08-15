shinyUI(fluidPage(
  
  titlePanel("fieldwork planner"),
  
  fluidRow(
    column(2,
           fluidRow(
             column(12,
                    # date range
                    dateRangeInput("dateRange",
                            label = NULL,
                            start = Sys.Date(),
                            end   = Sys.Date() + 7),
                    checkboxInput("WRK", "WORK", FALSE))),

           hr(),
           
           # shore list
           fluidRow(
             column(3, div(style = "height: 27px;", checkboxInput("sh1", x$sh[1], FALSE))),
             column(7, div(style = "height: 27px;", dateInput(    "dt1", NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput("td1", NULL, TRUE)))),
           fluidRow(
             column(3, div(style = "height: 27px;", checkboxInput("sh2", x$sh[2], FALSE))),
             column(7, div(style = "height: 27px;", dateInput(    "dt2", NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput("td2", NULL, TRUE)))),
           fluidRow(
             column(3, div(style = "height: 27px;", checkboxInput("sh3", x$sh[3], FALSE))),
             column(7, div(style = "height: 27px;", dateInput(    "dt3", NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput("td3", NULL, TRUE)))),
           fluidRow(
             column(3, div(style = "height: 27px;", checkboxInput("sh4", x$sh[4], FALSE))),
             column(7, div(style = "height: 27px;", dateInput(    "dt4", NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput("td4", NULL, TRUE)))),
           fluidRow(
             column(3, div(style = "height: 27px;", checkboxInput("sh5", x$sh[5], FALSE))),
             column(7, div(style = "height: 27px;", dateInput(    "dt5", NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput("td5", NULL, TRUE)))),
           fluidRow(
             column(3, div(style = "height: 27px;", checkboxInput("sh6", x$sh[6], FALSE))),
             column(7, div(style = "height: 27px;", dateInput(    "dt6", NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput("td6", NULL, TRUE)))),
           fluidRow(
             column(3, div(style = "height: 27px;", checkboxInput("sh7", x$sh[7], FALSE))),
             column(7, div(style = "height: 27px;", dateInput(    "dt7", NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput("td7", NULL, TRUE)))),
           fluidRow(
             column(3, div(style = "height: 27px;", checkboxInput("sh8", x$sh[8], FALSE))),
             column(7, div(style = "height: 27px;", dateInput(    "dt8", NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput("td8", NULL, TRUE)))),
           fluidRow(
             column(3, div(style = "height: 27px;", checkboxInput("sh9", x$sh[9], FALSE))),
             column(7, div(style = "height: 27px;", dateInput(    "dt9", NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput("td9", NULL, TRUE)))),
           fluidRow(
             column(3, div(style = "height: 27px;", checkboxInput("sh10", x$sh[10], FALSE))),
             column(7, div(style = "height: 27px;", dateInput(    "dt10", NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput("td10", NULL, TRUE)))),
           fluidRow(
             column(3, div(style = "height: 27px;", checkboxInput("sh11", x$sh[11], FALSE))),
             column(7, div(style = "height: 27px;", dateInput(    "dt11", NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput("td11", NULL, TRUE)))),
           fluidRow(
             column(3, div(style = "height: 27px;", checkboxInput("sh12", x$sh[12], FALSE))),
             column(7, div(style = "height: 27px;", dateInput(    "dt12", NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput("td12", NULL, TRUE)))),
           fluidRow(
             column(3, div(style = "height: 27px;", checkboxInput("sh13", x$sh[13], FALSE))),
             column(7, div(style = "height: 27px;", dateInput(    "dt13", NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput("td13", NULL, TRUE)))),
           fluidRow(
             column(3, div(style = "height: 27px;", checkboxInput("sh14", x$sh[14], FALSE))),
             column(7, div(style = "height: 27px;", dateInput(    "dt14", NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput("td14", NULL, TRUE)))),
           fluidRow(
             column(3, div(style = "height: 27px;", checkboxInput("sh15", x$sh[15], FALSE))),
             column(7, div(style = "height: 27px;", dateInput(    "dt15", NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput("td15", NULL, TRUE)))),
           fluidRow(
             column(3, div(style = "height: 27px;", checkboxInput("sh16", x$sh[16], FALSE))),
             column(7, div(style = "height: 27px;", dateInput(    "dt16", NULL, Sys.Date()))),
             column(2, div(style = "height: 27px;", checkboxInput("td16", NULL, TRUE))))),
    
    column(10,
      plotOutput("plotT",  height = 30),
      conditionalPanel("input.sh1",  uiOutput("ui.plot1")),
      conditionalPanel("input.sh2",  uiOutput("ui.plot2")),
      conditionalPanel("input.sh3",  uiOutput("ui.plot3")),
      conditionalPanel("input.sh4",  uiOutput("ui.plot4")),
      conditionalPanel("input.sh5",  uiOutput("ui.plot5")),
      conditionalPanel("input.sh6",  uiOutput("ui.plot6")),
      conditionalPanel("input.sh7",  uiOutput("ui.plot7")),
      conditionalPanel("input.sh8",  uiOutput("ui.plot8")),
      conditionalPanel("input.sh9",  uiOutput("ui.plot9")),
      conditionalPanel("input.sh10", uiOutput("ui.plot10")),
      conditionalPanel("input.sh11", uiOutput("ui.plot11")),
      conditionalPanel("input.sh12", uiOutput("ui.plot12")),
      conditionalPanel("input.sh13", uiOutput("ui.plot13")),
      conditionalPanel("input.sh14", uiOutput("ui.plot14")),
      conditionalPanel("input.sh15", uiOutput("ui.plot15")),
      conditionalPanel("input.sh16", uiOutput("ui.plot16")),
      plotOutput("plotB", height = 30))
    )
))
