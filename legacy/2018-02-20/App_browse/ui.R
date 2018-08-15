shinyUI(fluidPage(

  titlePanel("Fieldwork planner"),

  fluidRow(
    column(3,
           fluidRow(
             column(9, 
                    # date range
                    dateRangeInput('dateRange',
                                   label=NULL,
                                   start=Sys.Date(),
                                   end=Sys.Date() + 7)),
             column(3,
                    # save the plot as pdf
                    actionButton('downloadPlot', label = 'SAVE'))),
                    #downloadButton('downloadPlot', NULL))),
           
           hr(),
           
           # shore list
           fluidRow(
             column(5, div(style="height: 27px;", checkboxInput("shoreSc", '(UK) SCairn', F))),
             column(5, div(style="height: 27px;", dateInput('dateSc', NULL, Sys.Date()))),
             column(2, div(style="height: 27px;", checkboxInput("tideSc", NULL, T)))),
           fluidRow(
             column(5, div(style="height: 27px;", checkboxInput("shoreAn", '(UK) IAnglesey', F))),
             column(5, div(style="height: 27px;", dateInput('dateAn', NULL, Sys.Date()))),
             column(2, div(style="height: 27px;", checkboxInput("tideAn", NULL, T)))),
           fluidRow(
             column(5, div(style="height: 27px;", checkboxInput("shoreEm", '(IE) Emlagh', F))),
             column(5, div(style="height: 27px;", dateInput('dateEm', NULL, Sys.Date()))),
             column(2, div(style="height: 27px;", checkboxInput("tideEm", NULL, T)))),
           fluidRow(
             column(5, div(style="height: 27px;", checkboxInput("shoreMc", '(IE) MCastle', F))),
             column(5, div(style="height: 27px;", dateInput('dateMc', NULL, Sys.Date()))),
             column(2, div(style="height: 27px;", checkboxInput("tideMc", NULL, T)))),
           fluidRow(
             column(5, div(style="height: 27px;", checkboxInput("shoreWe", '(UK) Wembury', F))),
             column(5, div(style="height: 27px;", dateInput('dateWe', NULL, Sys.Date()))),
             column(2, div(style="height: 27px;", checkboxInput("tideWe", NULL, T)))),
           fluidRow(
             column(5, div(style="height: 27px;", checkboxInput("shoreLa", '(FR) Landunvez', F))),
             column(5, div(style="height: 27px;", dateInput('dateLa', NULL, Sys.Date()))),
             column(2, div(style="height: 27px;", checkboxInput("tideLa", NULL, T)))),
           fluidRow(
             column(5, div(style="height: 27px;", checkboxInput("shoreCc", '(FR) LeCroisic', F))),
             column(5, div(style="height: 27px;", dateInput('dateCc', NULL, Sys.Date()))),
             column(2, div(style="height: 27px;", checkboxInput("tideCc", NULL, T)))),
           fluidRow(
             column(5, div(style="height: 27px;", checkboxInput("shoreRn", '(FR) Royan', F))),
             column(5, div(style="height: 27px;", dateInput('dateRn', NULL, Sys.Date()))),
             column(2, div(style="height: 27px;", checkboxInput("tideRn", NULL, T)))),
           fluidRow(
             column(5, div(style="height: 27px;", checkboxInput("shoreBi", '(FR) Biarritz', F))),
             column(5, div(style="height: 27px;", dateInput('dateBi', NULL, Sys.Date()))),
             column(2, div(style="height: 27px;", checkboxInput("tideBi", NULL, T)))),
           fluidRow(
             column(5, div(style="height: 27px;", checkboxInput("shoreSv", '(ES) SBarquera', F))),
             column(5, div(style="height: 27px;", dateInput('dateSv', NULL, Sys.Date()))),
             column(2, div(style="height: 27px;", checkboxInput("tideSv", NULL, T)))),
           fluidRow(
             column(5, div(style="height: 27px;", checkboxInput("shoreLc", '(ES) LaCaridad', F))),
             column(5, div(style="height: 27px;", dateInput('dateLc', NULL, Sys.Date()))),
             column(2, div(style="height: 27px;", checkboxInput("tideLc", NULL, T)))),
           fluidRow(
             column(5, div(style="height: 27px;", checkboxInput("shoreTo", '(ES) Touriñan', F))),
             column(5, div(style="height: 27px;", dateInput('dateTo', NULL, Sys.Date()))),
             column(2, div(style="height: 27px;", checkboxInput("tideTo", NULL, T)))),
           fluidRow(
             column(5, div(style="height: 27px;", checkboxInput("shoreMo", '(PT) Moledo', F))),
             column(5, div(style="height: 27px;", dateInput('dateMo', NULL, Sys.Date()))),
             column(2, div(style="height: 27px;", checkboxInput("tideMo", NULL, T)))),
           fluidRow(
             column(5, div(style="height: 27px;", checkboxInput("shoreSl", '(PT) SLourenço', F))),
             column(5, div(style="height: 27px;", dateInput('dateSl', NULL, Sys.Date()))),
             column(2, div(style="height: 27px;", checkboxInput("tideSl", NULL, T)))),
           fluidRow(
             column(5, div(style="height: 27px;", checkboxInput("shoreAl", '(PT) Alteirinhos', F))),
             column(5, div(style="height: 27px;", dateInput('dateAl', NULL, Sys.Date()))),
             column(2, div(style="height: 27px;", checkboxInput("tideAl", NULL, T)))),
           fluidRow(
             column(5, div(style="height: 27px;", checkboxInput("shoreEv", '(PT) Evaristo', F))),
             column(5, div(style="height: 27px;", dateInput('dateEv', NULL, Sys.Date()))),
             column(2, div(style="height: 27px;", checkboxInput("tideEv", NULL, T))))),
  
    column(9,
      plotOutput('plot', height=750)
    ))
))
