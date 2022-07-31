trendSex_ui <- function(id) {
  
  ns <- NS(id)

    tabPanel(title = "Sex Trend", value = id,
             sidebarLayout(sidebarPanel(actionButton(ns("myTabInformation"), "Show Tab Information"),
                                        hr(),
                                        dottedSelectInput(ns("myCAUSE"), label=list("Cause of Death:", actionButton(inputId=ns("causeHelp"), label=helpIcon, style=myInputHelpButtonSty)), choices=fullList),
                                        selectInput(ns("myLHJ"),"County/State:",choices=lList,selected=STATE),
                                        selectInput(ns("myMeasure"),  label=list("Measure:", actionButton(ns("measureHelp"), label=helpIcon,style=myInputHelpButtonSty)),
                                                    choices=deathMeasures_Dropdown, selected = "aRate"),
                                        radioButtons(ns("myYearGrouping"), "Years to Group:", choices=c(1,3,5), inline = TRUE),
                                        checkboxInput(ns("myLogTrans"),  "Log Transform of Y Axis", value=FALSE),
                                        div(id = ns("suppressionNote"),
                                            paste('Note: All measures associated with counts <',criticalNumber,', as well as necessary complementrary counts/measures are excluded for data de-identification purposes'),style="color:blue;font-size:12px;padding-left:5px;"
                                        ),
                                        div(id=ns("recentYearNote"),
                                            paste('Note: Data for', currentYear, 'are not yet final. Number of deaths are likely to increase slightly.  Some cause of death codes will become more accurate. These changes are not expected to significantly impact the interpretation of any observed noteworthy patterns or trends.'),
                                            style="color:blue;font-size:12px;padding-left:5px;"
                                        ),
                                        hr(),
                                        fluidRow(column(width = 6, downloadButton(ns("myDownloadData"), "Download Data")), 
                                                 column(width = 6, downloadButton(ns("myDownloadChart"), "Download Chart"))),
                                        width = 4), 
                           mainPanel(plotOutput(ns("trendSex"), width = "100%", height = 700))))
  
}


trendSex_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        print(input$myLHJ)
      })
      trendStep <- reactive(trendGeneric(input$myLHJ, input$myCAUSE, input$myMeasure, input$trendsID, input$myYearGrouping, input$myYearGrouping_race_age,
                                         input$myLogTrans, input$myMultiRace))
      
      output$trendSex  <- renderPlot(trendStep()$plotL)
    }
  )
}