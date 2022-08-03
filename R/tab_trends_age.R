trendsAge_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Age Trend", value = id, 
           sidebarLayout(
             sidebarPanel(width = widthSidePanel, 
                          uiOutput(outputId = ns("clip")),
                          br(),
                          tabInformation(id = ns("myTabInformation")),
                          hr(),
                          inputCause(id1 = ns("myCause"), id2 = ns("causeHelp")),
                          inputLHJ(id = ns("myLHJ")),
                          inputMeasure(id1 = ns("myMeasure"), id2 = ns("measureHelp"), choices = deathMeasures_Dropdown_noADJ, selected = "cDeathRate"),
                          inputYearGroupingRaceAge(id = ns("myYearGrouping_race_age")),
                          inputLogTrans(id = ns("myLogTrans")),
                          noteSuppression(id = ns("mySuppressionNote")),
                          hr(),
                          fluidRow(column(width = 6, downloadData(id = ns("myData"))), 
                                   column(width = 6, downloadChart(id = ns("myChart"))))
             ),
             mainPanel(width = widthMainPanel, uiOutput(ns("plotTitle")),
                       plotlyOutput(ns("trendsAge"), width="100%",height = 700))
           ),
           br(),
           hr(),
           footer)
}


trendsAge_server <- function(id, urlParams = NULL) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      # Update inputs based on URL if custom URL is supplied ------------------------------------------------------------------------------
      observe({
        if (length(urlParams > 0)) {
          if (urlParams$tab == id) {
            if ("cause" %in% names(urlParams)) updateSelectizeInput(session, "myCause", selected = urlParams$cause)
            if ("county" %in% names(urlParams)) updateSelectInput(session, "myLHJ", selected = urlParams$county)
            if ("measure" %in% names(urlParams)) updateSelectInput(session, "myMeasure", selected = urlParams$measure)
            if ("yearGrouping" %in% names(urlParams)) updateRadioButtons(session, "myYearGrouping_race_age", selected = as.numeric(urlParams$yearGrouping))
            if ("logTrans" %in% names(urlParams)) updateCheckboxInput(session, "myLogTrans", value = as.logical(urlParams$logTrans))
          }
        }
      })
      
      # Automatically generate URL to current view ----------------------------------------------------------------------------------------
      output$clip <- renderUI({
        rclipButton(inputId = "clipbtn", label = "Copy Link to Current View", 
                    clipText = paste0(myURL, 
                                      "?tab=", id, 
                                      "&cause=", input$myCause,
                                      "&county=", input$myLHJ,
                                      "&measure=", input$myMeasure,
                                      "&yearGrouping=", input$myYearGrouping_race_age,
                                      "&logTrans=", input$myLogTrans), 
                    icon = icon("clipboard"))
      })
      
      output$plotTitle <- renderUI({
        h3(paste0("Trend in ",deathMeasuresNames[deathMeasures == input$myMeasure],
                  " of ", deathCauseLink$causeNameShort[deathCauseLink$causeCode == input$myCause], # JASPO
                  " in ", input$myLHJ," by AGE GROUP, ", minYear," to ", maxYear))
      })

      # Render plot -------------------------------------------------------------------------------------------------------------------------
      myStep <- reactive(trendGeneric(myLHJ = input$myLHJ, 
                                      myCause = input$myCause,
                                      myMeasure = input$myMeasure,
                                      myTab = id,
                                      myYearGrouping_race_age = input$myYearGrouping_race_age,
                                      myLogTrans = input$myLogTrans))
      
      output$trendsAge  <- renderPlotly(myStep()$plotL_interactive)
      
      
      # Download data and chart -------------------------------------------------------------------------------------------------------------
      output$myData <- downloadHandler(filename = function() { paste0(id, "-", input$myLHJ, "-", Sys.Date(), ".csv") },
                                       content = function(file) {
                                         write.csv(myStep()$dataL, file, row.names = FALSE)
                                       })
      
      output$myChart <- downloadHandler(filename = function() { paste0(id, "-", input$myLHJ, "-", Sys.Date(), ".png") },
                                        content = function(file) {
                                          png(file, width = 18, height = 10, units = "in", pointsize = 10, res = 100)
                                          print(myStep()$plotL)
                                          dev.off()
                                        })
      
      # Update, disable, & enable year grouping button based on county selection ---------------------------------------------------------------------
      observe({
        if (input$myLHJ != STATE) {
          updateRadioButtons(inputId = "myYearGrouping_race_age", selected = 3)
          shinyjs::disable("myYearGrouping_race_age") 
          } else {
            shinyjs::enable("myYearGrouping_race_age")
          }
      })
      
    }
  )
}