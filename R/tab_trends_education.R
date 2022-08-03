trendsEducation_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Education Trend", value = id, 
           sidebarLayout(
             sidebarPanel(width = widthSidePanel,
                          uiOutput(outputId = ns("clip")),
                          br(),
                          tabInformation(id = ns("myTabInformation")),
                          hr(),
                          inputCause(id1 = ns("myCause"), id2 = ns("causeHelp")),
                          inputLHJ(id = ns("myLHJ")),
                          inputMeasure(id1 = ns("myMeasure"), id2 = ns("measureHelp"), choices = deathMeasures_Dropdown, selected = "aRate"),
                          inputSex(id = ns("mySex")),
                          inputLogTrans(id = ns("myLogTrans")),
                          noteSuppression(id = ns("mySuppressionNote")),
                          hr(),
                          fluidRow(column(width = 6, downloadData(id = ns("myData"))), 
                                   column(width = 6, downloadChart(id = ns("myChart"))))
             ),
             mainPanel(width = widthMainPanel, uiOutput(ns("plotTitle")),
                       plotlyOutput(outputId = ns("trendsEducation"), width = "100%", height = 700))
             ),
             br(),
             hr(),
             footer)
  
}

trendsEducation_server <- function(id, urlParams = NULL) {
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
            if ("sex" %in% names(urlParams)) updateRadioButtons(session, "mySex", selected = urlParams$sex)
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
                                      "&sex=", input$mySex,
                                      "&logTrans=", input$myLogTrans), 
                    icon = icon("clipboard"))
      })
      
      # Plot title ---------------------------------------------------------------------------------------------------------------------------
      output$plotTitle <- renderUI({
        h3(paste0("Trend in ", 
                  deathMeasuresNames[deathMeasures == input$myMeasure], 
                  " of ", 
                  deathCauseLink$causeName[deathCauseLink$causeCode == input$myCause], 
                  " in ", 
                  input$myLHJ, 
                  " by Education Group, ", 
                  "2012 to ", maxYear, ", ", 
                  input$mySex, ", >24 years-old only, (crude age-adjustement)"))
      })
      
      # Render plot -------------------------------------------------------------------------------------------------------------------------
      myStep <- reactive(trendEducation(myLHJ = input$myLHJ, 
                                        myCause = input$myCause,
                                        mySex = input$mySex,
                                        myMeasure = input$myMeasure, 
                                        myLogTrans = input$myLogTrans))
      
      output$trendsEducation <- renderPlotly(myStep()$plotL_interactive)
      
      
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
      
     
    }
  )
}