trendsLifeExpectancy_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Life Expectancy", value = id, 
           sidebarLayout(
             sidebarPanel(width = widthSidePanel, 
                          uiOutput(outputId = ns("clip")),
                          br(),
                          tabInformation(id = ns("myTabInformation")),
                          hr(),
                          inputLHJ(id = ns("myLHJ")),
                          inputSexMultiSelect(id = ns("mySexMult")),
                          inputRaceMultiSelect(id = ns("myRaceMult")),
                          inputYearGrouping(id = ns("myYearGrouping")),
                          inputCI(id = ns("myCI")),
                          hr(),
                          fluidRow(column(width = 6, downloadData(id = ns("myData"))), 
                                   column(width = 6, downloadChart(id = ns("myChart"))))
             ),
             mainPanel(width = widthMainPanel, uiOutput(ns("plotTitle")), 
                       plotlyOutput(ns("trendsLifeExpectancy"), width="100%",height = 700))
             ),
             br(),
             hr(),
             footer)
}


trendsLifeExpectancy_server <- function(id, urlParams = NULL) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      # Update inputs based on URL if custom URL is supplied ------------------------------------------------------------------------------
      observe({
        if (length(urlParams > 0)) {
          if (urlParams$tab == id) {
            if ("county" %in% names(urlParams)) updateSelectInput(session, "myLHJ", selected = urlParams$county)
            if ("measure" %in% names(urlParams)) updateSelectInput(session, "myMeasure", selected = urlParams$measure)
            if ("sex" %in% names(urlParams)) updateCheckboxGroupButtons(session, "mySexMult", selected = unlist(strsplit(urlParams$sex, "[|]")))
            if ("race" %in% names(urlParams)) updateCheckboxGroupButtons(session, "myRaceMult", selected = unlist(strsplit(urlParams$race, "[|]")))
            if ("yearGrouping" %in% names(urlParams)) updateRadioButtons(session, "myYearGrouping", selected = as.numeric(urlParams$yearGrouping))
            if ("CI" %in% names(urlParams)) updateCheckboxInput(session, "myCI", value = as.logical(urlParams$CI))
            }
        }
      })
      
      # Automatically generate URL to current view ----------------------------------------------------------------------------------------
      output$clip <- renderUI({
        rclipButton(inputId = "clipbtn", label = "Copy Link to Current View", 
                    clipText = paste0(myURL, 
                                      "?tab=", id,
                                      "&county=", input$myLHJ,
                                      "&sex=", paste(input$mySexMult, collapse = "|"),
                                      "&race=", paste(input$myRaceMult, collapse = "|"),
                                      "&yearGrouping=", input$myYearGrouping,
                                      "&CI=", input$myCI), 
                    icon = icon("clipboard"))
      })
      
      # Render plot title -----------------------------------------------------------------------------------------------------------------
      output$plotTitle <- renderUI({
        h3(paste0("Trends in Life Expectancy, ", input$myLHJ, ", ", minYear_LT, "-", maxYear_LT))
      })
      
      # Render plot -------------------------------------------------------------------------------------------------------------------------
      myStep <- reactive(LEtrend(myLHJ = input$myLHJ, 
                                 mySexMult = input$mySexMult, 
                                 myRace = input$myRaceMult,
                                 myCI = input$myCI,
                                 myYearGrouping = input$myYearGrouping))
      
      output$trendsLifeExpectancy  <- renderPlotly(myStep()$plotL_interactive)
      
      
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