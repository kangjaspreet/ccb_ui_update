trendsLeadingCauses_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Leading Causes", value = id, 
           sidebarLayout(
             sidebarPanel(width = widthSidePanel, 
                          uiOutput(outputId = ns("clip")),
                          br(),
                          tabInformation(id = ns("myTabInformation")),
                          hr(),
                          inputLHJ(id = ns("myLHJ")),
                          inputMeasure(id1 = ns("myMeasure"), id2 = ns("measureHelp"), choices = deathMeasures_Dropdown, selected = "aRate"),
                          inputLevel(id1 = ns("myLevShort"), id2 = "myLevelShortHelp"),
                          inputN(id = ns("myN")),
                          inputBroadGroups(id1 = ns("myBroadGroups"), id2 = ns("myBroadGroupHelp")),
                          inputYearRank(id = ns("myYearRank")),
                          inputYearRange(id = ns("myYearRange")),
                          inputLogTrans(id = ns("myLogTrans")),
                          noteSuppression(id = ns("mySuppressionNote")),
                          hr(),
                          fluidRow(column(width = 12, downloadChart(id = ns("myChart"))))
             ),
             mainPanel(width = widthMainPanel, plotOutput(ns("trendsLeadingCauses"), width="100%",height = 700))
           ),
           br(),
           hr(),
           footer)
}


trendsLeadingCauses_server <- function(id, urlParams = NULL) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      
      # Update inputs based on URL if custom URL is supplied ------------------------------------------------------------------------------
      observe({
        if (length(urlParams > 0)) {
          if (urlParams$tab == id) {
            if ("county" %in% names(urlParams)) updateSelectInput(session, "myLHJ", selected = urlParams$county)
            if ("measure" %in% names(urlParams)) updateSelectInput(session, "myMeasure", selected = urlParams$measure)
            if ("level" %in% names(urlParams)) updateRadioButtons(session, "myLevShort", selected = urlParams$level)
            if ("N" %in% names(urlParams)) updateNumericInput(session, "myN", value = as.numeric(urlParams$N))
            if ("broadGroup" %in% names(urlParams)) updateCheckboxGroupButtons(session, "myBroadGroups", selected = unlist(strsplit(urlParams$broadGroup, "[|]")))
            if ("yearRank" %in% names(urlParams)) updateSliderInput(session, "myYearRank", value = as.numeric(urlParams$yearRank))
            if ("yearRange" %in% names(urlParams)) updateSliderInput(session, "myYearRange", value = as.numeric(unlist(strsplit(urlParams$yearRange, "[-]"))))
            if ("logTrans" %in% names(urlParams)) updateCheckboxInput(session, "myLogTrans", value = as.logical(urlParams$logTrans))
          }
        }
      })
      
      # Automatically generate URL to current view ----------------------------------------------------------------------------------------
      output$clip <- renderUI({
        rclipButton(inputId = "clipbtn", label = "Copy Link to Current View", 
                    clipText = paste0(myURL, 
                                      "?tab=", id, 
                                      "&county=", input$myLHJ,
                                      "&measure=", input$myMeasure,
                                      "&level=", input$myLevShort,
                                      "&N=", input$myN,
                                      "&broadGroup=", paste(input$myBroadGroups, collapse = "|"),
                                      "&yearRank=", input$myYearRank,
                                      "&yearRange=", paste(input$myYearRange, collapse = "-"),
                                      "&logTrans=", input$myLogTrans),
                    icon = icon("clipboard"))
      })
      
      # Render plot -------------------------------------------------------------------------------------------------------------------------
      myStep <- reactive(topCauses_trends(myLHJ = input$myLHJ, 
                                          myMeasure = input$myMeasure,
                                          myLogTrans = input$myLogTrans, 
                                          myN = input$myN,
                                          myLev = input$myLevShort,
                                          myBroad = input$myBroadGroups,
                                          myYearRange = input$myYearRange,
                                          myYearRank = input$myYearRank))
      
      output$trendsLeadingCauses <- renderPlot(myStep()$plotL)
      
      
      # Download chart -------------------------------------------------------------------------------------------------------------
      output$myChart <- downloadHandler(filename = function() { paste0(id, "-", input$myLHJ, "-", Sys.Date(), ".png") },
                                        content = function(file) {
                                          png(file, width = 18, height = 10, units = "in", pointsize = 10, res = 100)
                                          print(myStep()$plotL)
                                          dev.off()
                                        })
      
      # Show/hide inputs based on Level selection ----------------------------------------------------------------------------------------------------
      observe({
        if (input$myLevShort == "lev1") {
          hide("myBroadGroups"); hide("myN"); hide("myYearRank")
        } else {
          show("myBroadGroups"); show("myN"); show("myYearRank")
        }
      })
      
    }
  )
}