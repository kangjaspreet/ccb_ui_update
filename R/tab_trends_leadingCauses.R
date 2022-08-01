"topTrendsTab"=c("myLHJ", "myYearRank", "myYearRange", "myMeasure", "myLevShort", "myN_topTrends", "myBroadGroups", "myLogTrans", "suppressionNote")

trendsLeadingCauses_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Leading Causes", value = id, 
           sidebarLayout(
             sidebarPanel(width = widthSidePanel, 
                          uiOutput(outputId = ns("clip")),
                          br(),
                          tabInformation(id = ns("myTabInformation")),
                          hr(),
                          selectLHJ(id = ns("myLHJ")),
                          selectMeasure(id1 = ns("myMeasure"), id2 = ns("measureHelp"), choices = deathMeasures_Dropdown, selected = "aRate"),
                          radioButtons(ns("myLevShort"), label=list("Levels to show:", actionButton(ns("levelShortHelp"), label=helpIcon, style=myInputHelpButtonSty)),
                                       choices=c("Top" = "lev1", "Public Health" = "lev2"), inline=TRUE, selected = 'lev2'),
                          numericInput(ns("myN_topTrends"),  "How many conditions:", value=5,min=1,max= 50),
                          checkboxGroupButtons(ns("myBroadGroups"),
                                               label = list("Select one or more broad condition group:", actionButton(inputId=ns("broadGroupHelp"), label=helpIcon, style=myInputHelpButtonSty_broadGroup)),
                                               choices = c("All" = "0", "Communicable" = "A", "Cancer" = "B", "Cardiovascular" = "C", "Other Chronic" = "D", "Injury" = "E"),
                                               selected = c("A", "B", "C", "D", "E"), individual=TRUE, size="sm", status = "multiSelectButtons"),
                          sliderInput(ns("myYearRank"), label = "Leading causes in which year?:", value = maxYear, min = minYear, max = maxYear,
                                       round = TRUE, sep = "", step = 1),
                          sliderInput(ns("myYearRange"), label = "Year range To display:", min = minYear, max = maxYear, value = c(minYear, maxYear), sep = "", step = 1),
                          checkboxInput(ns("myLogTrans"),  "Log Transform of Y Axis", value=FALSE),
                          suppressionNote(id = ns("suppressionNote")),
                          hr(),
                          fluidRow(column(width = 12, downloadButton(ns("myChart"), "Download Chart")))
             ),
             mainPanel(width = widthMainPanel, plotOutput(ns("trendsLeadingCauses"), width="100%",height = 700))
           ),
           br(),
           hr(),
           footer)
}


trendsLeadingCauses_server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      myStep <- reactive(topCauses_trends(myLHJ = input$myLHJ, 
                                          myMeasure = input$myMeasure,
                                          myLogTrans = input$myLogTrans, 
                                          myN = input$myN_topTrends,
                                          myLev = input$myLevShort,
                                          myBroad = input$myBroadGroups,
                                          myYearRange = input$myYearRange,
                                          myYearRank = input$myYearRank))
      
      output$trendsLeadingCauses <- renderPlot(myStep()$plotL)
      
      output$clip <- renderUI({
        rclipButton(inputId = "clipbtn", label = "Copy Link to Current View", 
                    clipText = paste0("https://skylab.cdph.ca.gov/communityBurden/?tab=", id, 
                                      "?county=", input$myLHJ,
                                      "?measure=", input$myMeasure,
                                      "?logTrans=", input$myLogTrans,
                                      "&myN=", input$myN_topTrends, 
                                      "&myLev=", input$myLevShort,
                                      "&myBroad=", input$myBroadGroups,
                                      "&myYearRange=", input$myYearRange,
                                      "&myYearRank=", input$myYearRank), 
                    icon = icon("clipboard"))
      })
      
      output$myChart <- downloadHandler(filename = function() { paste0(id, "-", input$myLHJ, "-", Sys.Date(), ".png") },
                                        content = function(file) {
                                          png(file, width = 18, height = 10, units = "in", pointsize = 10, res = 100)
                                          print(myStep()$plotL)
                                          dev.off()
                                        })
      
      
      observe({
        if (input$myLevShort == "lev1") {
          hide("myBroadGroups"); hide("myN_topTrends"); hide("myYearRank")
        } else {
          show("myBroadGroups"); show("myN_topTrends"); show("myYearRank")
        }
      })
      
    }
  )
}