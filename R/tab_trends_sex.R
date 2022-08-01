trendsSex_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Sex Trend", value = id, 
           sidebarLayout(
             sidebarPanel(width = widthSidePanel,
                          uiOutput(outputId = ns("clip")),
                          br(),
                          tabInformation(id = ns("myTabInformation")),
                          hr(),
                          selectCause(id1 = ns("myCAUSE"), id2 = ns("causeHelp")),
                          selectLHJ(id = ns("myLHJ")),
                          selectMeasure(id1 = ns("myMeasure"), id2 = ns("measureHelp"), choices = deathMeasures_Dropdown, selected = "aRate"),
                          radioButtons(ns("myYearGrouping"), "Years to Group:", choices=c(1,3,5), inline = TRUE),
                          checkboxInput(ns("myLogTrans"),  "Log Transform of Y Axis", value=FALSE),
                          suppressionNote(id = ns("suppressionNote")),
                          hr(),
                          fluidRow(column(width = 6, downloadButton(ns("myData"), "Download Data")), 
                                   column(width = 6, downloadButton(ns("myChart"), "Download Chart")))
             ),
             mainPanel(width = widthMainPanel, plotOutput(ns("trendsSex"), width = "100%", height = 700))
           ), 
           br(), 
           hr(),
           footer)
}

trendsSex_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      myStep <- reactive(trendGeneric(myLHJ = input$myLHJ, 
                                      myCause = input$myCAUSE, 
                                      myMeasure = input$myMeasure, 
                                      myTab = id, 
                                      myYearGrouping = input$myYearGrouping, 
                                      myLogTrans = input$myLogTrans))
      
      output$trendsSex  <- renderPlot(myStep()$plotL)
      
      output$clip <- renderUI({
        rclipButton(inputId = "clipbtn", label = "Copy Link to Current View", 
                    clipText = paste0("https://skylab.cdph.ca.gov/communityBurden/?tab=", id, 
                                      "?county=", input$myLHJ, 
                                      "?measure=", input$myMeasure,
                                      "?yearGrouping=", input$myYearGrouping, 
                                      "?logTrans=", input$myLogTrans), 
                    icon = icon("clipboard"))
        })
      
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