trendsAge_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Age Trend", value = id, 
           sidebarLayout(
             sidebarPanel(width = widthSidePanel, 
                          uiOutput(outputId = ns("clip")),
                          br(),
                          tabInformation(id = ns("myTabInformation")),
                          hr(),
                          selectCause(id1 = ns("myCause"), id2 = ns("causeHelp")),
                          selectLHJ(id = ns("myLHJ")),
                          selectMeasure(id1 = ns("myMeasure"), id2 = ns("measureHelp"), choices = deathMeasures_Dropdown_noADJ, selected = "cDeathRate"),
                          radioButtons(ns("myYearGrouping_race_age"), "Years to Group:", choices=c(1,3), inline = TRUE),
                          checkboxInput(ns("myLogTrans"),  "Log Transform of Y Axis", value=FALSE),
                          suppressionNote(id = ns("suppressionNote")),
                          hr(),
                          fluidRow(column(width = 6, downloadButton(ns("myData"), "Download Data")), 
                                   column(width = 6, downloadButton(ns("myChart"), "Download Chart")))
             ),
             mainPanel(width = widthMainPanel, plotOutput(ns("trendsAge"), width="100%",height = 700))
           ),
           br(),
           hr(),
           footer)
}


trendsAge_server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {

      myStep <- reactive(trendGeneric(myLHJ = input$myLHJ, 
                                      myCause = input$myCause,
                                      myMeasure = input$myMeasure,
                                      myTab = id,
                                      myYearGrouping_race_age = input$myYearGrouping_race_age,
                                      myLogTrans = input$myLogTrans))
      
      output$trendsAge  <- renderPlot(myStep()$plotL)
      
      output$clip <- renderUI({
        rclipButton(inputId = "clipbtn", label = "Copy Link to Current View", 
                    clipText = paste0("https://skylab.cdph.ca.gov/communityBurden/?tab=", id, 
                                      "?county=", input$myLHJ,
                                      "?cause=", input$myCause,
                                      "?measure=", input$myMeasure,
                                      "?yearGrouping=", input$myYearGrouping_race_age,
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