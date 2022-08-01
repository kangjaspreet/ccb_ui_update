trendsLifeExpectancy_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Life Expectancy", value = id, 
           sidebarLayout(
             sidebarPanel(width = widthSidePanel, 
                          uiOutput(outputId = ns("clip")),
                          br(),
                          tabInformation(id = ns("myTabInformation")),
                          hr(),
                          selectLHJ(id = ns("myLHJ")),
                          checkboxGroupButtons(ns("mySexMult"), "Which Sex Groups?",
                                               choices = c("Total", "Male", "Female"),
                                               selected = c("Male", "Female"), individual=TRUE,size="sm", status = "multiSelectButtons"),
                          checkboxGroupButtons(ns("myRace"), "Which Race/Ethnic Groups?",
                                               choices = raceList,
                                               selected = raceList[!raceList %in% c("Multi-Race","Total","NH/PI","AI/AN")] , individual=TRUE,size="sm", status = "multiSelectButtons"),
                          radioButtons(ns("myYearGrouping"), "Years to Group:", choices=c(1,3,5), inline = TRUE),
                          checkboxInput(ns("myCI"), "95% CIs", value=FALSE),
                          hr(),
                          fluidRow(column(width = 6, downloadButton(ns("myData"), "Download Data")), 
                                   column(width = 6, downloadButton(ns("myChart"), "Download Chart")))
             ),
             mainPanel(width = widthMainPanel, plotOutput(ns("trendsLifeExpectancy"), width="100%",height = 700))
             ),
             br(),
             hr(),
             footer)
}


trendsLifeExpectancy_server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      myStep <- reactive(LEtrend(myLHJ = input$myLHJ, 
                                 mySexMult = input$mySexMult, 
                                 myRace = input$myRace,
                                 myCI = input$myCI,
                                 myYearGrouping = input$myYearGrouping))
      
      output$trendsLifeExpectancy  <- renderPlot(myStep()$plotL)
      
      output$clip <- renderUI({
        rclipButton(inputId = "clipbtn", label = "Copy Link to Current View", 
                    clipText = paste0("https://skylab.cdph.ca.gov/communityBurden/?tab=", id, 
                                      "?county=", input$myLHJ,
                                      "?yearGrouping=", input$myYearGrouping), 
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