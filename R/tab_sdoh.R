# sdoh_ui <- function(id, urlParams = NULL) {
#   ns <- NS(id)
#   
#   tabPanel(title = "Disparities", value = id,
#            fluidRow(
#              column(width = 3, 
#                     fluidRow(inputCause(id1 = ns("myCause"), id2 = ns("causeHelp"))),
#                     fluidRow(inputLHJ(id = ns("myLHJ")))),
#              column(width = 3, 
#                     fluidRow(inputCompare(id1 = ns("myCompare"), id2 = ns("compareHelp")))),
#              column(width = 3, 
#                     fluidRow(uiOutput(outputId = ns("clip"))), 
#                     fluidRow(tabInformation(id = ns("myTabInformation")))), 
#              column(width = 3, 
#                     fluidRow(downloadData(id = ns("myData"))), 
#                     fluidRow(downloadChart(id = ns("myChart"))))
#            ),
#            fluidRow(uiOutput(ns("plotTitle"))),
#            fluidRow(
#              column(width = 8, h4("Race/Ethnicity", style = "color: #164077"), plotlyOutput(ns("disparitiesRace"), width = "100%", height = 400)),
#              column(width = 4, h4("Sex", style = "color: #164077"), plotlyOutput(ns("disparitiesSex"), width = "100%", height = 400))
#            ), 
#            fluidRow(
#              column(width = 12, h4("Age Groups", style = "color: #164077"), plotlyOutput(ns("disparitiesAge"), width = "100%", height = 400))
#            ),
#            br(), 
#            hr(),
#            footer
#   )
# }
# 
# sdoh_server <- function(id, urlParams = NULL) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       
#       # Update inputs based on URL if custom URL is supplied ------------------------------------------------------------------------------
#       observe({
#         if (length(urlParams > 0)) {
#           if (urlParams$tab == id) {
#             if ("cause" %in% names(urlParams)) updateSelectizeInput(session, "myCause", selected = urlParams$cause)
#             if ("county" %in% names(urlParams)) updateSelectInput(session, "myLHJ", selected = urlParams$county)
#             if ("compare" %in% names(urlParams)) updateRadioButtons(session, "myCompare", selected = urlParams$compare)
#           }
#         }
#       })
#       
#       # Automatically generate URL to current view ----------------------------------------------------------------------------------------
#       output$clip <- renderUI({
#         rclipButton(inputId = "clipbtn", label = "Copy Link to Current View", 
#                     clipText = paste0(myURL, 
#                                       "?tab=", id,
#                                       "&cause=", input$myCause,
#                                       "&county=", input$myLHJ, 
#                                       "&compare=", input$myCompare), 
#                     icon = icon("clipboard"))
#       })
#       
#       output$plotTitle <- renderUI({
#         h3(paste0(myTitle <- paste0("Disparities in Death Rates, ", deathCauseLink$causeName[deathCauseLink$causeCode== input$myCause],
#                                     " in ", input$myLHJ, ", ", yearGrp3)), style = "text-align: center;")
#       })
#       
#       # Render plots  -------------------------------------------------------------------------------------------------
#       myStep <- reactive(disparity(myLHJ = input$myLHJ, 
#                                    myCause = input$myCause,
#                                    myCompare = input$myCompare))
#       
#       output$disparitiesRace <- renderPlotly(myStep()$racePlotL_interactive)
#       output$disparitiesSex <- renderPlotly(myStep()$sexPlotL_interactive)
#       output$disparitiesAge <- renderPlotly(myStep()$agePlotL_interactive)
#       
#       # Download data and chart -------------------------------------------------------------------------------------------------------------
#       output$myData <- downloadHandler(filename = function() { paste0(id, "-", input$myCause, "-", Sys.Date(), ".csv") },
#                                        content = function(file) {
#                                          write.csv(myStep()$dataL, file, row.names = FALSE)
#                                        })
#       
#       output$myChart <- downloadHandler(filename=function(){paste0(id,"-",input$myCause,"-",Sys.Date(),".jpg")},content = function(file) {
#         jpeg(file, width = 1200, height = 680)  # , pointsize = 20
#         print(myStep()$plot)
#         dev.off()
#       })
#       
#     }
#   )
# }