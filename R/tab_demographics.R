demographics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 4, inputLHJ(id = ns("myLHJ"))), 
      column(width = 4, inputYearDemo(id = ns("myYearDemo"))), 
      column(width = 4, uiOutput(outputId = ns("clip")), tabInformation(id = ns("myTabInformation")))
    ),
    hr(),
    fluidRow(
      column(width = 6, class = "demo", tabBox(id = ns("re_pie"), width = NULL,
                               tabPanel("Chart", plotlyOutput(ns("re_pie_chart"))), 
                               tabPanel("Table", dataTableOutput(ns("re_pie_table"))))),
      column(width = 6, class = "demo", tabBox(id = ns("pop_pyramid"), width = NULL,
                               tabPanel("Chart", plotlyOutput(ns("pop_pyramid_chart"))), 
                               tabPanel("Table", dataTableOutput(ns("pop_pyramid_table")))))),
    br(),
    fluidRow(
      column(width = 6, class = "demo", tabBox(id = ns("race_age"), width = NULL,
                               tabPanel("Chart", plotlyOutput(ns("race_age_chart"))), 
                               tabPanel("Table", dataTableOutput(ns("race_age_table"))))),
      column(width = 6, class = "demo", tabBox(id = ns("trends"), width = NULL,
                               tabPanel("Chart", plotlyOutput(ns("trends_chart"))), 
                               tabPanel("Table", dataTableOutput(ns("trends_table")))))),
    br(), 
    hr(),
    footer
    )
}

demographics_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Automatically generate URL to current view ----------------------------------------------------------------------------------------
      output$clip <- renderUI({
        rclipButton(inputId = "clipbtn", label = "Copy Link to Current View", 
                    clipText = paste0(myURL, 
                                      "?tab=", id, 
                                      "&county=", input$myLHJ, 
                                      "&year=", input$myYearDemo), 
                    icon = icon("clipboard"))
      })
      
      # Render plots and tables -------------------------------------------------------------------------------------------------
      demographics1Step <- reactive(make_demoPop_RacePie(myCounty = input$myLHJ, myYear = input$myYearDemo))
      demographics2Step <- reactive(make_demoPop_Pyramid(myCounty = input$myLHJ, myYear = input$myYearDemo))
      demographics3Step <- reactive(make_demoPop_RaceAge(myCounty = input$myLHJ, myYear = input$myYearDemo))
      
      # Race/Ethnicity pie
      output$re_pie_chart <- renderPlotly(demographics1Step()$plotL)
      output$re_pie_table <- renderDataTable(demographics1Step()$tableL, server = F)
      
      # Population Pyramid
      output$pop_pyramid_chart <- renderPlotly(demographics2Step()$plotL)
      output$pop_pyramid_table <- renderDataTable(demographics2Step()$tableL, server = F)
      
      # R/E Age
      output$race_age_chart <- renderPlotly(demographics3Step()$plotL)
      output$race_age_table <- renderDataTable(demographics3Step()$tableL, server = F)
      
      
    }
  )
}