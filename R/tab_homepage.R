home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "homeMainContent",
        # - CCB Anatomy ----------------------------------------------------------------------
        div(class = "anatomyRow", 
            fluidRow(
              column(width = 2, 
                     radioGroupButtons(inputId = ns("anatomy_menu_input"), 
                                       label = h4("Dyanmic Menu"), 
                                       choiceValues = as.list(homeAnatomyMenu$menuValue), 
                                       choiceNames = as.list(homeAnatomyMenu$menuName), 
                                       direction = "vertical", 
                                       justified = TRUE, 
                                       individual = TRUE, 
                                       status = "anatomyMenu")),
              column(width = 3, uiOutput(ns("anatomy_menu_info"))),
              column(width = 7, plotOutput(outputId = ns("preview_plot")))
            )),
        br(), br(),
        # - Featured Stories ------------------------------------------------------------------
        div(class = "featuredStoryRow",
            fluidRow(class = "test",
                     column(width = 3, feature_shaCM), 
                     column(width = 3, feature_xMDB), 
                     column(width = 3, feature_lghcChildhood), 
                     column(width = 3, feature_scoda)))
    )
  )
}

home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$preview_plot <- renderPlot({
      mtcars %>% 
        ggplot(aes(x = mpg, y = disp)) +
        geom_point()
    })
    
    output$anatomy_menu_info <- renderUI({
      HTML(paste0("<div class = 'bracket-dividers'>", 
             anatomyMenuInfo(selectedCategory = input$anatomy_menu_input),
             "</div>"
      ))
    })
    
  })
}