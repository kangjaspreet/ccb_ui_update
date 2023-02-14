home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "homeMainContent",
        
        # - CDPH logo + CCB Power Statement --------------------------------------------------
        div(class = "statementRow", 
            fluidRow(
              column(width = 2, img(src = 'Logos/CDPH_Logo1.jpg', height = "auto", align = "left")),
              column(width = 6, 
                     h3("About the California Community Burden of Disease Engine"), 
                     "A dynamic, interactive data visualization tool displaying mortality, morbidity, 
                     social determinants of health, risk factors, and population data at multiple levels of 
                     geographic granularity with a focus on disparities to inform public health action and policy."),
              column(width = 4)
            )),
        br(),
        # - CCB Anatomy ----------------------------------------------------------------------
        div(class = "anatomyRow", 
            fluidRow(
              column(width = 2, 
                     radioGroupButtons(inputId = ns("anatomy_menu_input"), 
                                       label = h4("Dynamic Menu"), 
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
        
        div(class = "anatomyRow2", 
            
            fluidRow(
              column(
                width = 12,
                verticalTabsetPanel(
                  id = ns("my_vertical_tab_panel"),
                  color = "#9E8DE3",
                  contentWidth = 10,
                  verticalTabPanel(
                    title = "MAPS",
                    box_height = "auto",
                    column(width = 4, "Control panel 1"), 
                    column(width = 8, plotOutput(outputId = ns("preview_plot_maps"), height = "100%")),
                  ),
                  verticalTabPanel(
                    title = "TRENDS",
                    box_height = "auto",
                    column(width = 4, "Control panel 1"), 
                    column(width = 8, plotOutput(outputId = ns("preview_plot_trends"), height = "100%")),
                  ),
                  verticalTabPanel(
                    title = "RANK BY CAUSE",
                    box_height = "auto",
                    column(width = 4, "Control panel 1"), 
                    column(width = 8, plotOutput(outputId = ns("preview_plot_rankByCause"), height = "100%")),
                  ), 
                  verticalTabPanel(
                    title = "RANK BY GEOGRAPHY",
                    box_height = "auto",
                    column(width = 4, "Control panel 1"), 
                    column(width = 8, plotOutput(outputId = ns("preview_plot_rankByGeography"), height = "100%")),
                  ),
                  verticalTabPanel(
                    title = "AGE RACE FOCUS",
                    box_height = "auto",
                    column(width = 4, "Control panel 1"), 
                    column(width = 8, plotOutput(outputId = ns("preview_plot_ageRaceFocus"), height = "100%")),
                  ),
                  verticalTabPanel(
                    title = "DEATH HOSP ED",
                    box_height = "auto",
                    column(width = 4, "Control panel 1"), 
                    column(width = 8, plotOutput(outputId = ns("preview_plot_deathHospED"), height = "100%")),
                  ),
                  verticalTabPanel(
                    title = "HOSPITALIZATIONS",
                    box_height = "auto",
                    column(width = 4, "Control panel 1"), 
                    column(width = 8, plotOutput(outputId = ns("preview_plot_hospitalizations"), height = "100%")),
                  ),
                  verticalTabPanel(
                    title = "DISPARITIES",
                    box_height = "auto",
                    column(width = 4, "Control panel 1"), 
                    column(width = 8, plotOutput(outputId = ns("preview_plot_disparities"), height = "100%")),
                  ),
                  verticalTabPanel(
                    title = "SDOH",
                    box_height = "auto",
                    column(width = 4, "Control panel 1"), 
                    column(width = 8, plotOutput(outputId = ns("preview_plot_sdoh"), height = "100%")),
                  ),
                  verticalTabPanel(
                    title = "DEMOGRAPHICS",
                    box_height = "auto",
                    column(width = 4, "Control panel 1"), 
                    column(width = 8, plotOutput(outputId = ns("preview_plot_demographics"), height = "100%")),
                  )
                )
              )
            )
        ),
        
        br(), br(),
        
        # - CCB Information (Updates, About, SHA/SHIP, etc) Row -------------------------------
        div(class = "infoRow", 
            fluidRow(
              column(width = 2, 
                     radioGroupButtons(inputId = ns("info_menu_input"), 
                                       label = NULL, 
                                       choiceValues = as.list(homeInfo$menuValue), 
                                       choiceNames = as.list(homeInfo$menuName), 
                                       direction = "vertical", 
                                       justified = TRUE, 
                                       individual = TRUE, 
                                       status = "anatomyMenu")),
              column(width = 1),
              column(width = 9, uiOutput(ns("menu_info")))
            )),
        br(), br(),
        
        # - Featured Stories ------------------------------------------------------------------
        div(class = "featuredStoryRow",
            fluidRow(class = "test",
                     column(width = 3, feature_shaCM), 
                     column(width = 3, feature_xMDB), 
                     column(width = 3, feature_lghcChildhood), 
                     column(width = 3, feature_scoda))),
        
        br(), 
        hr(),
        footer
    )
  )
}

home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # - Anatomy - Plot Preview -------------------------------------
    output$preview_plot <- renderPlot({
      mtcars %>% 
        ggplot(aes(x = mpg, y = disp)) +
        geom_point()
    })
    
    # - Anatomy 2 - Plot Preview -----------------------------------
    observeEvent(input$my_vertical_tab_panel, {
      
      if (input$my_vertical_tab_panel == "MAPS") {
        
        output$preview_plot_maps <- renderPlot({
          mtcars %>% 
            ggplot(aes(x = mpg, y = disp)) +
            geom_point()
        })
        
      } else if (input$my_vertical_tab_panel == "TRENDS") {
        
        output$preview_plot_trends <- renderPlot({
          mtcars %>% 
            ggplot(aes(x = mpg, y = disp)) +
            geom_point()
        })
        
      } else if (input$my_vertical_tab_panel == "RANK BY CAUSE") {
        
        output$preview_plot_rankByCause <- renderPlot({
          mtcars %>% 
            ggplot(aes(x = mpg, y = disp)) +
            geom_point()
        })
        
      } else if (input$my_vertical_tab_panel == "RANK BY GEOGRAPHY") {
        
        output$preview_plot_rankByGeography <- renderPlot({
          mtcars %>% 
            ggplot(aes(x = mpg, y = disp)) +
            geom_point()
        })
        
      } else if (input$my_vertical_tab_panel == "AGE RACE FOCUS") {
        
        output$preview_plot_ageRaceFocus <- renderPlot({
          mtcars %>% 
            ggplot(aes(x = mpg, y = disp)) +
            geom_point()
        })
        
      } else if (input$my_vertical_tab_panel == "DEATH HOSP ED") {
        
        output$preview_plot_deathHospED <- renderPlot({
          mtcars %>% 
            ggplot(aes(x = mpg, y = disp)) +
            geom_point()
        })
        
      } else if (input$my_vertical_tab_panel == "HOSPITALIZATIONS") {
        
        output$preview_plot_hospitalizations <- renderPlot({
          mtcars %>% 
            ggplot(aes(x = mpg, y = disp)) +
            geom_point()
        })
        
      } else if (input$my_vertical_tab_panel == "DISPARITIES") {
        
        output$preview_plot_disparities <- renderPlot({
          mtcars %>% 
            ggplot(aes(x = mpg, y = disp)) +
            geom_point()
        })
        
      } else if (input$my_vertical_tab_panel == "SDOH") {
        
        output$preview_plot_sdoh <- renderPlot({
          mtcars %>% 
            ggplot(aes(x = mpg, y = disp)) +
            geom_point()
        })
        
      } else if (input$my_vertical_tab_panel == "DEMOGRAPHICS") {
        
        output$preview_plot_demographics <- renderPlot({
          mtcars %>% 
            ggplot(aes(x = mpg, y = disp)) +
            geom_point()
        })
        
      }
      
    })
    
    # - Anatomy - Bulleted list ------------------------------------
    output$anatomy_menu_info <- renderUI({
      HTML(paste0("<div class = 'bracket-dividers'>", 
             anatomyMenuInfo(selectedCategory = input$anatomy_menu_input),
             "</div>"
      ))
    })
    
    # - Information Row ------------------------------------------
    output$menu_info <- renderUI({
      HTML(menuInfo(selectedCategory = input$info_menu_input))
    })
    
  })
}