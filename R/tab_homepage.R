home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "homeMainContent",
        # - CCB Anatomy ----------------------------------------------------------------------
        div(class = "anatomyRow", 
            fluidRow(
              column(width = 2, 
                     radioGroupButtons(inputId = ns("menu_inputs"), 
                                       label = h4("Dyanmic Menu"), 
                                       choiceValues = list("mapsTab", "trendsTab", "rankByCauseTab", 
                                                           "rankByGeographyTab", "ageRaceFocusTab", "deathHospEdTab", 
                                                           "hospitalizationsTab", "disparitiesTab", "sdohTab", 
                                                           "demographicsTab"), 
                                       choiceNames = list("MAPS", "TRENDS", "RANK BY CAUSE", "RANK BY GEOGRAPHY", 
                                                          "AGE RACE FOCUS", "DEATH HOSP ED", "HOSPITALIZATIONS", 
                                                          "DISPARITIES", "SDOH", "DEMOGRAPHICS"), 
                                       direction = "vertical", 
                                       justified = TRUE, 
                                       individual = TRUE, 
                                       status = "anatomyMenu")),
              column(width = 3, HTML("<div class = 'bracket-dividers'>
                                        <ul>
                                          <li>Geography
                                            <ul class='secondLevel'>
                                              <li><strong>State</strong></li>
                                              <li>County</li>
                                            </ul>
                                          </li>
                                          <li>Demographics
                                            <ul class='secondLevel'>
                                              <li>Sex</li>
                                              <li>Age</li>
                                              <li><strong>Race/Ethnicity</strong></li>
                                              <li>Educational Attainment</li>
                                            </ul>
                                          </li>
                                          <li>Causes of Death</li>
                                          <li>Death measures
                                            <ul class='secondLevel'>
                                              <li>Numbers</li>
                                              <li><strong>Adjusted Rates</strong></li>
                                              <li>Years of Life Lost</li>
                                            </ul>
                                          </li>
                                        </ul>
                                      </div>")),
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
    
  })
}