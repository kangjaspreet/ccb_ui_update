server <- function(input, output, session) {
  

  # User-supplied URL for linking to specific view ---------------------------------------------------------------------------------
  
  # Parse URL to pass through eaxh server module
  parseURL <- reactive({
    parseQueryString(session$clientData$url_search)
  })
  
  # Navigate to tab
  observe({
    print(parseURL())
    if (length(parseURL()) > 0) {
      if ("tab" %in% names(parseURL())) {
        tabID <- tabsLink %>% filter(sub_tabID == parseURL()$tab) %>% pull(tabID)
        updateTabsetPanel(session = session, inputId = "navsID", selected = tabID)
        updateTabsetPanel(session = session, inputId = paste0(tabID, "ID"), selected = parseURL()$tab)
      }
    }
  })
  
  # Server modules --------------------------------------------------------------------------------------------------------------------
  trendsSex_server(id = "sexTrendTab", urlParams = parseURL())
  trendsAge_server(id = "ageTrendTab", urlParams = parseURL())
  trendsRace_server(id = "raceTrendTab", urlParams = parseURL())
  trendsEducation_server(id = "educationTrendTab", urlParams = parseURL())
  trendsLifeExpectancy_server(id = "lifeExpectancyTrendTab", urlParams = parseURL())
  trendsLeadingCauses_server(id = "leadingCausesTrendTab", urlParams = parseURL())
  disparities_server(id = "disparitiesTab", urlParams = parseURL())
  demographics_server(id = "demographicsTab")
  
  
}