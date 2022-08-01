server <- function(input, output, session) {
  
  # Store current navID (big tabs) and tabID (subtabs) for use throughout Server
  current <- reactiveValues()
  
  trendsSex_server(id = "sexTrendTab")
  trendsAge_server(id = "ageTrendTab")
  trendsRace_server(id = "raceTrendTab")
  trendsEducation_server(id = "educationTrendTab")
  trendsLifeExpectancy_server(id = "lifeExpectancyTrendTab")
  trendsLeadingCauses_server(id = "leadingCausesTrendTab")
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    names(query) <- str_to_lower(names(query))
    storeQueryNames <- names(query)
    
    print(storeQueryNames)
    
  })

  
}