server <- function(input, output, session) {
  
  # Store current navID (big tabs) and tabID (subtabs) for use throughout Server
  current <- reactiveValues()
  
  
  # Trend ----------------------------------------------------------------------------------------------------
  
  # observe({
  #   if (input$myLHJ == "CALIFORNIA" & current$tab %in% c("ageTrendTab", "raceTrendTab")) shinyjs::show("myYearGrouping_race_age") else shinyjs::hide("myYearGrouping_race_age")
  # })
  # 
  # observe({
  #   print(input$myLHJ)
  #   print(input$myCAUSE)
  #   print(input$myMeasure)
  #   print(input$trendID)
  #   print(input$myYearGrouping)
  #   print(input$myYearGrouping_race_age)
  #   print(input$myLogTrans)
  #   print(input$myMultiRace)
  # })
  
  trendSex_server("sexTrendTab")
  

  
}