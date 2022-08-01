ui <- tagList(
  rclipboardSetup(),
  shinyjs::useShinyjs(),
  navbarPage(title = "CA Community Burden of Disease Engine",
                 id = "navsID",
                 # position = "fixed-top",
                 theme = "style.css",
                 tabPanel(title = strong("HOME"), value = "home"), 
                 tabPanel(title = strong("MAPS"), value = "maps", 
                          tabsetPanel(type = "tab", id = "mapsID", 
                                      tabPanel(title = "INTERACTIVE MAP", value = "interactiveMapTab"))),
                 tabPanel(title = strong("RANKS"), value = "ranks", 
                          tabsetPanel(type = "tab", id = "ranksID", 
                                      tabPanel(title = "RANK BY CAUSE - Deaths", value = "rankByCauseTab"), 
                                      tabPanel(title = "RANK BY GEOGRAPHY - Deaths", value = "rankByCauseTab"),
                                      tabPanel(title = "AGE RACE FOCUS", value = "ageRaceFocusTab"),
                                      tabPanel(title = "Death Hosp ED", value = "deathHospEDTab"),
                                      tabPanel(title = "Attributable Risks - IHME", value = "riskByCauseTab"),
                                      tabPanel(title = "Two-Year IHME Rankings", value = "arrowsTab")
                                      )), 
                 tabPanel(title = strong("TRENDS"), value = "trends",
                          tabsetPanel(type = "tab", id = "trendsID",
                                      trendsSex_ui(id = "sexTrendTab"),
                                      trendsAge_ui(id = "ageTrendTab"),
                                      trendsRace_ui(id = "raceTrendTab"),
                                      trendsEducation_ui(id = "educationTrendTab"),
                                      trendsLifeExpectancy_ui(id = "lifeExpectancyTrendTab"),
                                      trendsLeadingCauses_ui(id = "leadingCausesTrendTab")
                                      )), 
                 tabPanel(title = strong("DISPARITIES"), value = "disparities", 
                          tabsetPanel(type = "tab", id = "disparitiesID", 
                                      tabPanel(title = "Disparities", value = "disparitiesTab")
                                      )), 
                 tabPanel(title = strong("SDOH"), value = "sdoh", 
                          tabsetPanel(type = "tab", id = "sdohID", 
                                      tabPanel(title = "SOCIAL DETERMINANTS", value = "sdohTab")
                                      )), 
                 tabPanel(title = strong("HOSPITALIZATIONS"), value = "hospitalizations", 
                          tabsetPanel(type = "tab", id = "hospitalizationsID", 
                                      tabPanel(title = "HOSPITAL DISCHARGE", value = "hospitalDischargeTab"),
                                      tabPanel(title = "HOSPITAL DISCHARGE--PRIMARY AND ANY DIAGNOSES", value = "hospitalPrimaryAnyTab")
                                      )), 
                 tabPanel(title = strong("DEMOGRAPHICS"), value = "demographics", 
                          tabsetPanel(type = "tab", id = "demographicsID", 
                                      tabPanel(title = "Demographics", value = "demographicsTab")
                                      )), 
                 tabPanel(title = strong(" DATA TABLE"), value = "dataTableTab"), 
                 tabPanel(title = strong("ABOUT"), value = "about", 
                          tabsetPanel(type = "tab", id = "aboutID", 
                                      tabPanel(title = "OVERVIEW", value = "overviewTab"),
                                      tabPanel(title = "Technical Documentation", value = "techDocTab"),
                                      tabPanel(title = "Links to Other Data", value = "otherLinksTab"),
                                      tabPanel(title = "CCB URL Parameters", value = "urlParametersTab")
                                      ))
                 ), # END OF NAVBARPAGE
  tags$script(src="style.js")
) # END OF TAG LIST



