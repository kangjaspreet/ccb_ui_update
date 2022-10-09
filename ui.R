ui <- tagList(
  rclipboardSetup(),
  shinyjs::useShinyjs(),
  tags$style("@import url(https://use.fontawesome.com/releases/v6.2.0/css/all.css);"), # Import current version of FontAwesome
  HTML("<div class = 'headerFooterBars'> <span> CALIFORNIA COMMUNITY BURDEN OF DISEASE ENGINE </span> </div>"),
  navbarPage(title = NULL,
                 id = "navsID",
                 # position = "fixed-top",
                 theme = "style.css",
                 tabPanel(title = "HOME", value = "home", icon = icon("house", class = "fa-2x"), home_ui(id = "homeTab")), 
                 tabPanel(title = strong("MAPS"), value = "maps", icon = icon("map", class = "fa-2x"), 
                          tabsetPanel(type = "tab", id = "mapsID", 
                                      tabPanel(title = "INTERACTIVE MAP", value = "interactiveMapTab"))),
                 tabPanel(title = strong("RANKS"), value = "ranks", icon = icon("chart-simple", class = "fa-2x"), 
                          tabsetPanel(type = "tab", id = "ranksID", 
                                      tabPanel(title = "RANK BY CAUSE - Deaths", value = "rankByCauseTab"), 
                                      tabPanel(title = "RANK BY GEOGRAPHY - Deaths", value = "rankByCauseTab"),
                                      tabPanel(title = "AGE RACE FOCUS", value = "ageRaceFocusTab"),
                                      tabPanel(title = "Death Hosp ED", value = "deathHospEDTab"),
                                      tabPanel(title = "Attributable Risks - IHME", value = "riskByCauseTab"),
                                      tabPanel(title = "Two-Year IHME Rankings", value = "arrowsTab")
                                      )), 
                 tabPanel(title = strong("TRENDS"), value = "trends", icon = icon("chart-line", class = "fa-2x"),
                          tabsetPanel(type = "tab", id = "trendsID",
                                      trendsSex_ui(id = "sexTrendTab"),
                                      trendsAge_ui(id = "ageTrendTab"),
                                      trendsRace_ui(id = "raceTrendTab"),
                                      trendsEducation_ui(id = "educationTrendTab"),
                                      trendsLifeExpectancy_ui(id = "lifeExpectancyTrendTab"),
                                      trendsLeadingCauses_ui(id = "leadingCausesTrendTab")
                                      )), 
                 tabPanel(title = strong("DISPARITIES"), value = "scale-unbalanced", icon = icon("house", class = "fa-2x"),
                          tabsetPanel(type = "tab", id = "disparitiesID", 
                                      disparities_ui(id = "disparitiesTab"))),
                 tabPanel(title = strong("SDOH"), value = "sdoh", icon = icon("people-arrows", class = "fa-2x"),
                          tabsetPanel(type = "tab", id = "sdohID", 
                                      tabPanel(title = "SOCIAL DETERMINANTS", value = "sdohTab")
                                      )), 
                 tabPanel(title = strong("HOSPITALIZATIONS"), value = "hospitalizations", icon = icon("stethoscope", class = "fa-2x"),
                          tabsetPanel(type = "tab", id = "hospitalizationsID", 
                                      tabPanel(title = "HOSPITAL DISCHARGE", value = "hospitalDischargeTab"),
                                      tabPanel(title = "HOSPITAL DISCHARGE--PRIMARY AND ANY DIAGNOSES", value = "hospitalPrimaryAnyTab")
                                      )), 
                 tabPanel(title = strong("DEMOGRAPHICS"), value = "demographics", icon = icon("share-nodes", class = "fa-2x"),
                          demographics_ui(id = "demographicsTab")),
                 tabPanel(title = strong(" DATA TABLE"), value = "dataTableTab", icon = icon("table", class = "fa-2x")), 
                 tabPanel(title = strong("ABOUT"), value = "about", icon = icon("circle-info", class = "fa-2x"), 
                          tabsetPanel(type = "tab", id = "aboutID", 
                                      tabPanel(title = "OVERVIEW", value = "overviewTab"),
                                      tabPanel(title = "Technical Documentation", value = "techDocTab"),
                                      tabPanel(title = "Links to Other Data", value = "otherLinksTab"),
                                      tabPanel(title = "CCB URL Parameters", value = "urlParametersTab")
                                      ))
                 ), # END OF NAVBARPAGE
  tags$script(src="style.js")
) # END OF TAG LIST



