
# LIST OF INPUTS IN EACH TAB ------------------------------------------------------------------------------------------------------------------------------------------------

TAB_INPUTS <- list("sexTrendTab" = c("input_myCAUSE", "input_myLHJ", "input_myMeasure", "input_myYearGrouping", "input_myLogTrans", "input_suppressionNote", "input_recentYearNote"), 
                   "ageTrendTab" = c("input_myCAUSE", "input_myLHJ", "input_myMeasure", "input_myYearGrouping_race_age", "input_myLogTrans", "input_suppressionNote"),
                   "raceTrendTab" = c("input_myCAUSE", "input_myLHJ", "input_myMeasure", "input_myYearGrouping_race_age", "input_myLogTrans", "input_myMultiRace", "input_suppressionNote")
)

# TAB_INPUTS <- list("homeTab"=c(),
#                    "aboutTab"=c(),
#                    "techDocTab"=c(),
#                    "otherLinksTab"=c(),
#                    "urlParametersTab"=c(),
#                    "lifeExpectancyTab"=c("ourDownloads","myLHJ","mySexMult","myRace","myYearGrouping","myCI"),
#                    "interactiveMapTab"=c("ourDownloads","myCAUSE","myLHJ","myGeo","mySex","myStateCut","myMeasure","myCutSystem", "suppressionNote", "recentYearNote"),   # "tabHelp", -- REMOVED FROM ALL
#                    "staticMapTab"=c("tabHelp", "myCAUSE", "myLHJ", "myGeo", "myMeasure", "myCutSystem", "myLabName", "mySex", "myStateCut"),
#                    "rankByCauseTab"=c("ourDownloads", "myLHJ", "mySex", "myLev", "myN", "myMeasureShort", "myMeanAge_sort", "suppressionNote", "recentYearNote"),
#                    "rankByCauseAndSexTab"=c("tabHelp", "myLev", "myN", "myMeasure"),
#                    "rankByGeographyTab"=c("ourDownloads","myCAUSE", "myLHJ", "mySex", "myMeasure", "myRefLine", "suppressionNote", "recentYearNote"),
#                    "ageRaceFocusTab" = c("ourDownloads", "myLHJ", "myData", "myStrata","mySort", "myMeasureAgeRaceFocus", "myLiveborn","myOlderFocus","myScale", "suppressionNote"),
#                    "deathHospEDTab" = c("ourDownloads", "myLHJ", "myStrata","mySort", "myMeasureAgeRaceFocus", "myLiveborn","suppressionNote"),
#                    "sexTrendTab"=c("ourDownloads", "myCAUSE", "myLHJ", "myMeasure", "myYearGrouping", "myLogTrans", "suppressionNote", "recentYearNote"),
#                    "ageTrendTab"=c("ourDownloads", "myCAUSE", "myLHJ", "myMeasure", "myYearGrouping_race_age", "myLogTrans", "suppressionNote"),
#                    "raceTrendTab"=c("ourDownloads","myCAUSE", "myLHJ", "myMeasure", "myYearGrouping_race_age", "myLogTrans", "myMultiRace", "suppressionNote"),
#                    "disparitiesTab"=c("ourDownloads","myCAUSE", "myLHJ", "myCompare","myAddN","myAddRate","myAddRR","suppressionNote"),
#                    "educationTrendTab"=c("ourDownloads", "myCAUSE", "myLHJ", "mySex", "myMeasure", "suppressionNote"),
#                    "topTrendsTab"=c("myLHJ", "myYearRank", "myYearRange", "myMeasure", "myLevShort", "myN_topTrends", "myBroadGroups", "myLogTrans", "suppressionNote"),
#                    "dataTableTab"=c("myLHJ", "suppressionNote"),
#                    "sdohTab"=c("myCAUSE", "myGeo_sdoh", "myMeasure", "myX", "suppressionNote"),
#                    "hospitalDischargeTab"=c("ourDownloads", "myLHJ", "mySex", "myN", "myOSHPDtype","myLiveborn"),
#                    # "MDC/DRGTab"=c("myLHJ", "mySex", "myN","myVar"),
#                    "hospitalPrimaryAnyTab"=c("ourDownloads", "myLHJ", "myPosition"),
#                    # "hospitalMapTab"=c("myCAUSE", "myLHJ", "mySex"),
#                    "arrowsTab"=c("display", "level", "measure", "yearRange", "sex", "metric"),
#                    "riskByCauseTab"=c("level", "year", "sex", "metric", "measure"), 
#                    "demographicsTab" = c("myLHJ", "myYearDemo")
# )

# INPUT WIDGETS ---------------------------------------------------------------------------------------------------------------------------------------------------

input_myCAUSE <- dottedSelectInput("myCAUSE", label=list("Cause of Death:", actionButton(inputId="causeHelp", label=helpIcon, style=myInputHelpButtonSty)), choices=fullList)

input_myLHJ <- selectInput("myLHJ","County/State:",choices=lList,selected=STATE)

input_myMeasure <- selectInput("myMeasure",  label=list("Measure:", actionButton( "measureHelp", label=helpIcon,style=myInputHelpButtonSty)),
            choices=deathMeasures_Dropdown, selected = "aRate")

input_myYearGrouping <- radioButtons("myYearGrouping", "Years to Group:", choices=c(1,3,5), inline = TRUE)

input_myLogTrans <- checkboxInput("myLogTrans",  "Log Transform of Y Axis", value=FALSE)

input_suppressionNote <- div(id="suppressionNote",
    paste('Note: All measures associated with counts <',criticalNumber,', as well as necessary complementrary counts/measures are excluded for data de-identification purposes'),style="color:blue;font-size:12px;padding-left:5px;"
)

input_recentYearNote <- div(id="recentYearNote",
    paste('Note: Data for', currentYear, 'are not yet final. Number of deaths are likely to increase slightly.  Some cause of death codes will become more accurate. These changes are not expected to significantly impact the interpretation of any observed noteworthy patterns or trends.'),
    style="color:blue;font-size:12px;padding-left:5px;"
)


input_myGeo <- selectInput("myGeo","Geographic Level:", choices=c("County","Community","Census Tract"))

input_myGeoHelpText <- div(id = "myGeoHelpText", helpText(h6(appTextL$tractWarning,style="color:red; float:left; margin: 20px;")))

input_myGeo_sdoh <- selectInput("myGeo_sdoh","Geographic Level:", choices=c("County","Community"))

input_myYear <- sliderInput("myYear","Year:",value=maxYear,min=2001,max=maxYear,animate = TRUE, round=TRUE,sep="",step=1)  #can use value=c(2017,2017)

input_myYearDemo <- sliderInput("myYearDemo","Year:",value=maxYear,min=2000,max=maxYear,animate = TRUE, round=TRUE,sep="",step=1)

input_mySex <- radioButtons("mySex", "Sex:", choices=c("Total","Female","Male"), inline=TRUE)

input_myLev <- radioButtons("myLev", label=list("Levels to show:", actionButton("levelHelp", label=helpIcon, style=myInputHelpButtonSty)),
             choices=c("Top" = "lev1","Public Health" = "lev2","Detail" = "lev3"), inline=TRUE, selected = 'lev2')

input_myLevShort <- radioButtons("myLevShort", label=list("Levels to show:", actionButton("levelShortHelp", label=helpIcon, style=myInputHelpButtonSty)),
                                 choices=c("Top" = "lev1","Public Health" = "lev2"), inline=TRUE, selected = 'lev2')

input_myBroadGroups <- checkboxGroupButtons( "myBroadGroups",
                      label = list("Select one or more broad condition group:", actionButton(inputId="broadGroupHelp", label=helpIcon, style=myInputHelpButtonSty_broadGroup)),
                      choices = c("All" = "0", "Communicable" = "A", "Cancer" = "B", "Cardiovascular" = "C", "Other Chronic" = "D", "Injury" = "E"),
                      selected = c("A", "B", "C", "D", "E"), individual=TRUE, size="sm")


input_myStateCut <- checkboxInput("myStateCut", label=list("State-based cutpoints", actionButton("stateCutHelp", label=helpIcon, style=myInputHelpButtonSty)),
              value=TRUE)

input_myN <- numericInput( "myN",  "How Many:", value=10,min=1,max= 50)

input_myN_topTrends <- numericInput( "myN_topTrends",  "How many conditions:", value=5,min=1,max= 50)

input_myMeasureShort <- selectInput("myMeasureShort",  "Measure Sort Order:",  
                                   choices=deathMeasuresShort_Dropdown, selected= "aRate")

input_myMeanAge_sort <- radioButtons("myMeanAge_sort",  'Sort "Mean Age at Death" from:',  choices=mean_age_sort)

input_myYearGrouping_race_age <- radioButtons("myYearGrouping_race_age", "Years to Group:", choices=c(1,3), inline = TRUE)

input_myData <- selectInput("myData","Data Type:", choices=c("Deaths", "Hospitalizations","Emergency Department"))  

input_myStrata <- selectInput("myStrata","Grouping Variable:", choices=c("Age Group", "Race/Ethnicity")) 

input_mySort <- selectInput( "mySort", "Sort by?", choices = raceSort, selected = "White")

input_myOlderFocus <- checkboxInput("myOlderFocus", label="Older Adult Focus",value=FALSE)

input_myLiveborn <- checkboxInput("myLiveborn", list(label="Include Births", actionButton("includeBirthsHelp", label=helpIcon, style=myInputHelpButtonSty)), value=FALSE)

input_myMeasureAgeRaceFocus <- selectInput("myMeasureAgeRaceFocus",  list(label="Measure:", actionButton("measureAgeRaceFocusHelp", label=helpIcon, style=myInputHelpButtonSty)),
            choices=c("Number"="N","Crude Rate"="cRate","Adjusted Rate"="aRate"), selected="cRate")

input_myScale <- selectInput("myScale",  label= list("Scale:", actionButton("axisScaleHelp", label=helpIcon,style=myInputHelpButtonSty)),
            choices=c("fixed","free"))

input_myCutSystem <- radioButtons("myCutSystem",label=list("Cut-point method:", actionButton("cutmethodHelp", label=helpIcon,style=myInputHelpButtonSty)),
             choices=c("quantile","fisher"))  # pretty

input_myLabName <- checkboxInput("myLabName",  "Place Names", value=FALSE)

input_myCI <- checkboxInput("myCI",       "95% CIs", value=FALSE)

input_myRefLine <- checkboxInput("myRefLine",  "Reference Line", value=FALSE)

input_myLogTrans <- checkboxInput("myLogTrans",  "Log Transform of Y Axis", value=FALSE)

input_myYearRank <- sliderInput( "myYearRank", label = "Leading causes in which year?:", value = maxYear, min = minYear, max = maxYear,
             round = TRUE, sep = "", step = 1)

input_myYearRange <- sliderInput( "myYearRange", label = "Year range To display:", min = minYear, max = maxYear, value = c(minYear, maxYear), sep = "", step = 1)

input_myMultiRace <- checkboxInput("myMultiRace",  "Include Multirace Line", value=FALSE)

input_myMultiRaceHelpText <- div(id = "myMultiRaceHelpText", helpText(h6(multiRaceWarning,style="color:red; float:left; margin: 20px;")))

input_myCompare <- radioButtons("myCompare", list(label = "Compare to group with:", actionButton("disparityCompareHelp", label = helpIcon, style = myInputHelpButtonSty)), 
             choices=c("lowest rate","highest rate"))

input_myAddN <- checkboxInput("myAddN",  "Show NUMBER of Deaths?", value=FALSE)

input_myAddRate <- checkboxInput("myAddRate",  "Show Rate?", value=FALSE)      

input_myAddRR <- checkboxInput("myAddRR",  "Show Rate Ratio?", value=FALSE)

input_myLifeRace <- checkboxInput("myLifeRace",  "Show Race/Ethnicity Detail?", value=FALSE)

input_myRace <- checkboxGroupButtons( "myRace", "Which Race/Ethnic Groups?",
                      choices = raceList,
                      selected = raceList[!raceList %in% c("Multi-Race","Total","NH/PI","AI/AN")],individual=TRUE,size="sm")

input_mySexMult <- checkboxGroupButtons( "mySexMult", "Which Sex Groups?",
                      choices = c("Total", "Male", "Female"),
                      selected = c("Male", "Female"), individual=TRUE,size="sm")

# input_myXselectInput("myX", "Social Determinant of Health Variable:", choices=sdohVec, selected="pov")

input_myOSHPDtype <- selectInput( "myOSHPDtype", "Measure Sort Order:", choices = hMNames_short)

input_myOSHPDtype_mdcdrg <- selectInput( "myOSHPDtype_mdcdrg", "Measure Sort Order:", choices = hMDCDrop_down)

input_myPosition <- selectInput( "myPosition", "Sort Order:", choices = listPosition)

input_myprimetype <- selectInput("myprimetype", "Variable", choice = c("any", "primary"))

input_level <- sliderInput( "level", label = "Level:", min = 0, max = 4, value = 2)

input_year <- sliderInput( "year", label = "Year:", min = min(VALID_YEARS), max = max(VALID_YEARS), value = max(VALID_YEARS), sep = "", step = 1) # animate = animationOptiointerval = 3000)

input_yearRange <- sliderTextInput( "yearRange", label = "Years:", choices = as.character(VALID_YEARS), selected = range(VALID_YEARS))  # grid=TRUE

input_display <- radioButtons("display",label = "Display:",choices = c("Cause" = "cause", "Risk" = "risk"), inline=TRUE)

input_sex <- radioButtons("sex", "Sex:", choices=c("Both" = 3,"Female" = 2,"Male" = 1), inline=TRUE)

input_metric <- radioButtons("metric",label = "Metric:",choices = c("Number" = 1, "Percent" = 2, "Rate" = 3), inline=TRUE)

input_measure <- selectInput( "measure", label = "Measure:", choices = c("Deaths" = 1,
                                                        "Disability Adjusted Life Years (DALYs)" = 2,
                                                        "Years Lived with Disability (YLDs)" = 3,
                                                        "Years of Life Lost (YLLs)" = 4), selected = 1)

input_suppressionNote <- div(id="suppressionNote",
    paste('Note: All measures associated with counts <',criticalNumber,', as well as necessary complementrary counts/measures are excluded for data de-identification purposes'),style="color:blue;font-size:12px;padding-left:5px;"
)

input_recentYearNote <- div(id="recentYearNote",
    paste('Note: Data for', currentYear, 'are not yet final. Number of deaths are likely to increase slightly.  Some cause of death codes will become more accurate. These changes are not expected to significantly impact the interpretation of any observed noteworthy patterns or trends.'),
    style="color:blue;font-size:12px;padding-left:5px;")


sidebarPanel_custom <- function(tabID = "sexTrendTab", mainPanelCode) {
  
  myTagList <- tagList()
  i <- 1
  for (element in TAB_INPUTS[[tabID]]) { myTagList[[i]] <- get(element); i <- i + 1 }
  
  sidebarPanel(width = 4) 
  
}


sidebarPanel_custom1 <- function(tabID = "sexTrendTab", mainPanelCode) {
  
  myTagList <- tagList()
  i <- 1
  for (element in TAB_INPUTS[[tabID]]) { myTagList[[i]] <- get(element); i <- i + 1 }
  
  sidebarPanel(actionButton("myTabInformation", "Show Tab Information"),
               hr(),
               myTagList, 
               hr(),
               fluidRow(column(width = 6, downloadButton("myDownloadData")), 
                        column(width = 6, downloadButton("myDownloadChart"))),
               width = 4) 
  
}