# RANKS
# rankByCauseTab
# rankByGeoTab



# TRENDS
# sexTrendTab            -> inputCause, inputLHJ, inputMeasure, inputYearGrouping, inputLogTrans, noteSuppression
# ageTrendTab            -> inputCause, inputLHJ, inputMeasure, inputYearGroupingRaceAge, inputLogTrans, noteSuppression
# raceTrendTab           -> inputCause, inputLHJ, inputMeasure, inputYearGroupingRaceAge, inputLogTrans, inputMultiRace, noteMultiRace, noteSuppression
# educationTrendTab      -> inputCause, inputLHJ, inputMeasure, inputSex, inputLogTrans, noteSuppression
# lifeExpectancyTrendTab -> inputLHJ, inputSexMultiSelect, inputRaceMultiSelect, inputYearGrouping, inputCI
# LeadingCausesTrendTab  -> inputLHJ, inputMeasure, inputLevel, inputN, inputBroadGroup, inputYearRank, inputYearRange, inputYearRange, noteSuppression

# DEMOGRAPHICS
# demographicsTab -> inputLHJ, inputYearDemo


downloadData <- function(id = NULL) {
  downloadButton(id, "Download Data")
}

downloadChart <- function(id = NULL) {
  downloadButton(id, "Download Chart")
}

tabInformation <- function(id = NULL) {
  actionButton(inputId = id, label = "Show Tab Information")
}

# Inputs ------------------------------------------------------------------------------------------------------------------------------------------
inputCause <- function(id1 = NULL, id2 = NULL) {
  dottedSelectInput(id1, label=list("Cause of Death:", actionButton(id2, label=helpIcon, style=myInputHelpButtonSty)), choices=fullList)
}

inputLHJ <- function(id = NULL) {
  selectInput(id, "County/State:", choices = lList, selected = STATE)
}

inputMeasure <- function(id1 = NULL, id2 = NULL, choices = deathMeasures_Dropdown, selected = "aRate") {
  selectInput(id1, label=list("Measure:", actionButton(id2, label=helpIcon,style=myInputHelpButtonSty)), choices = deathMeasures_Dropdown, selected = selected)
}

inputYearGrouping <- function(id = NULL) {
  radioButtons(id, "Years to Group:", choices=c(1,3,5), inline = TRUE)
}

inputYearGroupingRaceAge <- function(id = NULL) {
  radioButtons(id, "Years to Group:", choices=c(1,3), inline = TRUE)
}

inputLogTrans <- function(id = NULL) {
  checkboxInput(id,  "Log Transform of Y Axis", value=FALSE)
}

inputMultiRace <- function(id = NULL) {
  checkboxInput(id,  "Include Multirace Line", value=FALSE)
}

inputSex <- function(id = NULL) {
  radioButtons(id, "Sex:", choices=c("Total","Female","Male"), inline=TRUE)
}

inputSexMultiSelect <- function(id = NULL) {
  checkboxGroupButtons(id, "Which Sex Groups?",
                       choices = c("Total", "Male", "Female"),
                       selected = c("Male", "Female"), individual=TRUE,size="sm", status = "multiSelectButtons")
}

inputRaceMultiSelect <- function(id = NULL) {
  checkboxGroupButtons(id, "Which Race/Ethnic Groups?",
                       choices = raceList,
                       selected = raceList[!raceList %in% c("Multi-Race","Total","NH/PI","AI/AN")] , individual=TRUE,size="sm", status = "multiSelectButtons")
}

inputCI <- function(id = NULL) {
  checkboxInput(id, "95% CIs", value=FALSE)
}

inputLevel <- function(id1 = NULL, id2 = NULL) {
  radioButtons(id1, label=list("Levels to show:", actionButton(id2, label=helpIcon, style=myInputHelpButtonSty)),
               choices=c("Top" = "lev1", "Public Health" = "lev2"), inline=TRUE, selected = 'lev2')
}

inputN <- function(id = NULL) {
  numericInput(id,  "How many conditions:", value=5,min=1,max= 50)
} 

inputBroadGroups <- function(id1 = NULL, id2 = NULL) {
  checkboxGroupButtons(id1,
                       label = list("Select one or more broad condition group:", actionButton(id2, label=helpIcon, style=myInputHelpButtonSty_broadGroup)),
                       choices = c("All" = "0", "Communicable" = "A", "Cancer" = "B", "Cardiovascular" = "C", "Other Chronic" = "D", "Injury" = "E"),
                       selected = c("A", "B", "C", "D", "E"), individual=TRUE, size="sm", status = "multiSelectButtons")
}

inputYearRank <- function(id = NULL) {
  sliderInput(id, label = "Leading causes in which year?:", value = maxYear, min = minYear, max = maxYear,
              round = TRUE, sep = "", step = 1)
}

inputYearRange <- function(id = NULL) {
  sliderInput(id, label = "Year range To display:", min = minYear, max = maxYear, value = c(minYear, maxYear), sep = "", step = 1)
}

inputYearDemo <- function(id = NULL) {
  sliderInput(id, "Year:", value = maxYear, min = 2000, max = maxYear, animate = TRUE,
              round = TRUE, sep = "", step = 1)
}



# Notes -------------------------------------------------------------------------------------------------------------------------------------
noteSuppression <- function(id) {
  
  div(id = id,
      paste('Note: All measures associated with counts <', criticalNumber,', as well as necessary complementrary counts/measures are excluded for data de-identification purposes'),
      style="color:blue;font-size:12px;padding-left:5px;")
  
}

noteMultiRace <- function(id) {
  div(id = id, paste(multiRaceWarning, br()), style="color:red; font-size: 12px; padding-left: 5px; margin-bottom: 5px;")
}


