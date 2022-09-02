# =============================================================================
# "Global.R" file     
#
#   Core file for Shiny Application
#
#   Loads standards; Set Paths
#   Designates global constants for application
#   Loads all packages needed for application                                                           
#   Reads in datasets and info files needed for application                                                          
#   Load functions used to create visualizations
#   Loads and creates app text files/objects
#   Creates vectors used for Shiny app inputs   
#
# 
#   Michael Samuel, Jaspreet Kang
#   2021
#
# =============================================================================

# - 1. LOAD STANDARDS & SET PATHS ----------------------------------------------------------------------------------------------------
CCB         <- TRUE
server      <- TRUE
myPlace     <- getwd() # ERROR IF THIS ISNT HERE. DETERMINE AN APPROACH WHERE THIS ISNT NEEDED

source(paste0(myPlace, "/Standards/FusionStandards.R")) # This works when specifying the full path, but not the relative path??????

raceLink    <- select(raceLink, raceCode,raceName,raceNameShort)

# IN MULTIBAR FUNCTION
raceSort <- raceLink %>%
  filter(!raceCode %in% c("Other", "Unknown", "Total", "Multi-Race")) %>%
  pull(raceNameShort)

# IN OSHPD PRIMARY FUNCTION
listPosition <- c("nPrimary", "nOther", "nTotal", "percentPrimary", "percentOther")

# IN IGME SCRIPT
VALID_YEARS <- 1990:2017


# - 2. GLOBAL CONSTANTS --------------------------------------------------------------------------------------------------
LOCAL <- FALSE

myURL <- ifelse(LOCAL, "http://127.0.0.1:4437/", "https://skylab.dev.cdph.ca.gov/content/0d97f8b5-1ce0-4d5a-9df6-97ef53cbaac1/")

whichData <- 'real' # Change to fake to run app on local machine without access to real datasets

widthSidePanel <- 3
widthMainPanel <- 12 - widthSidePanel

viewType <- "Present"

# TEXT Constants
VERSION           <- "Version P3.0" # Used in ui.R
criticalNumber    <- 11 # Used in input_widgets.R
appTitle          <- "California Community Burden of Disease Engine" # Used in ui.R

figureAttribution <- "California Department of Public Health" # Used in make_MAPS.R

# eliminates "Rplots.pdf" error generated only on CDPH Shiny Server, from tmap leaflet map
pdf(NULL) 

# USE consistent map projection system throughout all app code !
proj1 <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
proj2 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


# Styles for help buttons and boxes ============================

myInputHelpButtonSty <- paste0("width:20px;  color:#fff; background-color:#337ab7; border-color:white; padding:0px; font-size: 18px;",
                               "margin:0px;",
                               "margin-left:10px;",
                               "float:right;"
)   #2e6da4
helpIcon <- "?"

# Used for broad group input in leading causes tab - Replaced float right with display inline block. 
myInputHelpButtonSty_broadGroup <- paste0("width:20px;  color:#fff; background-color:#337ab7; border-color:white; padding:0px; font-size: 18px;",
                                          "margin:0px;",
                                          "margin-left:10px;",
                                          "display:inline-block;"
) 


# - 3. LOAD PACKAGES ------------------------------------------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(magrittr)
library(leaflet)
library(tmap)
library(sf)
library(classInt) # Used in maps
library(plotly)
library(markdown)
library(directlabels)
library(scales)
library(visNetwork) # used in IHME tab
library(DT)
library(cowplot)
library(docxtractr)
library(rclipboard) # Copying link to clipboard

# - 4. READ DATASETS & INFO ------------------------------------------------------------------------------------------------------

# MORTALITY DATASETS
datCounty <- readRDS(paste0("myData/", whichData, "/datCounty.RDS"))
datCounty_3year <- readRDS(paste0("myData/", whichData, "/datCounty_3year.RDS"))
datCounty_5year <- readRDS(paste0("myData/", whichData, "/datCounty_5year.RDS"))
datCounty_RE <- readRDS(paste0("myData/", whichData, "/datCounty_RE.RDS"))
datState_RE <- readRDS(paste0("myData/", whichData, "/datState_RE.RDS"))

myAgeGroups <- c("0 - 4", "5 - 14", "15 - 24", "25 - 34", "35 - 44", "45 - 54", "55 - 64", "65 - 74", "75 - 84", "85+")
datCounty_AGE_3year <- readRDS(paste0("myData/", whichData, "/datCounty_AGE_3year.RDS")) %>% 
  mutate(ageGroup = factor(ageGroup, levels = myAgeGroups))
datState_AGE <- readRDS(paste0("myData/", whichData, "/datState_AGE.RDS")) %>% 
  mutate(ageGroup = factor(ageGroup, levels = myAgeGroups))

datCounty_EDU <- readRDS(paste0("myData/", whichData, "/datCounty_EDU.RDS"))
eduMap        <- as.data.frame(read_csv("myInfo/Education Codes and Names.csv"))
datCounty_EDU <- left_join(datCounty_EDU, eduMap, by="eduCode")

# Life Expectancy
geoMap  <- as.data.frame(read_excel("myInfo/County Codes to County Names Linkage.xlsx")) %>%
  select(FIPSCounty,county=countyName)

lifeTableCounty <- readRDS("myData/e0ciCounty.RDS") %>%
  mutate(FIPSCounty=substr(GEOID,3,5))  %>%
  left_join(geoMap,by="FIPSCounty") %>%
  mutate(sex = str_to_title(sex))

lifeTableState  <- readRDS("myData/e0ciState.RDS") %>%
  mutate(county = "CALIFORNIA") %>%
  mutate(sex = str_to_title(sex))

lifeTableSet   <- bind_rows(lifeTableCounty, lifeTableState) %>%
  left_join(select(raceLink, raceCode, raceNameShort), by = "raceCode")

# FIX MIN and MAX Year in global or other life tables function eventaully
minYear_LT <- min(lifeTableSet$year)
maxYear_LT <- max(lifeTableSet$year)


# POPULATION (DEMOGRAPHICS) DATASETS
popData_AgePyramid <- readRDS("myData/popData_AgePyramid.RDS")
popData_RaceAge <- readRDS("myData/popData_RaceAge.RDS")
popData_RacePie <- readRDS("myData/popData_RacePie.RDS")
popData_trends <- readRDS("myData/popData_SexRaceAge_Trends.RDS")

chartYearMap <-  read_excel("myInfo/Year to Year-Group Linkage.xlsx")


# - 5. LOAD FUNCTIONS ------------------------------------------------------------------------------------------------------------------------------------

# PLOTTING/TABLE FUNCTIONS
source("myFunctions/make_ANY_TREND_chart.R")
source("myFunctions/make_LIFE-EXPECTANCY_chart.R")
source("myFunctions/make_topTrends.R")
source("myFunctions/make_TREND-EDUCATION_chart.R")
source("myFunctions/make_demographic_charts.R")

# HELPER FUNCTIONS
source("myFunctions/helperFunctions/wrapLabels.R")
source("myFunctions/helperFunctions/dottedSelectInput.R")


# - 6. APP TEXT ---------------------------------------------------------------------------------------------------------------------------------------

appText         <- read_docx(path(ccbData, "appText/appText.docx")) # Read in appText Word Doc
appText         <- docx_extract_tbl(appText, 1) # Extract table
appTextL        <- split(appText$Text, seq(nrow(appText))) # Convert data frame into a list
names(appTextL) <- appText$varName # Add varNames to list

# We use appTextL list object to insert text throughout the app

# For hospTab text
HospitalizationsTab   <- paste(appTextL$hospA,"<br><br>", appTextL$hospB)
HospitalPrimaryAnyTab <- paste(appTextL$hospA,"<br><br>", appTextL$hospC)


# NEWS AND UPDATES
news_and_updates <- read_docx(paste0(ccbData, "/appText/newsUseCCB_Word.docx"))
news_and_updates <- docx_extract_tbl(news_and_updates, 1) %>%
  mutate(Text = paste("<li>", Date, Update, "</li>"))
news_and_updates <- paste("\n<li>Welcome to the CCB!</li>\n<br>", (paste(news_and_updates$Text, collapse = "\n")), sep = "\n")


# WARNING MESSAGES
multiRaceWarning <- "*Note: Multirace data are NOT RELIABLE due to changing data collection practices"


tabsLink <- read_xlsx("myInfo/queryParameter_Linkage.xlsx") %>% 
  select(tabID, sub_tabID)


# - 7. CREATE VECTORS FOR SHINY INPUTS -------------------------------------------------------------------------------------------------------------


# -- DEATH MEASURE DROPDOWNS 

deathMeasures <- c("Ndeaths", "cDeathRate", "aRate", "YLL", "YLLper", "YLL.adj.rate", "mean.age", "SMR")

deathMeasuresNames <- c(
  "Number of deaths",
  "Crude Death Rate per 100,000 population",
  "Age-Adjusted Death Rate",
  "Years of Life Lost (YLL)",
  "YLL Rate per 100,000 population",
  "Age-Adjusted YLL Rate",
  "Mean Age at Death",
  "Standard Mortality Ratio")

names(deathMeasures) <- deathMeasuresNames


# MOST TABS
deathMeasures_Dropdown         <- deathMeasures[deathMeasures != 'SMR']

# AGE TREND TAB
deathMeasures_Dropdown_noADJ   <- deathMeasures[!deathMeasures %in% c("aRate", "YLL.adj.rate", "SMR")]

# RANK BY CAUSE TAB
deathMeasuresShort_Dropdown    <- deathMeasures[deathMeasures %in% c("Ndeaths", "aRate", "YLLper", "mean.age", "SMR")]

# SDOH TAB
deathMeasures_Dropdown_SDOH    <- deathMeasures[deathMeasures %in% c("Ndeaths", "cDeathRate", "aRate")]
mean_age_sort <- c("Youngest to Oldest", "Oldest to Youngest")



# HOSPITALIZATION INPUTS
hospMeasures      <- c("n_hosp", "cHospRate", "ahospRate","avg_los", "charges", "cChargeRate", "avgcharge", "avgcharge_per_day", "medcharge", "medcharge_per_day")

hospMeasuresNames <- c("Number of Hospitalizations", "Crude Hospitalization Rate", "Age-Adjusted Hospitalization Rate", "Average Length of Stay (Days)", "Total Charges",
                       "Crude Charge Rate", "Average Charges", "Average Charges per Day", "Median Charges", "Median Charges per Day")

hospMeasures_Revalue        <- hospMeasuresNames #used in function to rename from short to long names
names(hospMeasures_Revalue) <- hospMeasures


shorthospList <- c(-2, -3, -6, -7, -8, -10)
shortMDCList  <- c(-2,-3,-4,-6,-8, -10)
hM_short      <- hospMeasures[shorthospList] 
hMNames_short <- hospMeasuresNames[shorthospList]#Used in shiny app dropdown menu



MDC_DRG_ICD <- c("icd10_cm", "mdc", "drg")
MDC_DRG_ICD_names <- c("Global Burden", "Major Diagnostic Code", "Diagnostic Related Groups")

MDC_DRG_ICD_Dropdown <- MDC_DRG_ICD
names(MDC_DRG_ICD_Dropdown) <- MDC_DRG_ICD_names


hMDCRevalue_short <- hospMeasures_Revalue[shortMDCList]
hMDCNames_short <- hospMeasuresNames[shortMDCList]

hMDCDrop_down <- c("Number of Hospitalizations", "Total Charges (in thousands)", "Average Charges (in thousands)", "Median Charges (in thousands)")

MDC_DRG <- c("mdc", "drg")
MDC_DRGNames <- c("Major Diagnostic Code", "Diagnostic Related Groups")

MDCDRG_Dropdown <- MDC_DRG
names(MDCDRG_Dropdown) <- MDC_DRGNames


# CAUSES - FULL LIST, PUBLIC HEALTH LEVEL LIST, TOP LEVEL LIST
fullList          <- deathCauseLink[, "causeCode"]
names(fullList)   <- deathCauseLink[, "causeList" ]

phList            <- deathCauseLink %>% filter(nchar(causeCode) <= 3)
phCode            <- phList[, "causeCode"]
names(phCode)     <- phList[, "causeList" ]

bigList           <- deathCauseLink %>% filter(nchar(causeCode) <= 1)
bigCode           <- bigList[, "causeCode"]
names(bigCode)    <- bigList[, "causeList"]


# SOCIAL DETERMINANTS OF HEALTH

sdohLink <- readxl::read_excel(paste0(standardsPlace, "sdohLink.xlsx")) %>%
  filter(inCCB == "x")

sdohVec <- setNames(sdohLink$sdohCode, sdohLink$sdohName)


# COUNTY LIST
lList         <- sort(as.character(unique(datCounty$county))) # Used in input widgets


# INPUT FUNCTIONS
# source("myFunctions/inputFunctions/input_widgets.R")


# Plotting functions ----------------------------------------------------------------------------------------------
plotly_layout <- function(p, myTitle = "", myTitleSize = 22, 
                          myTitleX = "", myTitleXSize = 18, 
                          myTitleY = "", myTitleYSize = 18) {
  p %>% plotly::layout(title = list(text = myTitle, font = list(size = myTitleSize, color = "navyblue")), 
                       xaxis = list(title = list(text = myTitleX, font = list(size = myTitleXSize))), 
                       yaxis = list(title = list(text = myTitleY, font = list(size = myTitleYSize)))
  )
}

plotly_directLabels <- function(myText = "", myColor = "black", myY = 0, myX = 0.95, labelLocation = "right") {
  # x <- ifelse(labelLocation == "right", 0.95, 0.05)
  xanchor <- ifelse(labelLocation == "right", "left", "right")
  list(xref = 'paper', x = myX, y = myY, xanchor = xanchor, yanchor = "middle", 
       text = myText, font = list(size = 14, color = myColor), showarrow = FALSE)
}


