# Documentation ==================================================================================================================================

# About: Comprised of standard objects to be sourced into the CCB app and other projects

# 1) Loads common packages into session
# 2) 

# server <- F
# if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
# if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

# Packages ==================================================================================================================================

library(readxl) # Read excel files
library(readr) # Read csv files
library(dplyr) # Data manipulation
library(tidyr) # Data tidying
library(stringr) # Working with strings
library(ggplot2) # Data visualization
library(fs) # ?
library(scales) # Formatting
library(RColorBrewer) # Colors

# Global contants =====================================================================================================================

STATE <- "CALIFORNIA" # String for state
yF <- 100000  # rate constant 

## Years -----------------------------------------------------------------------
currentYear          <- 2021 # Most recent year available - Death data
currentYear_hosp_ed  <- 2020 # Most recent year available - Hosp/ED data
incRecentYearData <- T # Include recent year?
currentYear <- ifelse(incRecentYearData, currentYear, currentYear - 1) # Set to either most recent year or prior year - death data

yearGrp3 <- paste0(currentYear - 2, "-", currentYear) # 3-year aggregate - Death data
yearGrp5 <- paste0(currentYear - 4, "-", currentYear) # 5-year aggregate - Death data
yearGrp3_hosp_ed     <- paste0(currentYear_hosp_ed-2, "-", currentYear_hosp_ed) # 3-year aggregate - Hosp/ED data
yearGrp3_hosp_ed_num <- (currentYear_hosp_ed-2):(currentYear_hosp_ed) # 5-year aggregate - Hosp/ED data

# For trends
minYear      <- 2000
maxYear      <- currentYear

# Read in info files ==================================================================================================================

raceLink    <-  read_excel("Standards/raceLink.xlsx")
ageLink     <-  read_excel("Standards/ageLink.xlsx",sheet = "standard")
commInfo    <- read.csv("myInfo/comName.csv", header = T)

## CCB Cause Code linkage ---------------
deathCauseLink   <- read_excel("myInfo/icd10_to_CAUSE.xlsx", sheet = "main") %>%
  filter(!is.na(causeList)) %>%
  mutate(causeNameShort = ifelse(is.na(causeNameShort), causeName, causeNameShort),
         topLevName = case_when(topLevCode  == "0" ~ "All Causes",
                                topLevCode  == "A" ~ "Communicable",
                                topLevCode  == "B" ~ "Cancer",
                                topLevCode  == "C" ~ "Cardiovascular",
                                topLevCode  == "D" ~ "Other Chronic",
                                topLevCode  == "E" ~ "Injury", 
                                TRUE ~ "Ill-Defined")) %>%
  select(causeCode, causeName, causeNameShort, causeList, topLevCode, topLevName) %>%
  arrange(causeCode) %>%
  as.data.frame()

## Hosp Cause Code linkage -----------------
hospCauseLink  <- read_excel("myInfo/CCS Code and Names Linkage.xlsx") %>%
  mutate(causeCode = str_pad(ccsCode, 5,"left",pad="o")) %>%
  mutate(causeNameShort = ifelse(is.na(ccsNameShort),ccsName,ccsNameShort)) %>%
  select(causeCode, causeName = ccsName, causeNameShort, topLevName, birth) %>%
  as.data.frame()


# COLORS ==========================================================================================================================

## Color-blind friendly (9 colors) palette ---------------- 
paletteCB <- c("#0072B2", # darker blue, 
               "#4A4A4A", # darker gray,
               "#D55E00", # darker orange
               "#117733", # green
               "#56B4E9", # lightblue
               "#4BE62F", # light green
               "#E69F00", # lighter orange
               "#CC79A7",  # pink
               "#b22222" # firebrick
               )

# Sex color palette ------------------------------------
genderColors <- setNames(
  paletteCB[1:3],
  c("Female","Male", "Total")
)

## Race color palette ---------------------
paletteCB_race <- paletteCB[c(
  2, # darker gray
  3, # darker orange
  5, # lightblue
  4, # green
  7, # lighter orange
  1, # darker blue
  9, # firebrick
  6, # light green
  8 # pink
)]

# Remove Unknown and Total race
raceList <- raceLink %>% 
  filter(!raceCode %in% c("Unknown", "Total"))

raceName <- c(sort(raceList$raceName), "Total")
raceNameColors <- setNames(paletteCB_race[1:length(raceName)], raceName)
  
raceNameShort <- c(sort(raceList$raceNameShort), "Total")
raceNameShortColors <- setNames(paletteCB_race[1:length(raceNameShort)], raceNameShort)


## Broad condition group color palette -----------------------------
topLev <- c("Communicable","Cancer","Cardiovascular","Other Chronic","Injury","Ill-Defined","Perinatal","Other")
topLevColors <- setNames(
  paletteCB[1:length(topLev)],
  topLev
)

topLevTextColors <- setNames(
  c("#FFFFFF", "#000000","#000000", "#000000", "#000000", "#000000", "#000000", "#000000"),
  topLev
)


# ggplot2 standards =======================================================================================================
myTitleSize <- 16
myTextSize <- 14

myTheme <- theme_bw() +
  theme(plot.title = element_text(size = myTitleSize, color = "darkblue", face = 'bold'),
        strip.text.x = element_text(size = myTextSize, face="bold", angle = 0),
        strip.text.y = element_text(size = myTextSize, face="bold", angle = 0),
        axis.title   = element_text(size = myTextSize, face="bold"), 
        axis.text.x  = element_text(size = myTextSize),
        axis.text.y  = element_text(size = myTextSize),
        legend.title = element_text(size = myTextSize, face = "bold"),
        legend.text = element_text(size = myTextSize), 
  )

theme_set(myTheme)

myLineWidth <- 1.5
myPointSize <- 2

myWrapNumber <- 70
myTitleColor <- "darkblue"
