# server <- T
# if (!server) source("g:/FusionData/Standards/FusionStandards.R")
# if (server) source("/mnt/projects/FusionData/Standards/FusionStandards.R")

#== FUNCTION ========================================================================================================  
  
  
LEtrend <- function(myLHJ="CALIFORNIA", mySexMult, myRace, myCI, myYearGrouping = 1, bar = FALSE) {
   
 
#---BAR PART------------------------------------------------------------------------------------------------------
   
  
  if(1==2){
    myLHJ="Alameda" 
    myLHJ="CALIFORNIA"
    myLHJ="Alameda" 
    myLHJ="Butte"
    myLHJ="Marin"
    mySex <- c("Male","Female")
    myRace <- "TOTAL"
    myCause="A01"
    myMeasure = "YLL"
    mySex   = "Total"
    myLHJ="CALIFORNIA"
    mySexMult = c("Male","Female")
    # myRace = c("AIAN_NH",   "ASIAN_NH",   "BLACK_NH",  "HISPANIC",   "WHITE_NH")
    myRace = c(  "Asian",   "Black",  "Latino",   "White")
    myCI = FALSE
  }  
  
  # Prepare data ---------------------------------------------------------------------------------------------------------------------------------------------------
  dat.1 <- lifeTableSet %>% filter(county==myLHJ, sex %in% mySexMult, raceNameShort %in% myRace) %>% 
    filter(nyrs == myYearGrouping) %>% 
    mutate(lineLabel = ifelse(sex == "Total", raceNameShort, paste(raceNameShort,"-",sex)), 
           ex = round(ex, 1),
           exlow = round(exlow, 1),
           exhigh = round(exhigh, 1),
           Group = paste(raceNameShort, sex), 
           plotText = paste0("<b>Year:</b> ", year, 
                             "<br><b>Sex:</b> ", sex,
                             "<br><b>R/E:</b> ", raceNameShort, 
                             "<br><b>Life Expectancy:</b> ", ex, 
                             "<br><b>95% CI:</b> (", exlow, ", ", exhigh, ")"))
  
  if (nrow(dat.1)==0) stop("Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria.")
  
  
  # Create bar chart if bar = TRUE --------------------------------------------------------------------------------------------------------------------------------
  if (bar) {
    tplot_bar <- ggplot(data=filter(dat.1, year== 2020, nyrs == myYearGrouping), aes(x=raceNameShort, y=ex, fill=sex)) + 
      geom_bar(stat = "identity",position="dodge", colour = 'black')  +
      scale_fill_manual(values = genderColors) + 
      labs(x = "Race/Ethnicity", y = "Life Expectancy at Birth", x = "Year") +
      coord_cartesian(ylim=c(65,90)) +
      geom_segment(aes(x = .3, y = 64.8, xend = .5, yend = 65.3),
                   color="red",size=1.2) +
      geom_segment(aes(x = .3, y = 64.4, xend = .5, yend = 64.9),
                   color="red",size=1.2) +
      geom_text(aes(label=format(round(ex, 1), nsmall = 1)), position=position_dodge(width=0.9), vjust=2,fontface="bold", color = 'white')
  } else {
    tplot_bar <- NULL
  }
 

 # Create line chart --------------------------------------------------------------------------------------------------------------------------------------------
 
  # Plot title
  myTitle <- paste0("Trend in Life Expectancy, ",myLHJ,", ",minYear_LT,"-",maxYear_LT)
  myTitle <-  wrap.labels(myTitle,myWrapNumber)
  
  myBreaks <- minYear_LT:maxYear_LT
  myLabels <- myBreaks
  
  # Static plot -------------------------------------------------------------------------------------------------------------
  tplot <- ggplot(data=dat.1, aes(x=year, y=ex)) +                     # , nyrs == 1
    geom_line(size=1.6,aes(color=raceNameShort,linetype=sex)) +
     # geom_point(shape = 21, size=2.5, aes(color = raceNameShort), fill = "white")  +
    ylim(62, 93) +
    scale_x_continuous(minor_breaks=myBreaks,breaks=myBreaks,
                       expand = expansion(mult = c(0, 0), add = c(1, 5)), # lower-limit: 2000 - (2018 - 2000) * 0 - 1... upper-limit: 2018 + (2018 - 2000) * 0 + 5
                       #expand=c(0,5), # 
                       labels=myLabels) +
    scale_color_manual(values = raceNameShortColors) +   
    labs(title =myTitle, y = "Life Expectancy at Birth", x = "Year")  +
    theme_bw() +
    theme(axis.text=element_text(size=myAxisSize),
          axis.title=element_text(size=myAxisSize,face="bold"),
          plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
          axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1), 
          legend.position = "none"
          ) +    
    geom_dl(aes(label = lineLabel,color=raceNameShort), method = list(dl.trans(x = x + 0.2), "last.points", 
                                                                       #size = myLineLabelSize, # use this instead of cex
                                                                       cex=myLineLabelCex, 
                                                                       'last.bumpup',font="bold"))
   
   if (myCI) {
     tplot <- tplot +
       geom_line(data=dat.1,aes(x=year, y=exlow, color=raceNameShort,linetype=sex)) +
       geom_line(data=dat.1,aes(x=year, y=exhigh,color=raceNameShort,linetype=sex)) 
   }
  
  
  # Interactive plot --------------------------------------------------------------------------------------------------------------------
   tplot_interactive <- plot_ly(dat.1, x = ~year, y = ~ex, 
                                type = "scatter", mode = "lines+markers", 
                                linetype = ~sex, 
                                color = ~raceNameShort, colors = raceNameShortColors, 
                                hoverinfo = "text", text = ~plotText) 
   
   tplot_interactive <- plotly_layout(tplot_interactive, myTitle = NULL, myTitleX = "Year", myTitleY = "Life Expectancy at Birth") 
   
   for (group in unique(dat.1$Group)) {
     tDat <- dat.1 %>% filter(year == max(year), Group == group)
     y <- tDat$ex
     dl_text <- tDat$Group
     dl_color <- unname(raceNameShortColors[names(raceNameShortColors) == unique(tDat$raceNameShort)])
     tplot_interactive <- tplot_interactive %>% 
       layout(annotations = plotly_directLabels(myText = dl_text, myColor = dl_color, myX = 0.9, myY = y, labelLocation = "right"), 
              xaxis = list(range=c(minYear_LT - 1, maxYear_LT + 3)))
     
   }
   
   if (myCI) {
     tplot_interactive <- tplot_interactive %>% 
       add_trace(x = ~year, y = ~exhigh, 
                 type = "scatter", mode = "lines", 
                 linetype = ~sex, 
                 color = ~raceNameShort, colors = raceNameShortColors,
                 showlegend = FALSE, hoverinfo = "skip") %>% 
       add_trace(x = ~year, y = ~exlow, 
                 type = "scatter", mode = "lines", 
                 linetype = ~sex, 
                 color = ~raceNameShort, colors = raceNameShortColors, 
                 showlegend = FALSE, hoverinfo = "skip")
   }
   
   # Prepare data for downloading --------------------------------------------------------------------------------------------
   dat.1 <- dat.1 %>%
     select(county,nyrs,year, sex,race=raceNameShort, LifeExpectancy=ex, LECI_lower = exlow, LECI_upper=exhigh)
   
   
   list(plotL = tplot, plotL_interactive = tplot_interactive, dataL = dat.1, bar=tplot_bar)

 }
 
 
 