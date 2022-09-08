if(1==2){
  myLHJ="CALIFORNIA" 
  myCause="0"
  myCompare = "lowest rate"
  myAddN = TRUE
  myAddRR = TRUE
  myAddRate = TRUE
}

# ------------------------------------------------------------------------------------

disparity <- function(myLHJ="CALIFORNIA", myCause="A", myCompare="lowest rate", myAddN=T, myAddRR=T, myAddRate=T) {
  
  lowColor  <- "palegreen"
  midColor  <- "paleturquoise"
  highColor <- "tomato"
  
  mySmaller <- 0.8   
  myAxisSize <- myAxisSize  * mySmaller
  myTextSize3 <-  myTextSize3 * mySmaller
  myLegendSize <- myLegendSize * mySmaller
  
  
  #--RACE ------------------------------------------------------------------------------------------------------------------------
  
  if(myCompare == "highest rate") {
    raceTest <- raceTest_HIGH %>% filter(raceCode != "Multi")
    fillColor <- c("Highest" = highColor, "Sig. Lower (p<.01)" = lowColor, "No Difference" = midColor)
  }
  
  if(myCompare == "lowest rate") {
    raceTest <- raceTest_LOW %>% filter(raceCode != "Multi")
    fillColor <- c("Lowest" = lowColor, "Sig. Higher (p<.01)" = highColor, "No Difference" = midColor)
  }
  
  myMeasureRace <- "aRate"
  
  dat.1 <- filter(raceTest,county == myLHJ,causeCode == myCause, yearG3==yearGrp3, sex == "Total")
  dat.1 <- left_join(dat.1,raceLink,by="raceCode") %>%
    mutate(across(c(aRate, rateRatio), ~ round(.x, 1)),
           plotText = paste0("<br><b>R/E:</b> ", raceNameShort, 
                             "<br><b>Number of deaths:</b> ", Ndeaths, 
                             "<br><b>Age-Adjusted Death Rate:</b> ", aRate,
                             "<br><b>Rate Ratio:</b> ", rateRatio))
    
  
  raceDF <- dat.1 %>%
    mutate(ageGroup = "Total", rateType = "Age-Adjusted Death Rate") %>%
    left_join(select(deathCauseLink, causeCode, causeName), by = "causeCode") %>%
    select(yearG3, county, causeName, sex, ageGroup, raceName, Ndeaths, rateType, 
           rate = aRate, rateSE = aSE, LCI, UCI, compareGroup = lowRace, compareRate = bestRate, 
           compareSE = bestSE, rateRatio, Ztest, pValue, pMark)
  
  if (nrow(dat.1)==0) stop("Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria.")
  
  tMax <- max(dat.1$aRate)
  placeLabels  <- tMax/5 
  placeLabels2 <- tMax/8 
  placeLabels3 <- tMax/20
  
  racePlot <- ggplot(data=dat.1, aes(x=raceNameShort, y=aRate,fill=pMark)) +
    geom_bar(stat="identity") +
    
    
    theme_grey() +   #base_size = myBaseSize
    scale_fill_manual("legend", values = fillColor) +
    geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, size=1, position=position_dodge(.9), color="gray") + 
    labs(title="Race/Ethnicity",y = "rate per 100,000 (age-adjusted)") +
    theme(legend.position="bottom",
          legend.spacing.x = unit(10.0, 'points'),  ### EXPLORE THIS for optimal look  - unit(10.0, 'px')
          plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
          axis.title.y = element_text(size = myTextSize3, margin = margin(t=0, r=10, b=0, l= 0), face="bold"),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = myAxisSize),
          axis.text.x = element_text(size = myAxisSize),
          legend.title = element_blank(),
          legend.text = element_text(size = myLegendSize),
          strip.text = element_text(size = myAxisSize)) 
  
  
  if (myAddN)  racePlot   <- racePlot + geom_text(aes(y=placeLabels,label=paste("N =",comma(Ndeaths)))) 
  if (myAddRate) racePlot <- racePlot + geom_text(aes(y=placeLabels2,label=paste("Rate =",number(aRate,accuracy = 0.1))))
  if (myAddRR) racePlot   <- racePlot + geom_text(aes(y=placeLabels3,label=paste("RR =",number(rateRatio,accuracy = 0.1))))
  
  
  # Interactive plot --------------------------------------------------------------------------------------------------------------------
  racePlot_interactive <- plot_ly(dat.1, x = ~raceNameShort, y = ~aRate, 
                                  type = "bar", 
                                  color = ~pMark, colors = fillColor, 
                                  hoverinfo = "text", text = ~plotText) 
  
  racePlot_interactive <- plotly_layout(racePlot_interactive, 
                                        myTitle = NULL, 
                                        myTitleX = NULL,
                                        myTitleY = "Rate per 100k (Age-Adjusted)") %>%
    layout(legend = list(orientation = "h", 
                         xanchor = "center",  
                         x = 0.5)) 
  
  
  #--AGE ------------------------------------------------------------------------------------------------------------------------
  
  ageMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Group Standard and US Standard 2000 Population.xlsx"), sheet = "data"))
  
  myMeasure <- "cDeathRate"
  
  if(myCompare == "highest rate") {
    ageTest <- ageTest_HIGH
    fillColor <- c("Highest" = highColor, "Sig. Lower (p<.01)" = lowColor, "No Difference" = midColor)
  }
  
  if(myCompare == "lowest rate") {
    ageTest <- ageTest_LOW
    fillColor <- c("Lowest" = lowColor, "Sig. Higher (p<.01)" = highColor, "No Difference" = midColor)
  }
  
  dat.1 <- filter(ageTest,county == myLHJ,causeCode == myCause, yearG3==yearGrp3, sex == "Total") %>%
    mutate(ageGroup = factor(ageGroup,levels= ageMap$ageLabel)) %>%
    mutate(across(c(cDeathRate, rateRatio), ~ round(.x, 1)),
           plotText = paste0("<br><b>Age Group:</b> ", ageGroup, 
                             "<br><b>Number of deaths:</b> ", Ndeaths, 
                             "<br><b>Age-Adjusted Death Rate:</b> ", cDeathRate,
                             "<br><b>Rate Ratio:</b> ", rateRatio))
  
  ageDF <- dat.1 %>%
    mutate(raceName = "Total", rateType = "Age-Specific Death Rate") %>%
    left_join(select(deathCauseLink, causeCode, causeName), by = "causeCode") %>%
    select(yearG3, county, causeName, sex, ageGroup, raceName, Ndeaths, rateType, 
           rate = cDeathRate, rateSE, LCI, UCI, compareGroup = lowAge, compareRate = bestRate, 
           compareSE = bestSE, rateRatio, Ztest, pValue, pMark)
  ###KEY new approach here:
  #dat.1$ageGroup <- factor(dat.1$ageGroup,levels = ageMap$ageLabel)
  
  tMax <- max(dat.1$cDeathRate)
  AGEplaceLabels  <- tMax/5
  AGEplaceLabels2 <- tMax/8
  AGEplaceLabels3 <- tMax/20
  
  
  if (nrow(dat.1)==0) stop("Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria.")
  
  agePlot <- ggplot(data=dat.1, aes(x=ageGroup, y= cDeathRate, fill=pMark)) +
    geom_bar(stat="identity") +
    theme_grey() +   #base_size = myBaseSize
    scale_fill_manual("legend", values = fillColor) +
    geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, size=1, position=position_dodge(.9), color="gray") +
    
    labs(title = "Age Groups", y = "rate per 100,000 (age-specific)") +
    
    theme(legend.position="bottom",
          plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
          axis.title.y = element_text(size = myTextSize3, margin = margin(t=0, r=10, b=0, l= 0), face="bold"),
          axis.text.y = element_text(size = myAxisSize),
          axis.title.x = element_blank(),                  axis.text.x = element_text(size = myAxisSize),
          legend.title = element_blank(),                  legend.text = element_text(size = myLegendSize)
    ) + 
    scale_y_continuous(labels = comma)
  
  
  
  
  if (myAddN)  agePlot   <- agePlot + geom_text(aes(y=AGEplaceLabels, label=paste("N =",comma(Ndeaths)))) 
  if (myAddRate) agePlot <- agePlot + geom_text(aes(y=AGEplaceLabels2,label=paste("Rate =",number(cDeathRate,accuracy = 0.1))))
  if (myAddRR) agePlot   <- agePlot + geom_text(aes(y=AGEplaceLabels3,label=paste("RR =",number(rateRatio,accuracy = 0.1))))
  
  
  # Interactive plot --------------------------------------------------------------------------------------------------------------------
  agePlot_interactive <- plot_ly(dat.1, x = ~ageGroup, y = ~cDeathRate, 
                                  type = "bar", 
                                  color = ~pMark, colors = fillColor, 
                                  hoverinfo = "text", text = ~plotText) 
  
  agePlot_interactive <- plotly_layout(agePlot_interactive, 
                                        myTitle = NULL, 
                                        myTitleX = NULL,
                                        myTitleY = "Rate per 100k (Age-Specific)") %>%
    layout(legend = list(orientation = "h", 
                         xanchor = "center",  
                         x = 0.5)) 
  
  #--SEX ------------------------------------------------------------------------------------------------------------------------
  
  myMeasureRace <- "aRate"  # works...
  
  if(myCompare == "highest rate") {
    sexTest <- sexTest_HIGH
    fillColor <- c("Highest" = highColor, "Sig. Lower (p<.01)" = lowColor, "No Difference" = midColor)
  }
  
  if(myCompare == "lowest rate") {
    sexTest <- sexTest_LOW
    fillColor <- c("Lowest" = lowColor, "Sig. Higher (p<.01)" = highColor, "No Difference" = midColor)
  }
  
  dat.1 <- filter(sexTest,county == myLHJ,causeCode == myCause, yearG3==yearGrp3) %>%
    mutate(across(c(aRate, rateRatio), ~ round(.x, 1)),
           plotText = paste0("<br><b>Sex:</b> ", sex, 
                             "<br><b>Number of deaths:</b> ", Ndeaths, 
                             "<br><b>Age-Adjusted Death Rate:</b> ", aRate,
                             "<br><b>Rate Ratio:</b> ", rateRatio))
  
  sexDF <- dat.1 %>%
    mutate(ageGroup = "Total", raceName = "Total", rateType = "Age-Adjusted Death Rate") %>%
    left_join(select(deathCauseLink, causeCode, causeName), by = "causeCode") %>%
    select(yearG3, county, causeName, sex, ageGroup, raceName, Ndeaths, rateType, 
           rate = aRate, rateSE = aSE, LCI, UCI, compareGroup = lowRace, compareRate = bestRate, 
           compareSE = bestSE, rateRatio, Ztest, pValue, pMark)
  
  tMax <- max(dat.1$aRate)
  placeLabels  <- tMax/5 
  placeLabels2 <- tMax/8 
  placeLabels3 <- tMax/20
  
  
  if (nrow(dat.1)==0) stop("Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria.")
  
  myTit <-  "Sex (age-adjusted rate)"
  
  sexPlot <- ggplot(data=dat.1, aes(x=sex, y=eval(parse(text=paste0(myMeasureRace))),fill=pMark)) +
    geom_bar(stat="identity") +
    theme_grey() +   #base_size = myBaseSize
    scale_fill_manual("legend", values =fillColor) +
    # guides(fill = guide_legend(reverse=TRUE)) +
    geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, size=1, position=position_dodge(.9), color="gray") + 
    labs(y = deathMeasuresNames[deathMeasures == myMeasureRace], x="Sex") +
    
    labs(title = "Sex", y = "rate per 100,000 (age-adjusted)") +
    
    theme(legend.position="bottom",
          plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
          axis.title.y = element_text(size = myTextSize3,margin = margin(t=0, r=10, b=0, l= 10), face="bold"), axis.text.y = element_text(size = myAxisSize),
          axis.title.x = element_blank(),                  axis.text.x = element_text(size = myAxisSize),
          legend.title = element_blank(),                  legend.text = element_text(size = myLegendSize)
          
    )
  
  if (myAddN)  sexPlot <- sexPlot + geom_text(aes(y=placeLabels,label=paste("N =",comma(Ndeaths)))) 
  if (myAddRate) sexPlot <- sexPlot + geom_text(aes(y=placeLabels2,label=paste("Rate =",number(aRate,accuracy = 0.1))))
  if (myAddRR) sexPlot <- sexPlot + geom_text(aes(y=placeLabels3,label=paste("RR =",number(rateRatio,accuracy = 0.1))))
  
  
  # Interactive plot --------------------------------------------------------------------------------------------------------------------
  sexPlot_interactive <- plot_ly(dat.1, x = ~sex, y = ~aRate, 
                                 type = "bar", 
                                 color = ~pMark, colors = fillColor, 
                                 hoverinfo = "text", text = ~plotText) 
  
  sexPlot_interactive <- plotly_layout(sexPlot_interactive, 
                                       myTitle = NULL, 
                                       myTitleX = NULL,
                                       myTitleY = "Rate per 100k (Age-Adjusted)") %>%
    layout(legend = list(orientation = "h", 
                         xanchor = "center",  
                         x = 0.5)) 
  
  #------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  #------------------------------------------------------------------------------------
  library(cowplot)
  # https://wilkelab.org/cowplot/articles/plot_grid.html
  
  mainTitle <- ggdraw() + 
    draw_label(paste0("Disparities in Death Rates, ", deathCauseLink$causeName[deathCauseLink$causeCode== myCause]," in ",myLHJ,", ",yearGrp3), colour=myTitleColor, size=myTitleSize,fontface = "bold")
  
  
  topRow <- cowplot::plot_grid(racePlot,sexPlot,rel_widths = c(6,3))
  dPlot   <- plot_grid(mainTitle,topRow,agePlot,ncol=1,rel_heights = c(1,5,5))
  
  # Download data
  
  df <- bind_rows(sexDF, raceDF, ageDF)
  
  list(plot=dPlot,
       sexPlotL_interactive = sexPlot_interactive,
       racePlotL_interactive = racePlot_interactive,
       agePlotL_interactive = agePlot_interactive,
       dataL = df)
  
}
