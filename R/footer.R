footer <- fluidRow(
  column(width = 3, img(src = 'Logos/CDPH_Logo2.jpg', height = "100px", align = "left")),
  column(width = 6, style = "text-align: center;",
         h4("CCB Version 4.20 | Last Updated July 1, 2022"), 
         HTML('<h5>Contact: <a href = "mailto: ccb@cdph.ca.gov">CCB@cdph.ca.gov</a></h5>'),
         HTML("<h5><a href='https://www.surveymonkey.com/r/2N2JSTV' target='_blank'>Report Bugs Here</a>  |  <a href='https://www.surveymonkey.com/r/ZH9LSR8' target='_blank'>Share Feedback Here</a> | <a href= 'https://github.com/mcSamuelDataSci/CACommunityBurden' target='_blank'>CCB Github</a></h5> ")
  ),
  column(width = 3, img(src = 'Logos/FC_Logo-SquareCentered.jpg', height = "100px", align = "right"))
)