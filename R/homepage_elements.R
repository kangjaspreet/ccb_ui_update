
# Anatomy menu information ---------------------------------------------------------------------------------------------------
anatomyMenuInfo <- function(selectedCategory) {
  
  homeAnatomyMenu %>% 
    filter(menuValue == selectedCategory) %>% 
    pull(menuInfo) %>% 
    return()
  
}


# Featured assessments/stories -----------------------------------------------------------------------------------------------

featuredStory <- function(
  myURL,
  myImage,
  myTitle,
  myDescription
) {
  HTML(
    paste0(
      "<div class = 'featuredStory'>
        <a href='", myURL, "'>
          <span class='link-spanner'></span>
        </a>
        <img src='Featured Stories/", myImage, "'>
        <h1>", myTitle, "</h1>
        <p>", myDescription, "</p>
      </div>"
    )
  )
}


feature_shaCM <- featuredStory(
  myURL = "https://skylab.cdph.ca.gov/communityBurden/_w_ac3e8cdd/SOPH/2022/Full%20Report.html",
  myImage = "SHA CM - Image.png",
  myTitle = "State Health Assessment Core Module - 2022",
  myDescription = "This annual State Health Assessment (SHA) Core Module provides a snapshot of the health status for the entire California population. The Module is based upon a set of standard inputs, produced using an automated system, to assess population health across a range of health conditions, demographic characteristics, and other factors (e.g., disparities and inequities). The Module is used to identify key findings that contribute to informing the State Health Improvement Plan."
  
)

feature_xMDB <- featuredStory(
  myURL = "https://skylab.cdph.ca.gov/communityBurden/_w_ac3e8cdd/xMDA/2020_Excess_Mortality.html",
  myImage = "Excess Mortality Data Brief - Image.jpg",
  myTitle = "2020/2021 Excess Mortality Data Brief",
  myDescription = "This annual State Health Assessment (SHA) Core Module provides a snapshot of the health status for the entire California population. The Module is based upon a set of standard inputs, produced using an automated system, to assess population health across a range of health conditions, demographic characteristics, and other factors (e.g., disparities and inequities). The Module is used to identify key findings that contribute to informing the State Health Improvement Plan."
  
)

feature_lghcChildhood <- featuredStory(
  myURL = "https://letsgethealthy.ca.gov/the-story/",
  myImage = "LGHC Childhood Wellbeing - Image.jfif",
  myTitle = "Let's Get Healthy CA Story - Early Childhood Well-Being",
  myDescription = "This annual State Health Assessment (SHA) Core Module provides a snapshot of the health status for the entire California population. The Module is based upon a set of standard inputs, produced using an automated system, to assess population health across a range of health conditions, demographic characteristics, and other factors (e.g., disparities and inequities). The Module is used to identify key findings that contribute to informing the State Health Improvement Plan."
  
)

feature_scoda <- featuredStory(
  myURL = "https://skylab.cdph.ca.gov/communityBurden/_w_ac3e8cdd/SOPH/2022/Full%20Report.html",
  myImage = "SCODA - Image.png",
  myTitle = "Social Determinant of Health-Cause of Death Analysis",
  myDescription = "This annual State Health Assessment (SHA) Core Module provides a snapshot of the health status for the entire California population. The Module is based upon a set of standard inputs, produced using an automated system, to assess population health across a range of health conditions, demographic characteristics, and other factors (e.g., disparities and inequities). The Module is used to identify key findings that contribute to informing the State Health Improvement Plan."
  
)

