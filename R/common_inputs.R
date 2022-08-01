tabInformation <- function(id) {
  actionButton(inputId = id, label = "Show Tab Information")
}

linkView_ui <- function(id) {
  ns <- NS(id)
  uiOutput(outputId = "clip")
}

linkView_server <- function(id, myURL) {
  moduleServer(
    id,
    function(input, output, session) {
      rclipButton(
        inputId = NS(id, "clipbtn"),
        label = "Copy Link to Current View",
        clipText = myURL,
        icon = icon("clipboard")
        )
    }
  )
}

selectCause <- function(id1, id2) {
  dottedSelectInput(id1, 
                    label=list("Cause of Death:", actionButton(id2, label=helpIcon, style=myInputHelpButtonSty)), choices=fullList)
}

selectLHJ <- function(id) {
  selectInput(id, "County/State:", choices = lList, selected = STATE)
}

selectMeasure <- function(id1, id2, choices = deathMeasures_Dropdown, selected = "aRate") {
  selectInput(id1, label=list("Measure:", actionButton(id2, label=helpIcon,style=myInputHelpButtonSty)), choices = deathMeasures_Dropdown, selected = selected)
}

suppressionNote <- function(id) {
  
  div(id = id,
      paste('Note: All measures associated with counts <', criticalNumber,', as well as necessary complementrary counts/measures are excluded for data de-identification purposes'),
      style="color:blue;font-size:12px;padding-left:5px;")
  
}

multiRaceNote <- function(id) {
  div(id = id, paste(multiRaceWarning, br()), style="color:red; font-size: 12px; padding-left: 5px; margin-bottom: 5px;")
}

# Download data and download chart --------------------------------------------------------------------------------

downloadData_ui <- function(id) {
  ns <- NS(id)
  downloadButton(ns("myData"), "Download Data")

}

downloadData_server <- function(id, myTab, myLHJ, myStep) {
  moduleServer(
    id,
    function(input, output, session) {

      downloadHandler(filename = function() { paste0(myTab, "-", myLHJ, "-", Sys.Date(), ".csv") },
                      content = function(file) {
                        write.csv(myStep, file, row.names = FALSE)
                        })

    }
  )
}


downloadChart_ui <- function(id) {
  ns <- NS(id)
  downloadButton(ns("myChart"), "Download Chart")

}

downloadChart_server <- function(id, myTab, myLHJ, myStep) {
  moduleServer(
    id,
    function(input, output, session) {

      downloadHandler(filename = function() { paste0(myTab, "-", myLHJ, "-", Sys.Date(), ".png") },
                      content = function(file) {
                        png(file, width = 18, height = 10, units = "in", pointsize = 10, res = 100)
                        print(myStep)
                        dev.off()
                        })

    }
  )
}


