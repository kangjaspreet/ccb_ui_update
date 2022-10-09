home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "homeMainContent",
    # - Featured Stories ------------------------------------------------------------------
      div(class = "featuredStoryRow",
        fluidRow(
          column(width = 3, feature_shaCM), 
          column(width = 3, feature_xMDB), 
          column(width = 3, feature_lghcChildhood), 
          column(width = 3, feature_scoda)))
    )
  )
}

home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}