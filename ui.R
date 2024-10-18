library(shiny)

navbarPage("CitRad",
           tabPanel("neuer Standort",
              add_location_UI("location_form")
           ),
           tabPanel("data view"),
           textOutput("userIDtext"),
           tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css")
           )
)
