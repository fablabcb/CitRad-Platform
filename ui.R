library(shiny)

navbarPage("CitRad",selected = "Sensor-Standorte",
           tabPanel("neuer Standort",
              add_location_UI("location_form")
           ),
           tabPanel("Sensor-Standorte",
              show_locations_UI("show_locations")
           ),
           tabPanel("data view"),
           textOutput("userIDtext"),
           tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css")
           )
)
