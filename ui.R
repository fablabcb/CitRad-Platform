library(shiny)

navbarPage("CitRad",
           tabPanel("upload",
              upload_UI("upload_form")
           ),
           tabPanel("data view"),
           textOutput("userIDtext"),
           tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css")
           )
)
