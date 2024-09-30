library(shiny)

navbarPage("CitRad",
           tabPanel("upload",
            fluidRow(
              column(4,
                 textOutput("userIDtext"),
                 fileInput("files", "Dateien für den Upload", buttonLabel = "Auswählen", multiple = TRUE, accept = c(".bin", ".csv", ".png", ".jpg", ".jpeg"),placeholder = "Dateien zum hochladen"),
                 tableOutput("file_confirmation"),
                 fileInput("picture", "Photo aufnehmen", capture = "environment", accept="image/*"),
                 numericInput("speedLimit", "Geschwindigkeitsbegrenzung", value = NA, min = 10, max=100, step = 10),
                 textAreaInput("notes", "Notizen zur Messstelle", placeholder = "Schreibe uns wenn es an dieser Messstelle etwas besonderes gibt", rows = 6, resize="vertical"),
                 dateInput("measurementDate", label = "Datum der Messung", value = Sys.Date(), max=Sys.Date(), language="de"),
                 textOutput("selectedLocation")
               ),
              column(8,
                 leafletOutput("map", height = 400)
              )
            ),
            fluidRow(class="mt-3",
              column(12, class="mt-3",
                actionButton("start_upload", "upload", class="btn-primary btn-lg btn-block mt-10 mb-10")
              )
            )
           ),
           tabPanel("data view"),
           tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css")
           )
)
