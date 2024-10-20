library(shiny)
#page_navbar
page_sidebar(title="CitRad", fillable_mobile = T,

             sidebar = sidebar(width=300,
                               textOutput("map_marker"),
                               accordion(multiple=F,open = "Meine Uploads",
                                         accordion_panel(
                                           "Standorte anzeigen",
                                           UI_show_locations("show_locations")
                                         ),
                                         accordion_panel(
                                           "Standort hinzufügen",
                                           uiOutput("add_location_UI")
                                         ),
                                         accordion_panel(
                                           "Meine Uploads",
                                           actionButton("show_uploads", "Zeige Uploads")
                                         )
                               )
             ),
             maplibreOutput("map", height = "calc(100vh - 40px - 16px - 48px)"),

             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css")
             )
)
