library(shiny)
#page_navbar
page_sidebar(title=div(class="logo", includeHTML("www/logo.svg")), fillable_mobile = T, window_title = "CitRad",
             sidebar = sidebar(width=300,
                               textOutput("map_marker"),
                               UI_show_locations("show_locations"),
                               uiOutput("add_location_UI")
             ),
             maplibreOutput("map", height = "calc(100vh - 40px - 16px - 48px)"),

             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css")
             )
)
