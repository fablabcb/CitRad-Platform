library(shiny)

page_navbar(title=div(class="logo", includeHTML("www/logo.svg")), fillable_mobile = T, window_title = "CitRad", collapsible = F,
            theme = bs_theme("grid-float-breakpoint"="200px"),#
            nav_spacer(),
            nav_menu("Standorte", icon = icon("location-dot"),
              nav_item(actionLink("show_locations", "anzeigen", icon("eye"))),
              nav_item(actionLink("hide_locations", "verstecken", icon("eye-slash"))),
              nav_item(actionLink("add_location", "hinzufügen", icon("plus"))),
            ),
            nav_item(actionLink("show_profile", NULL, icon = icon("user"))),


            footer=list(
              maplibreOutput("map", height = "calc(100vh - 40px - 16px - 48px)"),
              navset_bar(
                nav_item(uiOutput("show_locations_UI")),
                nav_item(uiOutput("add_location_UI")),
                position = "fixed-bottom"
              ),

              tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css")

              )
            ),
)
