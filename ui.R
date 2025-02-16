page_fillable(fillable_mobile = T, title = "CitRad - Datenplattform", collapsible = F,
            useShinyjs(),
            div(class="citrad-content",
              div(class="citrad-navbar",
                  div(class="nav-items", div(class="logo", includeHTML("www/logo.svg"))),
                  div(class="nav-items nav-spacer"),
                  div(class="nav-items",
                      div(class="dropdown", tabindex="1",
                          div(class="dropbtn", icon("map"), span(class="label", "Karte")),
                          div(class="dropdown-content",
                              actionLink("show_satellite", "Luftbild", icon("satellite")),
                              actionLink("show_speed", "Geschwindigkeit", icon("gauge-high")),
                              actionLink("show_traffic", "Verkehr", icon("car-side"))
                          )
                      )
                  ),
                  div(class="nav-items",
                    div(class="dropdown", tabindex="2",
                      div(class="dropbtn", span(icon("location-dot")), span(class="label", "Standorte")),
                      div(class="dropdown-content",
                          actionLink("show_locations", "anzeigen", icon("eye")),
                          actionLink("hide_locations", "verstecken", icon("eye-slash")),
                          actionLink("add_location", "hinzufügen", icon("plus"))
                      )
                    )
                  ),

                  div(class="nav-items", actionLink("show_profile", span(class="label", "Profil"), icon = icon("user"))),
                  shinyjs::hidden(div(id="admin_menu", class="nav-items", actionLink("open_admin_panel", span(class="label", "Admin"), icon= icon("users-gear"))))

              ),
              div(class="map-area",
                maplibreOutput("map", height = "100%")
              ),
              div(class="citrad-footer",
                  uiOutput("show_locations_UI"),
                  uiOutput("add_location_UI")
              )
            ),

            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css"),
	                    tags$meta(width="device-width", "initial-scale"="1.0", "maximum-scale"="1.0", "user-scalable"="no"),
              tags$meta(name="mobile-web-app-capable", content="yes"),
              tags$meta(name="display", content="standalone")

            )
)
