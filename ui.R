page_fillable(fillable_mobile = T, title = "CitRad - Datenplattform", collapsible = F,
            useShinyjs(),
            div(class="citrad-content",
              div(class="citrad-navbar",
                  div(class="nav-items", div(class="logo", includeHTML("www/logo.svg"))),
                  div(class="nav-items nav-spacer"),
                  # div(class="nav-items",
                  #     div(class="dropdown", tabindex="1",
                  #         div(class="dropbtn", icon("map"), span(class="label", "Karte")),
                  #         div(class="dropdown-content",
                  #             actionLink("show_satellite", "Luftbild", icon("satellite")),
                  #             actionLink("show_speed", "Geschwindigkeit", icon("gauge-high")),
                  #             actionLink("show_traffic", "Verkehr", icon("car-side"))
                  #         )
                  #     )
                  # ),
                  shinyjs::hidden(div(id="admin_menu", class="nav-items", actionLink("open_admin_panel", span(class="label", "Admin"), icon= icon("users-gear")))),
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
                  div(class="nav-items", actionLink("show_profile", tabindex="1", span(class="label", "Profil"), icon = icon("user")))

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
              tags$meta(name="display", content="standalone"),
              HTML(
  '<link rel="apple-touch-icon" sizes="180x180" href="favicons/apple-touch-icon.png">
  <link rel="icon" type="image/png" sizes="32x32" href="favicons/favicon-32x32.png">
  <link rel="icon" type="image/png" sizes="16x16" href="favicons/favicon-16x16.png">
  <link rel="manifest" href="favicons/site.webmanifest">
  <meta name="description" content="Verkehrsdaten messen">
  <meta property="og:url" content="https://data.citrad.de/">
  <meta property="og:site_name" content="Citizen Traffic Radar">
  <meta property="og:title" content="CitRad - Datenplattform">
  <meta property="og:description" content="CitRad - Citizen Traffic Radar: Datenplattform zum Sammeln und Visualisieren von Verkehrsdaten">
  <meta property="og:locale" content="de">
  <meta property="og:type" content="website">
  <meta property="og:image" content="https://citrad.de/featured-background.JPG">
  <meta itemprop="name" content="CitRad">
  <meta itemprop="description" content="CitRad - Citizen Traffic Radar: Datenplattform zum Sammeln und Visualisieren von Verkehrsdaten">
  <meta itemprop="dateModified" content="2024-12-24T12:39:01+01:00">
  <meta itemprop="wordCount" content="117">
  <meta itemprop="image" content="https://citrad.de/featured-background.JPG">
  <meta name="twitter:card" content="summary_large_image">
  <meta name="twitter:image" content="https://citrad.de/featured-background.JPG">
  <meta name="twitter:title" content="CitRad">
  <meta name="twitter:description" content="CitRad - Citizen Traffic Radar: Datenplattform zum Sammeln und Visualisieren von Verkehrsdaten">'
              )

            )
)
