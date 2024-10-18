show_locations_UI <- function(id, settings=NULL){
  ns <- NS(id)
  list(
    div(class="flex-container",
      div(class="flex-sidebar",
             p("Standorte"),
          actionButton(ns("show_my_locations"), "Meine Standorte anzeigen"),
          actionButton(ns("show_all_locations"), "Alle Standorte anzeigen")
      ),
      div(class="flex-main-panel",
             maplibreOutput(ns("map"), height = "100%")
      )
    )
  )
}


show_locations_server <- function(id, userID){
  moduleServer(id, function(input, output, session){
    ns = session$ns

    output$map <- renderMaplibre({
      maplibre("./neutrino.de.json",
               center=c(14.324609,51.759617), zoom=12, maxzoom=19, minzoom=12,

      ) %>%
        add_navigation_control() %>%
        add_fullscreen_control() %>%
        add_geocoder_control(collapsed = T) %>%
        add_line_layer(id = "streets", source = splitted_streets, before_id = "label-street-pedestrian", line_opacity = 1, line_width = interpolate(property = "zoom", type = list("exponential", 2), values = c(12,19), stops = c(1,60)), line_color = match_expr(
          "maxspeed",
          values = c("30", "50", "60", "100"),
          stops = c("#1f78b4", "#33a02c","#e31a1c", "#ff7f00"),
          default = "gray"
        )
        #popup = c("name", "maxspeed"),
        #tooltip = "name", hover_options = list(line_width=4)
        ) %>%
        add_legend(position="bottom-left", legend_title = "max speed", type="categorical",
                   values = c("30", "50", "60", "100"),
                   colors = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00")) %>%
        add_layers_control(layers=list("streets", "Luftbild"), collapsible = TRUE)
    })

    locations <- reactiveVal()

    observeEvent(input$show_my_locations, {
      count_query =  str_glue("SELECT count(id) FROM sensor_locations WHERE username = '{userID()}';")
      query=str_glue("SELECT id, username, date_created, street_name, 'street_name.hsb', user_speedlimit, osm_speedlimit, direction, oneway, lanes, location_geom FROM sensor_locations WHERE username = \'{userID()}\';")
      count <- dbGetQuery(content, count_query)
      if(count>0){
        locations <- pgGetGeom(content, query=query, geom="location_geom")
        locations(locations)
        maplibre_proxy("map", session) %>%
          clear_markers() %>%
          add_markers(marker_id = "id", data=locations, popup = "street_name")
      }else{
        locations()
        maplibre_proxy("map", session) %>%
          clear_markers()
        showNotification("Keine Standorte vorhanden")
      }
    })

    observeEvent(input$show_all_locations, {
      count_query =  "SELECT count(id) FROM sensor_locations;"
      query="SELECT id, username, date_created, street_name, 'street_name.hsb', user_speedlimit, osm_speedlimit, direction, oneway, lanes, location_geom FROM sensor_locations;"
      count <- dbGetQuery(content, count_query)
      if(count>0){
        locations <- pgGetGeom(content, query=query, geom="location_geom")
        locations(locations)
        maplibre_proxy("map", session) %>%
          clear_markers() %>%
          add_markers(marker_id = "id", data=locations, popup = "street_name")
      }else{
        locations()
        maplibre_proxy("map", session) %>%
          clear_markers()
        showNotification("Keine Standorte vorhanden")
      }
    })

  })
}
