UI_show_locations <- function(id, settings=NULL){
  ns <- NS(id)
  list(
    actionButton(ns("show_locations"), "zeige Standorte"),
    checkboxInput(ns("only_my_locations"), label = "nur meine", value = F)
  )
}


show_locations_server <- function(id, userID, map_proxy){
  moduleServer(id, function(input, output, session){
    ns = session$ns

    locations <- reactiveVal()

    observeEvent(list(input$show_locations, input$only_my_locations), {
      if(input$only_my_locations){
        count_query =  str_glue("SELECT count(id) FROM sensor_locations WHERE username = '{userID()}';")
        query=str_glue("SELECT id, username, date_created, street_name, 'street_name.hsb', user_speedlimit, osm_speedlimit, direction, oneway, lanes, location_geom FROM sensor_locations WHERE username = \'{userID()}\';")
      }else{
        count_query =  "SELECT count(id) FROM sensor_locations;"
        query="SELECT id, username, date_created, street_name, 'street_name.hsb', user_speedlimit, osm_speedlimit, direction, oneway, lanes, location_geom FROM sensor_locations;"
      }

      count <- dbGetQuery(content, count_query)

      if(isTruthy(count) & count>0){
        locations <- pgGetGeom(content, query=query, geom="location_geom")
        locations(locations)
        locations <- locations %>% mutate(link = str_glue('<p class="fs-6"><span class="badge bg-secondary">{id}</span> <b>{street_name}</b></p>
                                                          <p><button onclick="Shiny.onInputChange(\'map_marker_id\', {id}); Shiny.onInputChange(\'upload_data\', Math.random());" class="btn btn-default btn-sm btn-primary">Daten hochladen</button></p>
                                                          <p><button onclick="Shiny.onInputChange(\'show_data_for_id\', {id}); Shiny.onInputChange(\'show_data\', Math.random());" class="btn btn-default btn-sm btn-primary">Daten anzeigen</button></p>'))

        map_proxy() %>%
          clear_markers() %>%
          clear_layer("sensors") %>%
          add_symbol_layer(id="sensors", icon_image = "icon-viewpoint", source=st_as_sf(locations), symbol_placement = "point",
                           icon_size = 2,
                           icon_rotate = -90,
                           icon_allow_overlap = T,
                           popup = "link")
          #add_markers(marker_id = "id", data=locations, popup = "street_name")
      }else{
        locations()
        map_proxy() %>%
          clear_markers()
        showNotification("Keine Standorte vorhanden")
      }
    })

  })
}
