SERVER_show_locations <- function(id, userID, db, show_locations_button, hide_locations_button, map_proxy){
  moduleServer(id, function(input, output, session){
    ns = session$ns

    locations <- reactiveVal()

    UI <- reactiveVal()

    observeEvent(hide_locations_button(), {
      map_proxy() %>%
        clear_markers() %>%
        clear_layer("sensors") %>%
        clear_layer("sensor_locations")
      UI(NULL)
    })


    observeEvent(list(show_locations_button(), input$only_my_locations), {
      if(!is.null(userID())) UI(checkboxInput(ns("only_my_locations"), label = "nur meine", value = F))


      if(!is.null(userID()) && req(!is.null(input$only_my_locations)) && input$only_my_locations){
          count_query =  str_glue("SELECT count(id) FROM sensor_locations WHERE user_id = {userID()};")
          query=str_glue("SELECT id, user_id, date_created, street_name, 'street_name.hsb', user_speedlimit, osm_speedlimit, direction, oneway, lanes, location_geom FROM sensor_locations WHERE user_id = {userID()};")
      }else{
        count_query =  "SELECT count(id) FROM sensor_locations;"
        query="SELECT id, user_id, date_created, street_name, 'street_name.hsb', user_speedlimit, osm_speedlimit, direction, oneway, lanes, location_geom FROM sensor_locations;"
      }

      count <- dbGetQuery(db, count_query)

      if(isTruthy(count) & count>0){
        locations <- pgGetGeom(db, query=query, geom="location_geom")
        locations(locations)
        locations <- locations %>% mutate(link = str_glue('<p class="fs-6"><span class="badge bg-secondary">{id}</span> <b>{street_name}</b></p>
                                                          <p><button onclick="Shiny.onInputChange(\'map_marker_id\', {id}); Shiny.onInputChange(\'upload_data\', Math.random());" class="btn btn-default btn-sm btn-primary">Daten hochladen</button></p>
                                                          <p><button onclick="Shiny.onInputChange(\'show_data_for_id\', {id}); Shiny.onInputChange(\'show_data\', Math.random());" class="btn btn-default btn-sm btn-primary">Daten anzeigen</button></p>'))

        map_proxy() %>%
          clear_markers() %>%
          clear_layer("sensor_locations") %>%
          add_circle_layer(id="sensor_locations", source=st_as_sf(locations), circle_radius = 5, max_zoom = 16, popup = "link") %>%
          clear_layer("sensors") %>%
          add_symbol_layer(id="sensors", icon_offset = c(0,-9),
                           icon_image = "icon-citrad_arrow", source=st_as_sf(locations), symbol_placement = "point",
                           min_zoom = 16,
                           icon_size = 2,
                           icon_rotate = c("get", "direction"),
                           icon_allow_overlap = T,
                           icon_rotation_alignment = "map",
                           popup = "link")
          #add_markers(marker_id = "id", data=locations, popup = "street_name")
      }else{
        locations()
        map_proxy() %>%
          clear_markers()
        showNotification("Keine Standorte vorhanden")
      }
    })

    renderUI(
      UI()
    )

  })
}
