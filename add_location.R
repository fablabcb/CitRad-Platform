SERVER_add_location <- function(id, userID, add_location_Button, map_click, map_proxy){
  moduleServer(id, function(input, output, session){
    ns = session$ns

    observeEvent(input$map_zoom, once = T, {

      map_proxy %>%
        set_layout_property("streets", "line-join", "round") %>%
        set_layout_property("streets", "line-cap",  "round")
    })

    sensor_location <- reactiveVal()
    sensor_direction <- reactiveVal()
    nearest_street <- reactiveVal()

    observeEvent(add_location_Button(), {

      if(!isTruthy(userID())){
        showNotification("Sie müssen eingeloggt sein um Stationen hinzuzufügen")
        req(F)
      }
      UI(list(
        p("Standort: ", textOutput(ns("selectedLocation"), inline = T),
        textOutput(ns("selected_street"), inline=T),
        textOutput(ns("street_specifics"), inline=T)),
        uiOutput(ns("direction")),
        checkboxInput(ns("reverse"), "Messrichtung umkehren", value=FALSE),
        actionButton(ns("save_location"), "Standort speichern", class="btn-primary btn-block mt-10 mb-10"),
        actionButton(ns("cancel_location_save"), "Abbrechen")
      ))
      map_observer$resume()
    })

    map_observer <- observe(suspended = T, {
      req(!is.null(input$reverse))
      click <- req(map_click())

      location <- st_as_sf(data.frame(lon=click$lng, lat=click$lat), coords=c("lon", "lat"), crs = 4326)

      nearest_street <- splitted_streets[st_nearest_feature(location, splitted_streets ),] %>%
        mutate(icon="music")

      if(input$reverse){
        nearest_street <- nearest_street %>% st_reverse()
      }

      nearest_street(nearest_street) # save in reactiveVal

      nearest_street_points <- st_cast(nearest_street, "POINT")
      nearest_points <-st_nearest_points(location, nearest_street) %>% st_as_sf

      snap_point <- st_cast(nearest_points, "POINT")[2,]
      nearest_node_index <- st_nearest_feature(snap_point, nearest_street_points)

      if(nearest_node_index == 1 || nearest_node_index < nrow(nearest_street_points) && st_distance(nearest_street_points[nearest_node_index+1,], nearest_street_points[nearest_node_index,]) >
      st_distance(nearest_street_points[nearest_node_index+1,], snap_point)){
        nearest_line_segment <- nearest_street_points[nearest_node_index+(0:1),]
      }else{
        nearest_line_segment <- nearest_street_points[nearest_node_index-(1:0),]
      }
      street_azimuth <- as.numeric(st_geod_azimuth(nearest_street_points[nearest_line_segment,]))/2/pi*360

      map <- map_proxy() %>%
        set_paint_property(layer="label-street-residential", name="text-color", value="#000000") %>%
        clear_markers() %>%
        mapgl::add_markers(marker_id = "sensor_location", data=c(click$lng, click$lat), draggable=F) %>%
        clear_layer("nearest_street") %>%
        add_line_layer("nearest_street", source = nearest_street, line_color = "yellow", line_width = interpolate(property = "zoom", type = list("exponential", 2), values = c(12,19), stops = c(1,60))) %>%
        clear_layer("nearest_points") %>%
        add_line_layer("nearest_points", source = nearest_points, line_color = "#ccc", line_width = interpolate(property = "zoom", type = list("exponential", 2), values = c(12,19), stops = c(.2,20))) %>%
        clear_layer("nearest_street_symbol") %>%
        add_symbol_layer(
          id = "nearest_street_symbol",
          source = snap_point,
          #text_field = get_column("name"),
          text_justify = "left",
          icon_image = "icon-citrad_arrow",
          icon_offset = c(0,-9),
          icon_rotate = street_azimuth,
          icon_pitch_alignment = "map",
          icon_rotation_alignment = "map",
          icon_size = interpolate(property = "zoom", type = list("exponential", 2), values = c(12,19), stops = c(.16,10)),
          #icon_rotation_alignment = 90,
          symbol_spacing = 100,
          #icon_padding = 1,
          symbol_placement = "point",
          text_font = list("noto_sans_regular"),
          text_keep_upright = FALSE,
          icon_allow_overlap = TRUE,
          tooltip = "icon",
        ) %>%
        set_layout_property("nearest_street", "line-join", "round") %>%
        set_layout_property("nearest_street", "line-cap",  "round") %>%
        set_layout_property("nearest_points", "line-join", "round") %>%
        set_layout_property("nearest_points", "line-cap",  "round")

      if(!is.na(nearest_street$maxspeed)){
        updateNumericInput(session, "speedLimit", value = nearest_street$maxspeed)
      }

      sensor_direction(round(street_azimuth%%360))
      sensor_location(location)
    })


    output$selectedLocation <- renderText({
      if(isTruthy(sensor_location())){
        location <- sensor_location()
        message(str(location))
        paste(paste(round(st_coordinates(location),5), collapse = ", "))
      }else{
        "Bitte auf Karte auswählen"
      }
    })

    output$selected_street <- renderText({
      req(isTruthy(nearest_street()$name))
      if(is.na(nearest_street()$`name:hsb`)){
        str_glue("Strasse: {nearest_street()$name}")
      }else{
        str_glue("Strasse: {nearest_street()$name} ({nearest_street()$`name:hsb`})")
      }
    })

    output$street_specifics <- renderText({
      req(isTruthy(nearest_street()))

      if(!is.na(nearest_street()$lanes)){
        if(nearest_street()$lanes>1){
          lanes <- paste(nearest_street()$lanes, "Fahrspuren")
        }else{
          lanes <- "einspurig"
        }
      }else{
        lanes <- NULL
      }
      if(!is.na(nearest_street()$oneway)){
        oneway <- "Einbahnstraße"
      }else{
        oneway <- NULL
      }
      if(!is.na(nearest_street()$maxspeed)){
        maxspeed <- paste(nearest_street()$maxspeed, "km/h")
      }else{
        maxspeed <- NULL
      }

      paste(c(lanes, oneway, maxspeed), collapse=", ")
    })

    output$direction <- renderUI({
      req(sensor_direction())
      p("Fahrtichtung: ", azimuth_to_direction(sensor_direction()), icon("arrow-up", style = str_glue("transform: rotate({sensor_direction()}deg);")), str_glue(" ({sensor_direction()}° von Nord)"))
    })

    # observeEvent(input$map_marker_sensor_location,{
    #   marker <- input$map_marker_sensor_location
    #   sensor_location(marker[c("lng", "lat")])
    # })


    observeEvent(input$save_location, {
      showModal(modalDialog(
        title="Standort speichern",
        numericInput(ns("speedLimit"), "Geschwindigkeitsbegrenzung", value = 30, min = 10, max=100, step = 10, width = "100%"),
        textAreaInput(ns("notes"), "Notizen zur Messstelle", placeholder = "Schreibe uns wenn es an dieser Messstelle etwas besonderes gibt", rows = 6, resize="vertical", width = "100%"),
        footer = tagList(
          actionButton(ns("cancel_location_save"), "Abbrechen"),
          actionButton(ns("confirm_location_save"), "Speichern")
        )
      ))
    })

    observeEvent(input$cancel_location_save,{
      removeModal()
      map_observer$suspend()
      map_proxy() %>%
        clear_layer("nearest_street") %>%
        clear_markers() %>%
        clear_layer("nearest_points") %>%
        clear_layer("nearest_street_symbol")
      UI(NULL)
    })

    observeEvent(input$confirm_location_save, {

        nearest_street_geom = unlist(lapply(sf::st_as_binary(sf::st_geometry(nearest_street())), function(x) {
          paste(x, collapse = "")
        }))
        location_geom = unlist(lapply(sf::st_as_binary(sf::st_geometry(sensor_location())), function(x) {
          paste(x, collapse = "")
        }))

        query <- str_glue(.na="DEFAULT",
          "INSERT INTO sensor_locations (user_id, street_name, \"street_name:hsb\", user_speedlimit, osm_speedlimit, oneway, lanes, direction, notes, location_geom, street_geom) VALUES (
                  {userID()},
                  '{nearest_street()$name}',
                  '{nearest_street()$`name:hsb`}',
                  {input$speedLimit},
                  {as.integer(nearest_street()$maxspeed)},
                  {as.logical(nearest_street()$oneway)},
                  {as.integer(nearest_street()$lanes)},
                  {sensor_direction()},
                  '{input$notes}',
                  ST_SetSRID('{location_geom}'::geometry,4326),
                  ST_SetSRID('{nearest_street_geom}'::geometry,4326)
                  ) RETURNING id;"
        )
        cat(query)

        id = dbGetQuery(db, query)$id

        showNotification(str_glue("Standort wurde gespeichert mit id {id}"))
        map_observer$suspend()
        removeModal()
        map_proxy() %>%
          clear_layer("nearest_street") %>%
          clear_markers() %>%
          clear_layer("nearest_points") %>%
          clear_layer("nearest_street_symbol")
        UI(NULL)
    })

    UI <- reactiveVal()

    renderUI({
      UI()
    })

  })
}
