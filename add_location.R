add_location_UI <- function(id, settings=NULL){
  ns <- NS(id)
  list(
    div(class="flex-confirm-container",
      div(class="flex-container",
        div(class="flex-sidebar",
               numericInput(ns("speedLimit"), "Geschwindigkeitsbegrenzung", value = 30, min = 10, max=100, step = 10),
               textAreaInput(ns("notes"), "Notizen zur Messstelle", placeholder = "Schreibe uns wenn es an dieser Messstelle etwas besonderes gibt", rows = 6, resize="vertical"),
               checkboxInput(ns("reverse"), "Fahrtrichtung umkehren", value=FALSE),
               p("Standort: ", textOutput(ns("selectedLocation"), inline = T)),
               p(textOutput(ns("selected_street"), inline=T)),
               p(textOutput(ns("street_specifics"), inline=T)),
               uiOutput(ns("direction")),
        ),
        div(class="flex-main-panel",
               maplibreOutput(ns("map"), height = "100%")
        )
      ),
      div(class="flex-confirm",
          actionButton(ns("save_location"), "Standort speichern", class="btn-primary btn-lg btn-block mt-10 mb-10")
      )
    )
  )
}


add_location_server <- function(id, userID){
  moduleServer(id, function(input, output, session){
    ns = session$ns


    output$map <- renderMaplibre({
      # other map styles: colorful, graybeard
      # https://github.com/versatiles-org/versatiles-style/releases/tag/v4.4.1
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

    observeEvent(input$map_zoom, once = T, {

      maplibre_proxy("map", session) %>%
        set_layout_property("streets", "line-join", "round") %>%
        set_layout_property("streets", "line-cap",  "round")
    })

    sensor_location <- reactiveVal()
    sensor_direction <- reactiveVal()
    nearest_street <- reactiveVal()

    observe({
      click <- req(input$map_click)
      location <- st_as_sf(data.frame(lon=click$lng, lat=click$lat), coords=c("lon", "lat"), crs = 4326)

      nearest_street <- splitted_streets[st_nearest_feature(location, splitted_streets ),] %>%
        mutate(icon="music")

      if(input$reverse){
        if(isTruthy(nearest_street$oneway)){
          showNotification("Die Fahrtrichtung der Einbahnstraße kann nicht umgekehrt werden.", type = "error")
          updateCheckboxInput(session, "reverse", value = F)
        }else{
          nearest_street <- nearest_street %>% st_reverse()
        }
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

      map <- maplibre_proxy("map", session) %>%
        set_paint_property(layer="label-street-residential", name="text-color", value="#000000") %>%
        clear_markers() %>%
        add_markers(marker_id = "sensor_location", data=c(click$lng, click$lat), draggable=F) %>%
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
          icon_image = "marking-arrow",
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

        nearest_street_geom = unlist(lapply(sf::st_as_binary(sf::st_geometry(nearest_street())), function(x) {
          paste(x, collapse = "")
        }))
        location_geom = unlist(lapply(sf::st_as_binary(sf::st_geometry(sensor_location())), function(x) {
          paste(x, collapse = "")
        }))

        query <- str_glue(.na="DEFAULT",
          "INSERT INTO sensor_locations (username, street_name, \"street_name:hsb\", user_speedlimit, osm_speedlimit, oneway, lanes, direction, notes, location_geom, street_geom) VALUES (
                  '{userID()}',
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

        id = dbGetQuery(content, query)$id

        showNotification(str_glue("Standort wurde gespeichert mit id {id}"))


    })
  })
}
