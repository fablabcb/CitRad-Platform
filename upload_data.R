upload_UI <- function(id, settings=NULL){
  ns <- NS(id)
  list(
    fluidRow(
      column(4,
             fileInput(ns("files"), "Dateien für den Upload", buttonLabel = "Auswählen", multiple = TRUE, accept = c(".bin", ".csv", ".png", ".jpg", ".jpeg"),placeholder = "Dateien zum hochladen"),
             tableOutput(ns("file_confirmation")),
             fileInput(ns("picture"), "Photo aufnehmen", capture = "environment", accept="image/*"),
             numericInput(ns("speedLimit"), "Geschwindigkeitsbegrenzung", value = NA, min = 10, max=100, step = 10),
             textAreaInput(ns("notes"), "Notizen zur Messstelle", placeholder = "Schreibe uns wenn es an dieser Messstelle etwas besonderes gibt", rows = 6, resize="vertical"),
             dateInput(ns("measurementDate"), label = "Datum der Messung", value = Sys.Date(), max=Sys.Date(), language="de"),
             checkboxInput(ns("reverse"), "reverse", value=FALSE),
             p("Standort: ", textOutput(ns("selectedLocation"), inline = T)),
             p("Fahrtichtung: ", textOutput(ns("direction"), inline = T)),
      ),
      column(8,
             maplibreOutput(ns("map"), height = 600)
      )
    ),
    fluidRow(class="mt-3",
             column(12, class="mt-3",
                    actionButton(ns("start_upload"), "upload", class="btn-primary btn-lg btn-block mt-10 mb-10")
             )
    )
  )
}


upload_server <- function(id, userID){
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






      map <- maplibre_proxy("map", session) %>%
        set_paint_property(layer="label-street-residential", name="text-color", value="#000000") %>%
        clear_markers() %>%
        add_markers(marker_id = "sensor_location", data=c(click$lng, click$lat), draggable=T) %>%
        clear_layer("nearest_street") %>%
        add_line_layer("nearest_street", source = nearest_street, line_color = "yellow", line_width = interpolate(property = "zoom", type = list("exponential", 2), values = c(12,19), stops = c(1,60))) %>%
        clear_layer("nearest_points") %>%
        add_line_layer("nearest_points", source = nearest_points, line_color = "#ccc", line_width = interpolate(property = "zoom", type = list("exponential", 2), values = c(12,19), stops = c(.5,30))) %>%
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

      sensor_direction(round(street_azimuth%%360))
      sensor_location(location)
    })


    output$selectedLocation <- renderText({
      location <- req(sensor_location())
      message(str(location))
      paste(paste(round(st_coordinates(location),5), collapse = ", "))
    })

    output$direction <- renderText({
      req(sensor_direction())
      paste0(azimuth_to_direction(sensor_direction()), " (", sensor_direction(), "° von Nord)")
    })

    # observeEvent(input$map_marker_sensor_location,{
    #   marker <- input$map_marker_sensor_location
    #   sensor_location(marker[c("lng", "lat")])
    # })


    output$file_confirmation <- renderTable({
      req(input$files)
      files <- input$files %>%
        mutate(size = as.character(fs::fs_bytes(size))) %>%
        mutate(filetype= str_extract(name, "\\.[a-z]+$")) %>%
        select(name, size, filetype)

    })

    observeEvent(input$start_upload, {
      files = input$files
      user_folder <-  file.path("./uploads", userID())
      if(!dir.exists(user_folder)) dir.create(user_folder)
      data_folder <- file.path(user_folder, input$measurementDate)
      if(!dir.exists(data_folder)) dir.create(data_folder)
      existing_files <- files[file.exists(file.path(data_folder, files$name)),]

      # if(nrow(existing_files)>0){
      #   showNotification(paste("Die folgenden Dateien liegen schon vor: ", paste(existing_files$name, collapse = ", ")))
      #   req(F)
      # }else{
        file.copy(files$datapath, file.path(data_folder, files$name), copy.date = T)

        nearest_street_geom = unlist(lapply(sf::st_as_binary(sf::st_geometry(nearest_street())), function(x) {
          paste(x, collapse = "")
        }))
        location_geom = unlist(lapply(sf::st_as_binary(sf::st_geometry(sensor_location())), function(x) {
          paste(x, collapse = "")
        }))

        query <- str_glue(.na="DEFAULT",
          "INSERT INTO file_uploads (username, date, name, \"name:hsb\", speedlimit, osm_speedlimit, oneway, lanes, direction, notes, files, location, street_geom) VALUES (
                  '{userID()}',
                  '{input$measurementDate}',
                  '{nearest_street()$name}',
                  '{nearest_street()$`name:hsb`}',
                  {input$speedLimit},
                  {as.integer(nearest_street()$maxspeed)},
                  {as.logical(nearest_street()$oneway)},
                  {as.integer(nearest_street()$lanes)},
                  {sensor_direction()},
                  '{input$notes}',
                  ARRAY['{paste(file.path(data_folder, files$name), collapse='\\', \\'')}'],
                  ST_SetSRID('{location_geom}'::geometry,4326),
                  ST_SetSRID('{nearest_street_geom}'::geometry,4326)
                  )"
        )
        cat(query)

        dbExecute(content, query)

        showNotification("Dateien wurden hochgeladen")

      # }


    })
  })
}
