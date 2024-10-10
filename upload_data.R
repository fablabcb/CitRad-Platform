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
             textOutput(ns("selectedLocation")),
             textOutput(ns("zoom"))
      ),
      column(8,
             maplibreOutput(ns("map"), height = 400)
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
        ), tooltip = "name", hover_options = list(line_width=4)) %>%
        add_legend(legend_title = "max speed", type="categorical",
                   values = c("30", "50", "60", "100"),
                   colors = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00"))
    })

    observeEvent(input$map_zoom, once = T, {

      maplibre_proxy("map", session) %>%
        set_layout_property("streets", "line-join", "round") %>%
        set_layout_property("streets", "line-cap",  "round")
    })

    sensor_location <- reactiveVal()

    observeEvent(input$map_click, {
      click <- input$map_feature_click

      maplibre_proxy("map", session) %>%
        set_paint_property(layer="label-street-residential", name="text-color", value="#000000") %>%
        clear_markers() %>%
        add_markers(marker_id = "sensor_location", data=c(click$lng, click$lat), draggable=T)

      sensor_location(click[c("lng", "lat")])
    })

    output$selectedLocation <- renderText({
      location <- req(sensor_location())
      paste("Ausgewählter Standort: ", location$lng, ", ", location$lat)
    })

    observeEvent(input$map_marker_sensor_location,{
      marker <- input$map_marker_sensor_location

      sensor_location(marker[c("lng", "lat")])
    })


    output$zoom <- renderText({
      input$map_zoom
    })

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

      if(nrow(existing_files)>0){
        showNotification(paste("Die folgenden Dateien liegen schon vor: ", paste(existing_files$name, collapse = ", ")))
        req(F)
      }else{
        file.copy(files$datapath, file.path(data_folder, files$name), copy.date = T)

        query <- str_glue(
          "INSERT INTO file_uploads (username, date, speedlimit, notes, location, files) VALUES (
                  '{userID()}',
                  '{input$measurementDate}',
                  {input$speedLimit},
                  '{input$notes}',
                  point({paste(input$map_click[c('lat', 'lng')], collapse=', ')}),
                  ARRAY['{paste(file.path(data_folder, files$name), collapse='\\', \\'')}']
                  )"
        )

        cat(query)

        dbExecute(content, query)

        showNotification("Dateien wurden hochgeladen")

      }


    })
  })
}
