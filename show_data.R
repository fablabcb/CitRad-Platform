SERVER_show_data <- function(id, location_id, show_data, userID){
  moduleServer(id, function(input, output, session){
    ns = session$ns

    file_ids <- reactiveVal()
    dates_with_data <- reactiveVal()

    observeEvent(list(req(location_id()), show_data()), {
      location_details <- dbGetQuery(content, str_glue("SELECT street_name FROM sensor_locations WHERE id = '{location_id()}';"))
      file_ids <- dbGetQuery(content, str_glue("SELECT id FROM file_uploads WHERE location_id = '{location_id()}' AND filetype = 'car_detections';"))
      file_ids(file_ids)
      query <- str_glue("SELECT date_trunc('day', timestamp) as day, count(id) FROM car_detections WHERE location_id = {location_id()} GROUP BY day;")
      dates_with_data <- dbGetQuery(content, query) %>% tibble
      dates_with_data(dates_with_data)

      showModal(modalDialog(size="xl",
        title=str_glue("Daten anzeigen für {location_details$street_name}"),



        selectInput(ns("date"), label = "Messdatum", choices = c(dates_with_data$day)),
        girafeOutput(ns("scatterplot"), height = "300px"),
        plotOutput(ns("spectrum"), height = "350px"),
        fluidRow(
          column(3,
            actionButton(ns("previous_car"), "Voriges"),
            actionButton(ns("next_car"), "Nächstes"),
          ),
          column(2, input_switch(ns("show_geometry"), "zeige Geometrie", value = F)),
          column(2, numericInput(ns("y_distance"), "Distanz zum Sensor", value = 2, min = 0.5, max = 10)),
          column(2, numericInput(ns("car_length"), "Fahrzeuglänge", value = 5, min = 1, max = 50, step=1)),
          column(2, numericInput(ns("time_offset"), "Zeitversatz", value=0, step=100))
        ),

        footer = tagList(
          actionButton(ns("close_modal"), "Schließen"),

        )
      ))
    })


    car_detections <- eventReactive(input$date, {
      query <- str_glue("SELECT * FROM car_detections WHERE location_id = {location_id()} AND date_trunc('day', timestamp) = '{input$date}';")
      dbGetQuery(content, query) %>% tibble
    })


    output$scatterplot <- renderGirafe({
      gg <- car_detections() %>%
        ggplot() +
        aes(x=timestamp, y=medianSpeed, tooltip = paste(timestamp, "\n", medianSpeed, " km/h"), data_id = id) +
        scale_y_continuous(breaks=(1:12)*10, minor_breaks = F) +
        geom_point_interactive()
      girafe(ggobj = gg, width_svg=11, height_svg=3) %>% girafe_options(opts_selection(type="single"), opts_tooltip(zindex = 2000))
      #ggplotly(gg, source="scatterplot_selected")
    })

    selected_dot <- reactiveVal()

    observe({
      selected_dot(as.numeric(input$scatterplot_selected))
    })
    observeEvent(input$previous_car,{
      selected_dot(selected_dot()-1)
    })
    observeEvent(input$next_car, {
      selected_dot(selected_dot()+1)
    })

    output$spectrum <- renderPlot({
      #selected <- event_data("plotly_click", source = "scatterplot_selected")


      validate(
        need(selected_dot(), "no points selected")
      )

      #selected_points <- car_detections()[selected$pointNumber,]
      selected_points <- car_detections() %>% filter(id==selected_dot())

      start_time <- min(selected_points$timestamp) - seconds(20)
      end_time <- max(selected_points$timestamp) + seconds(20)

      file_ids <- dbGetQuery(content, str_glue("SELECT id from file_uploads WHERE location_id = '{location_id()}' AND filetype = 'spectrum';"))$id

      byte_index <- dbGetQuery(content, str_glue("SELECT * from bin_index WHERE location_id = {location_id()} AND timestamp >= '{start_time}' AND timestamp <= '{end_time}' ORDER BY timestamp;")) %>% tibble

      validate(
        need(nrow(byte_index)>0, "no spectrum data for the selected car detections")
      )

      file_ids = byte_index$file_id %>% unique


      for(file_id in file_ids[1]){
        filename = dbGetQuery(content, str_glue("SELECT filename from file_uploads WHERE id = {file_id};"))$filename
        index <- byte_index %>%
          filter(file_id == file_id) %>%
          arrange(timestamp) %>%
          pull(byte_index) %>%
          unique()
        data <- read_from_byte_index(filename, index, debug=T)
        timestamps <- data$timestamps
        milliseconds <- data$milliseconds
        data <- data$data
      }

      par(mai=c(1,1,.1,1), bg="transparent", las=1)

      tick_positions <- tibble(index = 1:length(timestamps), timestamp = timestamps) %>%
        group_by(time = floor_date(timestamp, "1 seconds")) %>%
        summarise(position = first(index)) %>%
        mutate(label = time %>% with_tz("UTC") %>% format("%H:%M:%S") ) %>%

        tail(-1)

      sample_rate = 12000
      speed_conversion = (sample_rate/1024)/44.0
      speeds <- (1:1024-512) * speed_conversion


      image(1:nrow(data), speeds, data, col=magma(100), useRaster = T, xaxt="n", xlab="time", ylab="speed")
      axis(1, tick_positions$position, tick_positions$label, mgp=c(3, 1.2, 0))
      abline(v=which.min(abs(timestamps - selected_points$timestamp)), lty=3)
      if(input$show_geometry){
        abline(h=0)
        car_geometry(t0=selected_points$timestamp+milliseconds(input$time_offset), speed = selected_points$medianSpeed, time = timestamps, milliseconds, input$y_distance, length=input$car_length)
      }
    })


    observeEvent(input$close_modal,{
      removeModal()
    })
  })
}
