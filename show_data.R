SERVER_show_data <- function(id, location_id, userID){
  moduleServer(id, function(input, output, session){
    ns = session$ns

    file_ids <- reactiveVal()
    dates_with_data <- reactiveVal()

    observeEvent(location_id(), {
      location_details <- dbGetQuery(content, str_glue("SELECT street_name FROM sensor_locations WHERE id = '{location_id()}';"))
      file_ids <- dbGetQuery(content, str_glue("SELECT id FROM file_uploads WHERE location_id = '{location_id()}' AND filetype = 'car_detections';"))
      file_ids(file_ids)
      query <- str_glue("SELECT date_trunc('day', timestamp) as day, count(id) FROM car_detections WHERE file_id IN ('{paste0(file_ids$id, collapse='\\',\\'')}') GROUP BY day;")
      dates_with_data <- dbGetQuery(content, query) %>% tibble
      dates_with_data(dates_with_data)

      showModal(modalDialog(size="xl",
        title=str_glue("Daten anzeigen für {location_details$street_name}"),



        selectInput(ns("date"), label = "Messdatum", choices = c(dates_with_data$day)),
        girafeOutput(ns("scatterplot"), height = "300px"),
        plotOutput(ns("spectrum"), height = "350px"),

        footer = tagList(
          actionButton(ns("close_modal"), "Schließen"),

        )
      ))
    })


    car_detections <- eventReactive(input$date, {
      query <- str_glue("SELECT * FROM car_detections WHERE file_id IN ('{paste0(file_ids()$id, collapse='\\',\\'')}') AND date_trunc('day', timestamp) = '{dates_with_data()$day[1]}';")
      dbGetQuery(content, query) %>% tibble
    })

    output$scatterplot <- renderGirafe({
      gg <- car_detections() %>%
        ggplot() +
        aes(x=timestamp, y=medianSpeed, tooltip = paste(timestamp, "\n", medianSpeed, " km/h"), data_id = id) +
        geom_point_interactive()
      girafe(ggobj = gg, width_svg=11, height_svg=4) %>% girafe_options(opts_selection(type="single"))
      #ggplotly(gg, source="scatterplot_selected")
    })

    output$spectrum <- renderPlot({
      #selected <- event_data("plotly_click", source = "scatterplot_selected")

      selected <- input$scatterplot_selected
      validate(
        need(selected, "no points selected")
      )
      showNotification(str_glue("point {selected} selected"))
      #selected_points <- car_detections()[selected$pointNumber,]
      selected_points <- car_detections() %>% filter(id==selected)

      start_time <- min(selected_points$timestamp) - seconds(20)
      end_time <- max(selected_points$timestamp) + seconds(20)

      file_ids <- dbGetQuery(content, str_glue("SELECT id from file_uploads WHERE location_id = '{location_id()}' AND filetype = 'spectrum';"))$id

      byte_index <- dbGetQuery(content, str_glue("SELECT * from bin_index WHERE file_id IN ('{paste(file_ids, collapse='\\',\\'')}') AND timestamp >= '{start_time}' AND timestamp <= '{end_time}';")) %>% tibble

      validate(
        need(nrow(byte_index)>0, "no spectrum data for the selected car detections")
      )

      file_ids = byte_index$file_id %>% unique

      for(file_id in file_ids[1]){
        filename = dbGetQuery(content, str_glue("SELECT filename from file_uploads WHERE id = {file_id};"))$filename
        index <- byte_index %>%
          filter(file_id == file_id) %>%
          pull(byte_index)
        data <- read_from_byte_index(filename, index, debug=T)
        timestamps <- data$timestamps
        data <- data$data

      }

      par(mai=c(1,1,.1,1), bg="transparent", las=1)

      tick_positions <- tibble(index = 1:length(timestamps), timestamp = timestamps) %>%
        group_by(time = floor_date(timestamp, "1 seconds")) %>%
        summarise(position = first(index)) %>%
        mutate(label = time %>% with_tz("UTC") %>% format("%Y-%m-%d\n%H:%M:%S %Z") ) %>%

        tail(-1)

      sample_rate = 12000
      speed_conversion = (sample_rate/1024)/44.0
      speeds <- (1:1024-512) * speed_conversion



      image(1:nrow(data), speeds, data, col=magma(100), useRaster = T, xaxt="n", xlab="time", ylab="speed")
      axis(1, tick_positions$position, tick_positions$label, mgp=c(3, 2, 0))
      abline(v=which.min(abs(timestamps - selected_points$timestamp)))
    })


    observeEvent(input$close_modal,{
      removeModal()
    })
  })
}
