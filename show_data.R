SERVER_show_data <- function(id, db, location_id, show_data){
  moduleServer(id, function(input, output, session){
    ns = session$ns

    observeEvent(list(req(location_id()), show_data()), {
      location_details <- dbGetQuery(db, str_glue("SELECT street_name FROM sensor_locations WHERE id = '{location_id()}';"))

      query <- str_glue("SELECT date_trunc('day', timestamp) as day, count(id) FROM car_detections WHERE location_id = {location_id()} GROUP BY day;")
      dates_with_data <- dbGetQuery(db, query) %>% tibble

      group <- function(x){
        div(class="col-lg-2 col-md-4", x)
      }

      showModal(modalDialog(size="xl", easyClose = T,
        title=str_glue("Daten {location_details$street_name}"),



        fluidRow(
          div(class="col-lg-3 col-md-6", selectInput(ns("date"), label = "Messdatum", choices = c(dates_with_data$day))),
          div(class="col-lg-4 col-md-6", selectInput(ns("car_detections_source"), label="Datenquelle", choices=c("Erkennung auf Gerät"="sensor unit", "Erkennung auf Server"="R script"))),
          div(class="col-lg-2 col-md-4", checkboxInput(ns("heatmap"), label = "Heatmap"))
        ),
        girafeOutput(ns("scatterplot")),

        plotOutput(ns("spectrum"), width = "100%"),
        fluidRow(class="spectrum_navigation",
         div(class="col-md-3 col-sm-6",
            actionButton(ns("previous_car"), "Voriges", inline=T),
            actionButton(ns("next_car"), "Nächstes", inline=T),
          )
        ),
        fluidRow(
          group(input_switch(ns("show_geometry"), "zeige Geometrie", value = F)),
          group( numericInput(ns("speed_correction"), "Geschwindigkeit", value = 30, min = 0, max = 150)),
          group( numericInput(ns("y_distance"), "Distanz zum Sensor", value = 2, min = 0.5, max = 30)),
          group( numericInput(ns("car_length"), "Fahrzeuglänge", value = 5, min = 1, max = 50, step=1)),
          group(numericInput(ns("time_offset"), "Zeitversatz", value=0, step=100))
        ),
        fluidRow(
          group(input_switch(ns("show_power"), "zeige Power", value=F)),
          group(numericInput(ns("noise_floor_cutoff"), "Noise Floor Cutoff", value=-160, step = 10)),
          group(numericInput(ns("seconds_before"), "Sekunden vorher", value=10, step=1)),
          group(numericInput(ns("seconds_after"), "Sekunden danach", value=10, step=1)),
          group(downloadButton(ns("download_data"), "downlaod numpy"))
        ),


        footer = tagList(
          actionButton(ns("close_modal"), "Schließen"),
        )
      ))
    })


    car_detections <- reactive({
      query <- str_glue("SELECT * FROM car_detections WHERE location_id = {location_id()} AND date_trunc('day', timestamp) = '{input$date}' AND source = '{input$car_detections_source}' ORDER BY timestamp;")
      dbGetQuery(db, query) %>% tibble
    })

    bin_file_timespans <- reactive({
      query <- str_glue("SELECT id, filename, start_time, end_time from file_uploads where filetype = 'spectrum' AND (date_trunc('day', start_time) = '{input$date}' OR date_trunc('day', end_time) = '{input$date}');")
      dbGetQuery(db, query) %>% tibble
    })


    output$scatterplot <- renderGirafe({
      validate(
        need(nrow(car_detections())>0, "Keine Daten zu diesem Standort vorhanden")
      )

      direction = location_details()$direction

      scatterplot <- car_detections() %>%
        mutate(Fahrtrichtung = azimuth_to_direction(c(direction+180, direction))[(isForward==1)+1]) %>%
        mutate(n = 1:n()) %>%
        ggplot() +
        scale_y_continuous(breaks=(1:12)*10, minor_breaks = F, name="Geschwindigkeit (km/h)") +
        scale_x_datetime("Uhrzeit") +
        guides(fill="none", col="none")

      if(input$heatmap){
        scatterplot <- scatterplot +
          aes(x=timestamp, y=medianSpeed, tooltip=stat(count)) +
          scale_fill_viridis_c() +
          geom_bin_2d_interactive(binwidth=c(60*60, 5), show.legend=T, drop=T) +
          geom_hline(yintercept = location_details()$osm_speed, col="red") +
          facet_grid(Fahrtrichtung~.)
      }else{
        scatterplot <- scatterplot +
          aes(x=timestamp, y=medianSpeed, col=Fahrtrichtung, tooltip = paste(timestamp, "\n", round(medianSpeed), " km/h\nFahrtrichtung", Fahrtrichtung), data_id = n) +
          geom_rect_interactive(inherit.aes = F, ymin=0, ymax=120, aes(xmin = start_time, xmax = end_time, data_id = id, tooltip = basename(filename)), fill="gray90", data = bin_file_timespans()) +
          geom_hline(yintercept = location_details()$osm_speed, col="red") +
          geom_point_interactive()
      }


      breaks <- car_detections()$timestamp %>% round_date("hour") %>% unique

      cars_per_hour <- car_detections() %>%
        mutate(timestamp = floor_date(timestamp, "hour")) %>%
        group_by(timestamp, isForward) %>%
        summarise(Anzahl=n()) %>%
        mutate(Fahrtrichtung = azimuth_to_direction(c(direction+180, direction))[(isForward==1)+1]) %>%
        ggplot() +
        aes(x=timestamp, y=Anzahl, group=Fahrtrichtung, fill=Fahrtrichtung, col=Fahrtrichtung) +
        geom_area(col=NA, alpha=.3, position = "identity") +
        geom_line(linewidth=1) +
        geom_point_interactive(size=2, mapping = aes(data_id=paste(Fahrtrichtung, timestamp), tooltip = paste0(format(timestamp, "%H:%M"), "\n", Anzahl, " Autos\npro Stunde"))) +
       #geom_histogram(aes(x=timestamp, group=(isForward==1), fill=isForward==1), col="black", breaks = breaks, position="dodge") +
        scale_x_datetime("Uhrzeit") +
        scale_y_continuous(name="Fahrzeuge\npro Stunde") +
        guides(fill="none")

      gg <- cars_per_hour / scatterplot + plot_layout(heights=c(2,4), guides = "collect")
      girafe(ggobj = gg, width_svg=1090/100, height_svg=550/100) %>% girafe_options(opts_selection(type="single"), css="fill:inherit;stroke:yellow;r:10pt;", opts_tooltip(zindex = 2000))
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

    selected_points <- reactive({
      points <- car_detections()[selected_dot(),]
      updateNumericInput(session, "speed_correction", value =  points$medianSpeed)
      return(points)
    })


    data <- reactive({
      #selected <- event_data("plotly_click", source = "scatterplot_selected")


      validate(
        need(selected_dot(), "Wählen Sie einen Messpunkt um das Spektrum dazu anzuzeigen.")
      )

      #selected_points <- car_detections()[selected$pointNumber,]

      start_time <- min(selected_points()$timestamp) - seconds(20)
      end_time <- max(selected_points()$timestamp) + seconds(20)


      byte_index <- dbGetQuery(db, str_glue("SELECT * from bin_index WHERE location_id = {location_id()} AND timestamp >= '{start_time}' AND timestamp <= '{end_time}' ORDER BY timestamp;")) %>% tibble

      hann_window <- selected_points()$hann_window
      if(is.na(hann_window)) hann_window <- 31
      if(!selected_points()$isForward) hann_window <- 0

      corrected_detection_time <- byte_index$timestamp[which(selected_points()$milliseconds == byte_index$milliseconds)- hann_window]


      start_time <- corrected_detection_time - seconds(input$seconds_before)
      end_time <- corrected_detection_time + seconds(input$seconds_after)


      byte_index <- dbGetQuery(db, str_glue("SELECT * from bin_index WHERE location_id = {location_id()} AND timestamp >= '{start_time}' AND timestamp <= '{end_time}' ORDER BY timestamp;")) %>% tibble

      validate(
        need(nrow(byte_index)>0, "Keine Rohdaten vorhanden für die ausgewählte Messung.")
      )

      file_ids = byte_index$file_id %>% unique



      filename = dbGetQuery(db, str_glue("SELECT filename from file_uploads WHERE id = {file_ids[1]};"))$filename
      index <- byte_index %>%
        filter(file_id == file_id) %>%
        arrange(timestamp) %>%
        pull(byte_index) %>%
        unique()
      data <- read_binary_file(filename, index, debug=F, read_data = T)
      return(data)
    })


    location_details <- reactive({
      dbGetQuery(db, str_glue("SELECT id, street_name, user_speedlimit, osm_speedlimit, direction from sensor_locations WHERE id = {location_id()};"))
    })


    output$spectrum <- renderPlot(res=100, {
      data <- data()
      timestamps <- data$timestamps
      milliseconds <- data$milliseconds
      metadata <- list(file_version=data$file_version, start_time=data$start_time, num_fft_bins=data$num_fft_bins, iq_measurement=data$iq_measurement, sample_rate=data$sample_rate, n=data$n)
      data <- data$data


      tick_positions <- tibble(index = 1:length(timestamps), timestamp = timestamps, milliseconds=milliseconds) %>%
        group_by(time = floor_date(timestamp, "1 seconds")) %>%
        summarise(position = first(index), milliseconds = first(milliseconds)) %>%
        mutate(label = time %>% with_tz("UTC") %>% format("%S") ) %>%
        tail(-1)

      sample_rate = metadata$sample_rate
      speed_conversion = (sample_rate/1024)/44.0
      speeds <- (1:1024-512) * speed_conversion

      data[data<(input$noise_floor_cutoff)] <- input$noise_floor_cutoff

      detection_index_position <- which(milliseconds == selected_points()$milliseconds)
      timestamp_index_position <- which.min(abs(timestamps- selected_points()$timestamp))

      message("bin timestamp: ", timestamps[detection_index_position])
      message("car timestamp: ", selected_points()$timestamp)

      hann_window <- if_else(is.na(selected_points()$hann_window), 31, selected_points()$hann_window)

      if(selected_points()$isForward){
        t0 <- timestamps[detection_index_position-hann_window]
      }else{
        t0 = timestamps[detection_index_position]
      }


      par(mai=c(.6,.6,.3,.4), bg="transparent", las=3, cex.main=1, mgp=c(1.9, .4, 0))
      image.plot(1:nrow(data), speeds, data, col=magma(100), useRaster = T, xaxt="n", yaxt="n", xlab="time (s)", ylab="speed (km/h)", main=timestamps[1] %>% format(str_glue("{location_details()$street_name} %Y-%m-%d %H:%M")), font.main = 1, legend.lab="dBFS", legend.mar =5.5, legend.line=3, zlim=c(input$noise_floor_cutoff, -40))
      par(las=1, tck=-0.015)
      axis(2)
      axis(1, tick_positions$position, tick_positions$label, mgp=c(1.8, .5, 0))

      # abline(v=detection_index_position, col="white")
      # abline(v=timestamp_index_position, lty=3, col="white")
      # abline(v=detection_index_position-31, lty=4, col="white")
      # text(mean(c(detection_index_position, timestamp_index_position)), 120, (selected_points()$timestamp-timestamps[detection_index_position]) %>% round(3) %>% paste("ms"), col="white")
      # text(mean(c(detection_index_position, timestamp_index_position)), 100, (timestamp_index_position-detection_index_position) %>% round(3) %>% paste("pixel"), col="white")
      # legend("bottomright", title="detection from", text.col="white", col="white", legend=c("milliseconds", "timestamp", "millis-hann"), lty = c(1,3,4), bty="n")

      if(input$show_power){
        mean_power <- apply(data[,speeds>10 & speeds<50], 1, mean)
        lines(1:nrow(data), mean_power, col="white")
        legend("bottomleft", legend = "mean amplitude between 10 and 50 km/h", text.col="white", bty="n")
      }
      #abline(v=which.min(abs(timestamps - selected_points()$timestamp)), lty=3)
      if(input$show_geometry){
        abline(h=0)
        car_geometry(t0=t0+milliseconds(input$time_offset), speed = (input$speed_correction), time = timestamps, milliseconds, input$y_distance, length=input$car_length*c(1,-1)[selected_points()$isForward +1])

        # geometry_data <<- list(t0=selected_points()$timestamp+milliseconds(input$time_offset), speed = isolate(input$speed_correction), time = timestamps, milliseconds=milliseconds, y=input$y_distance, length=input$car_length, speed_conversion=speed_conversion, data=data)
      }
    })

    output$download_data <- downloadHandler(
      filename = function() {
        data()$timestamps[1] %>% format(str_glue("{location_details()$street_name}_%Y-%m-%d_%H-%M.npy"))
      },
      content = function(file){
        reticulate::r_to_py(data()$data)$dump(file)
      }
    )

    observeEvent(input$close_modal,{
      removeModal()
    })
  })
}
