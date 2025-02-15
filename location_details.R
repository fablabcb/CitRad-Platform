SERVER_location_details <- function(id, db, location_id, show_location_details_button){
  moduleServer(id, function(input, output, session){
    ns = session$ns


    observeEvent(show_location_details_button(), {

      location <-
        dbGetQuery(db, str_glue("
            SELECT
              sensor_locations.id,
              sensor_locations.street_name,
              sensor_locations.date_created,
              sensor_locations.user_speedlimit,
              sensor_locations.notes,
              sensor_locations.direction,
              sensor_locations.\"street_name:hsb\",
              sensor_locations.osm_speedlimit,
              sensor_locations.oneway,
              sensor_locations.lanes,
              count.num_files
            FROM
              sensor_locations
            LEFT JOIN
              (
                SELECT
                  location_id,
                  count(*) as num_files
                FROM
                  file_uploads
                WHERE
                  location_id = {location_id()}
                GROUP BY
                  location_id
              ) as count
              ON sensor_locations.id = count.location_id
            WHERE
              sensor_locations.id = {location_id()}
                                                  "))



      showModal(modalDialog(
        easyClose = T,
        title = str_glue("Standort Details für Standort {location_id()}"),
        p("id: ", location$id),
        p("Straßenname: ", location$street_name),
        p("Straßenname Sorbisch: ", location$`street_name:hsb`),
        p("angelegt am: ", floor_date(location$date_created, "minute")),
        p("Einbahnstraße: ", c("nein", "ja")[location$oneway +1]),
        p("Fahrspuren: ", location$lanes),
        p("Messrichtung: ", azimuth_to_direction(location$direction), icon("arrow-up", style = str_glue("transform: rotate({location$direction}deg);")), str_glue(" ({location$direction}° von Nord)")),
        p("OpenStreetMap Geschwindigkeit: ", location$osm_speedlimit, "km/h"),
        textInput(ns("user_speedlimit"), "Geschwindigkeitsbegrenzung", value = location$user_speedlimit),
        p("Notizen: ", location$notes),
        p("Anzahl an Datensätzen: ", location$num_files),

        uiOutput(ns("location_images")),
        footer = list(
          modalButton("Schließen")
        )
      ))
    })

    images <- reactive({
      req(location_id())
      dbGetQuery(db, str_glue("
                SELECT
                  filename,
                  hash
                FROM
                  file_uploads
                WHERE
                  location_id = {location_id()} AND
                  filetype = 'image'
                                        "))
    })

    output$location_images <- renderUI({

      req(nrow(images())>0)
      out <- list()
      for(i in 1:nrow(images())){
        extension <- images()$filename[i] %>% str_extract("\\.[^\\.]+$")
        hash <- images()$hash[i] %>% str_remove("\\n")
        gsub("\r", "", images()$hash[i])
        filename_out <- paste0("location_images/", hash, extension)
        out[[i]] <- img(src=filename_out, class="Standort_detail")
      }
      return(out)
    })
  })
}
