SERVER_location_details <- function(id, db, userID, location_id, show_location_details_button){
  moduleServer(id, function(input, output, session){
    ns = session$ns


    observeEvent(show_location_details_button(), {

      location <-
        dbGetQuery(db, str_glue("
            SELECT
              sensor_locations.id,
              sensor_locations.user_id,
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

      my_datasets <- dbGetQuery(db, str_glue("SELECT count(*) from file_uploads WHERE location_id = {location_id()} and user_id = {userID()}"))$count

      lab <- function(x, ...){
        div(class="details", div(class="label", x), div(class="value", ...))
      }

      showModal(modalDialog(
        class="location_details",
        easyClose = T,
        title = str_glue("Details zu Standort {location_id()}"),
        lab("Straße:", location$street_name),
        lab("Sorbischer Name:", location$`street_name:hsb`),
        lab("angelegt am:", floor_date(location$date_created, "minute")),
        lab("Einbahnstraße:", ifelse(is.na(location$oneway), "unbekannt", c("nein", "ja")[location$oneway +1])),
        lab("Fahrspuren:", ifelse(is.na(location$lanes), "unbekannt", location$lanes)),
        lab("Messrichtung:", azimuth_to_direction(location$direction), icon("arrow-up", style = str_glue("transform: rotate({location$direction}deg);")), str_glue(" ({location$direction}° von Nord)")),
        lab("Geschwindigkeitsbegrenzung (OpenStreetMap): ", location$osm_speedlimit, "km/h"),
        lab("Geschwindigkeitsbegrenzung (Benutzer): ", location$user_speedlimit, "km/h"),
        if(isTruthy(location$notes)) p("Notizen: ", location$notes),
        lab("Anzahl an Datensätzen: ", if_else(is.na(location$num_files), 0, location$num_files), if_else(isTruthy(my_datasets>0), paste0("(", my_datasets, " eigene)"), "")),
        if(!is.null(userID())){
          span(if(my_datasets>0) actionButton(ns("edit_location_data"), "Datensätze verwalten", inline=T),
          if(userID()==location$user_id) actionButton(ns("edit_location"), "Standort bearbeiten", inline=T))
        }else{
          p("Bitte einloggen um Datensätze zu verwalten.")
        },
        htmlOutput(ns("location_images")),
        footer = list(
          modalButton("Schließen")
        )
      ))
    })

    images <- reactive({
      show_location_details_button()
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
      first = T
        for(i in 1:nrow(images())){
          extension <- images()$filename[i] %>% str_extract("\\.[^\\.]+$")
          hash <- images()$hash[i] %>% str_remove("\\n")
          gsub("\r", "", images()$hash[i])
          filename_out <- paste0("location_images/", hash, extension)
          if(nrow(images())>1){
            out[[i]] <- div(class=ifelse(first, "carousel-item active", "carousel-item"), img(src=filename_out, class="d-block w-100"))
          }else{
            return(img(src=filename_out, class="d-block w-100"))
          }
          first=F
        }

      buttons <- paste0(str_glue('<button type="button" data-bs-target="#carouselExampleIndicators" data-bs-slide-to="{(1:length(out))-1}" class="{ifelse(1:length(out)==1,"active","") }" aria-current="true"></button>'), collapse="")

      out <- tagList(out)
      return(HTML(str_glue('
  <div id="carouselExampleIndicators" class="carousel slide">
    <div class="carousel-indicators">
        {buttons}
    </div>
    <div class="carousel-inner">

     {out}

    </div>
    <button class="carousel-control-prev" type="button" data-bs-target="#carouselExampleIndicators" data-bs-slide="prev">
      <span class="carousel-control-prev-icon" aria-hidden="true"></span>
      <span class="visually-hidden">Previous</span>
    </button>
    <button class="carousel-control-next" type="button" data-bs-target="#carouselExampleIndicators" data-bs-slide="next">
      <span class="carousel-control-next-icon" aria-hidden="true"></span>
      <span class="visually-hidden">Next</span>
    </button>
  </div>
      ')))

    })

    return(
      list(
        edit_location_data = reactive(input$edit_location_data),
        edit_location = reactive(input$edit_location)
      )
    )
  })
}
