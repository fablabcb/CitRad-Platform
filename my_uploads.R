SERVER_my_uploads <- function(id, db, userID, show_uploads){
  moduleServer(id, function(input, output, session){
    ns = session$ns


    observeEvent(show_uploads(), {
      showModal(modalDialog(size="xl",
        title = "Alle meine Uploads",
        reactableOutput(ns("my_uploads_table"), width = "100%"),
        footer = tagList(
          actionButton(ns("close_uploads_modal"), "Schließen", class="btn-primary")
        )
      ))
      output$my_uploads_table <- renderReactable({
        my_uploads <- dbGetQuery(db, str_glue("SELECT id, upload_date, temporary_speedlimit, location_id, filename FROM file_uploads WHERE user_id = {userID()};")) %>% as_tibble
        my_locations <- dbGetQuery(db, str_glue("SELECT id, street_name, user_speedlimit, osm_speedlimit, oneway, lanes FROM sensor_locations;")) %>% as_tibble

        my_uploads %>% left_join(my_locations, by = join_by("location_id"=="id")) %>%
          mutate(filename = basename(filename)) %>%
          mutate(location = str_glue("#{location_id} {street_name}")) %>%
          select(-id, -location_id, location, upload_date, filename, speed.osm=osm_speedlimit, speed.user=user_speedlimit, speed.temp=temporary_speedlimit, oneway, lanes) %>%
          reactable(searchable =T,
                    columns = list(
                      speed.osm = colDef(name = "osm"),
                      speed.user = colDef(name = "user"),
                      speed.temp = colDef(name = "temp")
                    ),
                    columnGroups = list(
                      colGroup(name = "speed", columns = c("speed.osm", "speed.user", "speed.temp"))
                    ),
                    showPageSizeOptions = T, compact=T, wrap=F, resizable = T)
      })
    })



    observeEvent(input$close_uploads_modal,{
      removeModal()
    })


  })


}
