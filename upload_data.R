output$map <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%
    setView(lat = 51.759617, lng = 14.324609, zoom = 12)
})

observeEvent(input$map_click, {
  click <- input$map_click
  leafletProxy("map", session) %>%
    clearMarkers() %>%
    addMarkers(lng = click$lng, lat = click$lat)

  output$selectedLocation <- renderText({
    paste("Ausgewählter Standort: ", click$lng, ", ", click$lat)
  })
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

    dbExecute(file_uploads,
              str_glue(
              "INSERT INTO file_uploads (id, username, date, speedLimit, notes, location, files) VALUES (
              nextval('seq_file_upload_id'),
              '{userID()}',
              '{input$measurementDate}',
              {input$speedLimit},
              '{input$notes}',
              row({paste(input$map_click[c('lat', 'lng')], collapse=', ')}),
              ['{paste(file.path(data_folder, files$name), collapse='\\', \\'')}']
              )"
    ))

    showNotification("Dateien wurden hochgeladen")

  }


})
