SERVER_upload_data <- function(id, location_id, show_upload, userID){
  moduleServer(id, function(input, output, session){
    ns = session$ns

    observeEvent(list(req(location_id()), show_upload()), {
      showModal(modalDialog(
        title=str_glue("Daten hochladen für Standort {location_id()}"),
        textAreaInput(ns("notes"), "Notizen zum Upload", placeholder = "Schreibe uns wenn es bei diesen Daten etwas besonderes gibt, z.B.: \n• \"zurzeit Baustelle\", \n• \"temporär veränderte Geschwindigkeitsbegrenzung\" oder \n• \"erhöhtes Verkehrsaufkommen wegen Umleitung\"", rows = 5, resize="vertical", width = "100%"),
        numericInput(ns("speedLimit"), "Änderung Geschwindigkeitsbegrenzung", value = NULL, min = 10, max=100, step = 10, width = "100%"),
        checkboxInput(ns("process_bin_file"), "bin-File verarbeiten", value = T),
        fileInput(ns("files"), "Datei auswählen", multiple = T, accept = c(".bin",".csv", ".png", ".jpg", ".jpeg"), width = "100%", placeholder = "bisher keine Datei ausgewählt", buttonLabel = "Auswählen"),

        footer = tagList(
          actionButton(ns("cancel_upload"), "Abbrechen"),
          input_task_button(ns("confirm_upload"), "Hochladen", class="btn-primary", label_busy = "verarbeite Daten")
        )
      ))
    })

    observeEvent(input$confirm_upload, {
      if(!isTruthy(input$files)){
        showNotification("Bitte Wählen Sie eine Datei zum hochladen aus")
      }

      files = req(input$files)
      user_folder <-  file.path("./uploads", userID())
      if(!dir.exists(user_folder)) dir.create(user_folder)
      data_folder <- file.path(user_folder, Sys.Date())
      if(!dir.exists(data_folder)) dir.create(data_folder)

      #existing_files <- files[file.exists(file.path(data_folder, files$name)),]
      # if(nrow(existing_files)>0){
      #   showNotification(paste("Die folgenden Dateien liegen schon vor: ", paste(existing_files$name, collapse = ", ")))
      #   req(F)
      # }else{

      for(i in 1:nrow(files)){
        file = files[i,]
        file.copy(file$datapath, file.path(data_folder, file$name), copy.date = T)
        filename <- file.path(data_folder, file$name)
        filetype <- str_extract(filename, '[^\\.]+$')
        if(filetype %in% c("bin", "BIN")) filetype = "spectrum"
        if(filetype == "csv" & str_detect(filename, "metrics")) filetype = "metrics"
        if(filetype == "csv" & str_detect(filename, "cars")) filetype = "car_detections"
        if(filetype %in% c("png", "jpg", "jpeg")) filetype = "image"
        query <- str_glue(.na="DEFAULT",
                          "INSERT INTO file_uploads (username, temporary_speedlimit, notes, filename, filetype, location_id) VALUES (
                    '{userID()}',
                    {as.integer(input$speedLimit)},
                    '{input$notes}',
                    '{filename}',
                    '{filetype}',
                    {location_id()}
                    ) RETURNING id;")

        id = dbGetQuery(content, query)$id
        if(filetype == "spectrum") index_binary_file(filename, id=id, location_id=location_id(), debug =T, shiny_notification=T)
        if(filetype == "spectrum" & input$process_bin_file) process_bin_to_db(filename, file_id=id, location_id=location_id())
        if(filetype == "metrics") read_metrics(filename, id, location_id=location_id())
        if(filetype == "car_detections") read_car_detections(filename, id, location_id=location_id())

        showNotification(str_glue("Datei {file$name} wurden hochgeladen mit id {id}."))
      }
      removeModal()
    })

    observeEvent(input$cancel_upload, removeModal())
  })
}
