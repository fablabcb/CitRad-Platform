SERVER_administration <- function(id, userID, db, open_admin_panel){
  moduleServer(id, function(input, output, session){
    ns = session$ns

    observe(invalidateLater(1000, cat(".", fill = F)))

    #---- detect admin --------
    user_roles <- reactive({
      if(isTruthy(userID())){
        dbGetQuery(db, str_glue("SELECT user_admin, location_admin, data_admin, validation from users where id = {req(userID())}"))
      }else{
        NULL
      }

    })

    observe({
      if(isTruthy(user_roles()$user_admin)){
        shinyjs::show("admin_menu", asis = TRUE)
      }else{
        shinyjs::hide("admin_menu", asis = TRUE)
      }
    })

    #----- user admin panel ----
    update_users <- reactiveVal(0)
    previous_modal <- reactiveVal()

    user_table <- reactive({
      if(debug) message("query user table")
      req(user_roles()$user_admin)
      update_users()
      dbGetQuery(db, "SELECT id, username, email_confirmed, activated, user_admin, location_admin, data_admin, validation from users ORDER BY username;")
    })

    user_admin_modal <- reactive({
      modalDialog(title="Administration", size = "xl", footer=modalButton("Schließen"), easyClose = T,
        reactableOutput(ns("user_administration_table")))
    })

    observeEvent(open_admin_panel(), ignoreInit = T, {
      if(debug) message("showing user admin modal")
      req(user_roles()$user_admin)
      showModal(user_admin_modal())
    })

    output$user_administration_table <- renderReactable({
      if(debug) message("filling user administration table")
      reactable(user_table(), filterable=F, searchable = T, striped = T, selection = "single",
                onClick = "select",
                highlight = TRUE,
                fullWidth = TRUE,
                defaultColDef = colDef(minWidth = 140),
                columns = list(
                  id = colDef(width=35),
                  username = colDef(sticky = "left"),
                  email_confirmed = colDef(cell=reactable_checkmark_cell, width=150),
                  activated = colDef(cell=reactable_checkmark_cell),
                  user_admin = colDef(cell=reactable_checkmark_cell),
                  location_admin = colDef(cell=reactable_checkmark_cell),
                  data_admin = colDef(cell=reactable_checkmark_cell),
                  validation = colDef(cell=reactable_checkmark_cell)
                )
      )
    })


    user_to_edit <- eventReactive(getReactableState("user_administration_table", "selected"), ignoreInit = T, {
      if(debug) message("selecting user to edit")
      user <- user_table()[req(getReactableState("user_administration_table", "selected")),]
      return(user)
    })

    user_details_modal <- reactive({
      if(debug) message("building user details modal")
      user = req(user_to_edit())
      modalDialog(title="Benutzer bearbeiten", size="m", easyClose = T,
                  textInput(ns("username"), "Benutzername", value = user$username),
                  checkboxInput(ns("email_confirmed"), "email_confirmed", user$email_confirmed),
                  checkboxInput(ns("activated"), "aktiviert", user$activated),
                  checkboxInput(ns("user_admin"), "Benutzeradmin", user$user_admin),
                  checkboxInput(ns("location_admin"), "Standortadmin", user$location_admin),
                  checkboxInput(ns("data_admin"), "Datenadmin", user$data_admin),
                  checkboxInput(ns("validation"), "Datenvalidierung", user$validation),
                  actionButton(ns("show_locations"), "Standorte verwalten"),
                  actionButton(ns("show_data"), "Daten verwalten", disabled = T),
                  actionButton(ns("send_email"), "Email senden", disabled = T),
                  actionButton(ns("change_password"), "Passwort ändern", disabled = T),
                  actionButton(ns("delete_user"), "Benutzer löschen", disabled = T),
                  footer = list(
                    actionButton(ns("cancel_user_edit"), "Abbrechen"),
                    actionButton(ns("confirm_user_edit"), "Speichern")
                  )
      )
    })

    observeEvent(user_to_edit(), ignoreInit = F, {
      if(debug) message("showing user details modal")
      req(user_roles()$user_admin)
      showModal(user_details_modal())
    })

    observeEvent(input$confirm_user_edit,  ignoreInit = T,{
      if(debug) message("confirming user edit")
      req(user_roles()$user_admin)
      user_id = user_to_edit()$id
      dbGetQuery(db, str_glue("UPDATE users SET
                                     username = '{input$username}',
                                     email_confirmed = {sql_bool(input$email_confirmed)},
                                     activated = {sql_bool(input$activated)},
                                     user_admin = {sql_bool(input$user_admin)},
                                     location_admin = {sql_bool(input$location_admin)},
                                     data_admin = {sql_bool(input$data_admin)},
                                     validation = {sql_bool(input$validation)}
                                     WHERE id = {user_id};"))

      showNotification(str_glue("Benutzer {user_to_edit()$username} wurde geändert"))
      update_users(update_users()+1)
      showModal(user_admin_modal())
    })

    observeEvent(input$cancel_user_edit,  ignoreInit = T,{
      if(debug) message("cancel user edit")
      showModal(user_admin_modal())
    })

    #-------- edit locations -----------
    update_locations <- reactiveVal(0)

    user_location_modal <- reactive({
      if(debug) message("build locations modal")
      modalDialog(size = "xl",  easyClose = T,
                  title = str_glue("Standorte von Benutzer "), #{user_to_edit()$username}
                  reactableOutput(ns("user_locations_table")),
                  footer = list(
                    actionButton(ns("cancel_show_locations"), "Schließen")
                  )
      )
    })

    observeEvent(input$show_locations, ignoreInit = T, {
      if(debug) message("show locations modal")
      showModal(user_location_modal())
    })

    observeEvent(input$cancel_show_locations, ignoreInit = T, {
      if(debug) message("cancel show locations")
      showModal(user_details_modal())
    })


    user_locations <- reactive({
      if(debug) message("query user locations")
      req(user_roles()$user_admin)
      update_locations()

      location_files <- dbGetQuery(db, str_glue("SELECT location_id, count(*) FROM file_uploads GROUP BY location_id;"))
      #query <- str_glue("SELECT id, street_name, date_created, user_speedlimit, notes, direction, \"street_name:hsb\", osm_speedlimit, oneway, lanes, username from sensor_locations;")
      query <- str_glue("SELECT id, street_name, date_created, user_speedlimit, notes, direction, \"street_name:hsb\", osm_speedlimit, oneway, lanes from sensor_locations WHERE user_id = {user_to_edit()$id};")

      dbGetQuery(db, query) %>%
        tibble %>%
        left_join(location_files, by=join_by("id"=="location_id")) %>%
        mutate(count = if_else(is.na(count), 0, count))
    })

    output$user_locations_table <- renderReactable({
      if(debug) message("fill user locations table")
      reactable(user_locations() %>% select(id, "Straßenname"=street_name, "Angelegt"=date_created, "Geschwindigkeit"=user_speedlimit, Notizen=notes, Richtung=direction, "Anzahl Datensätze"=count) %>%
                  mutate(Richtung = azimuth_to_direction(Richtung)),
                filterable=F, searchable = T, striped = T, selection = "single",
                onClick = "select",
                highlight = TRUE,
                fullWidth = TRUE,
                defaultColDef = colDef(minWidth = 140),
                columns = list(
                  id = colDef(width=35, sticky = "left"),
                  street_name = colDef(sticky = "left"),
                  notes = colDef(minWidth = 300)
                )

      )
    })

    location_to_edit <- reactiveVal()
    observeEvent(getReactableState("user_locations_table", "selected"), ignoreInit = T, {
      if(debug) message("filter location to edit")
      location_to_edit(user_locations()[req(getReactableState("user_locations_table", "selected")),])
    })

    location_details_modal <- reactive({
      if(debug) message("building location details modal")
      location = req(location_to_edit())
      modalDialog(title="Standort bearbeiten", size="m",  easyClose = T,
                  p("id: ", location$id),
                  p("Straßenname: ", location$street_name),
                  p("Straßenname Sorbisch: ", location$`street_name:hsb`),
                  p("angelegt am: ", floor_date(location$date_created, "minute")),
                  p("Einbahnstraße: ", c("nein", "ja")[location$oneway +1]),
                  p("Fahrspuren: ", location$lanes),
                  p("Messrichtung: ", azimuth_to_direction(location$direction), icon("arrow-up", style = str_glue("transform: rotate({location$direction}deg);")), str_glue(" ({location$direction}° von Nord)")),
                  checkboxInput(ns("reverse_direction"), "Messrichtung umkehren", value = F),
                  p("OpenStreetMap Geschwindigkeit: ", location$osm_speedlimit, "km/h"),
                  textInput(ns("user_speedlimit"), "Geschwindigkeitsbegrenzung", value = location$user_speedlimit),
                  textAreaInput(ns("notes"), "Notizen", value = location$notes),
                  p("Anzahl an Datensätzen: ", location$count),


                  actionButton(ns("show_data"), "Daten verwalten", disabled = T),
                  actionButton(ns("delete_data"), "Alle Daten löschen"),
                  actionButton(ns("delete_location"), "Standort löschen"),
                  footer = list(
                    actionButton(ns("cancel_location_edit"), "Abbrechen"),
                    actionButton(ns("confirm_location_edit"), "Speichern")
                  )
      )
    })

    observeEvent(location_to_edit(), ignoreInit = F, {
      if(debug) message("show location details modal")
      req(user_roles()$user_admin)
      showModal(location_details_modal())
    })

    observeEvent(input$cancel_location_edit, ignoreInit = T, {
      if(debug) message("cancel location edit")
      showModal(user_location_modal())
    })

    observeEvent(input$confirm_location_edit,  ignoreInit = T,{
      req(user_roles()$user_admin)
      location_id = location_to_edit()$id
      direction = location_to_edit()$direction
      if(input$reverse_direction) direction <- (direction + 180) %% 360
      dbGetQuery(db, str_glue("UPDATE sensor_locations SET
                                     notes = '{input$notes}',
                                     user_speedlimit = '{input$user_speedlimit}',
                                     direction = '{direction}'
                                     WHERE id = {location_id};"))

      showNotification(str_glue("Standort wurde geändert"))
      update_locations(update_locations()+1)
      showModal(user_location_modal())
    })

    observeEvent(input$delete_data, ignoreInit = T, {

      showModal(modalDialog(
        title="Daten für Standort löschen",
        p("Wollen Sie die Daten für diesen Standort wirklich löschen?"),
        footer = list(
          actionButton(ns("cancel_data_delete"), "Abbrechen"),
          actionButton(ns("confirm_data_delete"), "Löschen", class="btn-danger")
        )
      ))
    })

    observeEvent(input$cancel_data_delete, ignoreInit = T, {
      showModal(location_details_modal())
    })

    observeEvent(input$confirm_data_delete,  ignoreInit = T,{
      if(debug) message("confirm data delete")
      dbGetQuery(db, str_glue("DELETE FROM car_detections WHERE location_id = {location_to_edit()$id};"))
      dbGetQuery(db, str_glue("DELETE FROM bin_index WHERE location_id = {location_to_edit()$id};"))
      dbGetQuery(db, str_glue("DELETE FROM raw_metrics WHERE location_id = {location_to_edit()$id};"))
      dbGetQuery(db, str_glue("DELETE FROM file_uploads WHERE location_id = {location_to_edit()$id};"))
      # AND user_id = {user_to_edit()$id}
      update_locations(update_locations()+1)
      showNotification(str_glue("Alle Daten von Standort {location_to_edit()$id} wurden gelöscht."))
      showModal(location_details_modal())
    })

    observeEvent(input$delete_location, ignoreInit = T, {
      if(debug) message("delete location")
      if(location_to_edit()$count != 0){
        showNotification("Bitte löschen Sie zuerst alle Daten dieser Station")
      }else{
        showModal(modalDialog(
          title="Standort löschen",
          p("Wollen Sie diesen Standort wirklich löschen?"),
          footer = list(
            actionButton(ns("cancel_location_delete"), "Cancel"),
            actionButton(ns("confirm_location_delete"), "Löschen", class="btn-danger")
          )
        ))
      }
    })

    observeEvent(input$confirm_location_delete, ignoreInit = T, {
      if(debug) message("confirm location delete")
      dbGetQuery(db, str_glue("DELETE FROM sensor_locations WHERE id = {location_to_edit()$id}"))
      showNotification(str_glue("Standort {location_to_edit()$id} wurde gelöscht."))
      location_to_edit(NULL)
      update_locations(update_locations()+1)
      showModal(user_location_modal())
    })

    observeEvent(input$cancel_location_delete, ignoreInit = T, {
      if(debug) message("cancel location delete")
      showModal(location_details_modal())
    })





  }) # End moduleServer
} # End Server function


