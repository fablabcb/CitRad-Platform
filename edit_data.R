SERVER_edit_data <- function(id, db, userID, location_id, edit_location_data_button){
  moduleServer(id, function(input, output, session){
    ns = session$ns

    reload_data <- reactiveVal(0)

    edit_location_modal <- reactive(isolate({
      modalDialog(size = "xl", easyClose = T,
                  title = str_glue("Daten für Standort {location_id()} verwalten"),
                  reactableOutput(ns("location_data")),
                  actionButton(ns("delete_dataset"), "Ausgewählte löschen", class="btn-danger"),
                  actionButton(ns("move_dataset"), "Zu Standort verschieben", disabled = T),
                  footer=list(
                    modalButton("Schließen")
                  )
      )
    }))

    observeEvent(edit_location_data_button(), ignoreInit = T, {
      showModal(edit_location_modal())
    })

    location_data <- reactive({
      reload_data()
      dbGetQuery(db, str_glue("SELECT id,device_id, upload_date, filetype, file_version, start_time, end_time, indexed, processed, filename from file_uploads where location_id = {location_id()} AND user_id = {userID()}")) %>% tibble
    })

    output$location_data <- renderReactable({
      location_data() %>%
        reactable(filterable=F, searchable = T, striped = T,
                  selection = "multiple", wrap = F, resizable = TRUE,
                  defaultColDef = colDef(minWidth = 200),
                  onClick = "select",
                  highlight = TRUE,
                  fullWidth = TRUE,
                  defaultPageSize = 100,
                  theme = reactableTheme(
                    rowSelectedStyle = list(backgroundColor = "#ffcc00")
                  ),
                  columns = list(
                    .selection = colDef(sticky = "left"),
                    id = colDef(minWidth=50),
                    filetype = colDef(minWidth=150),
                    file_version = colDef(minWidth=110),
                    indexed = colDef(minWidth=80),
                    processed = colDef(minWidth=100),
                    device_id = colDef(minWidth=100),
                    filename = colDef(minWidth=800)
                  )
                  )

    })

    observeEvent(input$delete_dataset, ignoreInit = T, {
      if(is.null(getReactableState("location_data", "selected"))){
        showNotification("Wählen Sie in der Liste einen oder mehrere Datensätze aus um sie zu löschen.")
      }else{
        showModal(modalDialog(
          title="Datensatz löschen",
          p("Wollen Sie diesen Datensatz wirklich löschen?"),
          footer = list(
            actionButton(ns("cancel_data_delete"), "Abbrechen"),
            actionButton(ns("confirm_data_delete"), "Löschen", class="btn-danger")
          )

        ))
      }
    })

    observeEvent(input$confirm_data_delete, ignoreInit = T, {
      ids <- location_data()[req(getReactableState("location_data", "selected")),]$id

      dbGetQuery(db, str_glue("DELETE FROM file_uploads WHERE id IN ({paste(ids,collapse=',')});"))

      showNotification("Daten wurden gelöscht")
      reload_data(reload_data()+1)
      showModal(edit_location_modal())
    })

  })
}
