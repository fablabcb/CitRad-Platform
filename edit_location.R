SERVER_edit_location <- function(id, db, userID, location_id, edit_location_button){
  moduleServer(id, function(input, output, session){
    ns = session$ns



    edit_location_modal <- reactive(isolate({
      modalDialog(size = "m",
                  title = str_glue("Standort {location_id()} bearbeiten"),
                  actionButton(ns("delete_location"), "Standort löschen", class="btn-danger"),
                  footer=list(
                    modalButton("Abbrechen")
                  )
      )
    }))

    observeEvent(edit_location_button(), ignoreInit = T, {
      showModal(edit_location_modal())
    })



    observeEvent(input$delete_location, ignoreInit = T, {
      is_my_location <- dbGetQuery(db, str_glue("Select user_id = {userID()} as my_location from sensor_locations where id = {location_id()};"))$my_location
      if(is_my_location){
        showModal(modalDialog(
          title="Standort löschen",
          p("Wollen Sie diesen Standort wirklich löschen?"),
          footer = list(
            actionButton(ns("cancel_location_delete"), "Abbrechen"),
            actionButton(ns("confirm_location_delete"), "Löschen", class="btn-danger")
          )

        ))
      }else{
        showNotification("Sie haben diesen Standort nicht angelegt")
      }

    })

    observeEvent(req(input$cancel_location_delete), ignoreInit = T,{
      showModal(edit_location_modal())
    })

    observeEvent(input$confirm_location_delete, ignoreInit = T, {
      dbGetQuery(db, str_glue("DELETE FROM sensor_locations WHERE id = ({location_id()});"))

      showNotification("Standort wurde gelöscht")
      removeModal()
    })

  })
}
