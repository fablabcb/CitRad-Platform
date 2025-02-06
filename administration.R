SERVER_administration <- function(id, userID, users_db, open_admin_panel){
  moduleServer(id, function(input, output, session){
    ns = session$ns

    user_roles <- reactive({
      if(isTruthy(userID())){
        dbGetQuery(users_db, str_glue("SELECT user_admin, location_admin, data_admin, validation from users where id = {req(userID())}"))
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

    show_admin_panel <- reactiveVal(0)
    update_users <- reactiveVal(0)

    user_table <- reactive({
      req(user_roles()$user_admin)
      update_users()
      dbGetQuery(users_db, "SELECT id, username, email_confirmed, activated, user_admin, location_admin, data_admin, validation from users ORDER BY username;")
    })

    observeEvent(open_admin_panel(), {
      show_admin_panel(show_admin_panel()+1)
    })

    observeEvent(show_admin_panel(), ignoreInit = T, {
      req(user_roles()$user_admin)

      showModal(modalDialog(title="Administration", size = "xl",
                            footer=modalButton("Schließen"),

                            reactableOutput(ns("user_administration_table"))))
      output$user_administration_table <- renderReactable(
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
      )
    })

    user_to_edit <- eventReactive(getReactableState("user_administration_table", "selected"), ignoreInit = T, {
      user <- user_table()[req(getReactableState("user_administration_table", "selected")),]
      return(user)
    })

    observeEvent(user_to_edit(), ignoreInit = F, {
      req(user_roles()$user_admin)
      user = req(user_to_edit())
      showModal(modalDialog(title="Benutzer bearbeiten", size="m",
                            textInput(ns("username"), "Benutzername", value = user$username),
                            checkboxInput(ns("email_confirmed"), "email_confirmed", user$email_confirmed),
                            checkboxInput(ns("activated"), "aktiviert", user$activated),
                            checkboxInput(ns("user_admin"), "Benutzeradmin", user$user_admin),
                            checkboxInput(ns("location_admin"), "Standortadmin", user$location_admin),
                            checkboxInput(ns("data_admin"), "Datenadmin", user$data_admin),
                            checkboxInput(ns("validation"), "Datenvalidierung", user$validation),
                            actionButton(ns("show_locations"), "Standorte verwalten", disabled = T),
                            actionButton(ns("show_data"), "Daten verwalten", disabled = T),
                            actionButton(ns("send_email"), "Email senden", disabled = T),
                            actionButton(ns("change_password"), "Passwort ändern", disabled = T),
                            actionButton(ns("delete_user"), "Benutzer löschen", disabled = T),
                            footer = list(
                              actionButton(ns("cancel_user_edit"), "Abbrechen"),
                              actionButton(ns("confirm_user_edit"), "Speichern")
                            )
                            ))
    })

    observeEvent(input$confirm_user_edit,{
      req(user_roles()$user_admin)
      user_id = user_to_edit()$id
      dbGetQuery(users_db, str_glue("UPDATE users SET
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
      show_admin_panel(show_admin_panel()+1)
    })
    observeEvent(input$cancel_user_edit,{
      show_admin_panel(show_admin_panel()+1)
    })





  })
}


