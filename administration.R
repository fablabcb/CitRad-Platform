SERVER_administration <- function(id, userID, users_db, open_admin_panel){
  moduleServer(id, function(input, output, session){
    ns = session$ns

    user_roles <- reactive({
      req(userID())
      dbGetQuery(users_db, str_glue("SELECT role from user_roles where user_id = {req(userID())}"))$role
    })

    observe({
      req("user_admin" %in% user_roles())
      shinyjs::show("admin_menu", asis = TRUE)
    })

    observeEvent(open_admin_panel(),{
      showModal(modalDialog(title="Administration", "User Administration"))

    })

  })
}


