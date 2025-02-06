SERVER_user_management <- function(id, users_db, show_profile){
  moduleServer(id, function(input, output, session){
    ns = session$ns

    userInfo <- reactiveVal()

    profile_modal <- modalDialog(easyClose = T,
                                 size="s",
                                 title=span("Profil für", textOutput(ns("user_id_text"), inline = T)),
                                 actionButton(ns("log_out"), "Ausloggen", icon= icon("right-from-bracket")),
                                 actionButton(ns("change_password"), "Passwort ändern", icon = icon("key")),
                                 actionButton(ns("delete_account"), "Account löschen", icon = icon("circle-xmark")),
                                 footer = modalButton("Schließen")
    )

    output$user_id_text <- reactive({
      req(userInfo()$name)
    })

    observeEvent(show_profile(), {
      if(isTruthy(userInfo())){
        showModal(profile_modal)
      }else{
        showModal(login_form)
      }
    })

    login_form <- modalDialog(
      easyClose = T,
      size="s",
      title="Bitte einloggen",
      textInput(ns("username"), "Benutzername"),
      passwordInput(ns("password"), "Passwort"),
      p(textOutput(ns("login_errors"))),
      p("oder ", actionLink(ns("go_to_registration"), "Registrieren")),
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton(ns("login"), "Login", class="btn-primary")
      )
    )


    observeEvent(input$go_to_registration, {
      showModal(modalDialog(easyClose = T,
        size="s",
        title="Registrierung",
        textInput(ns("register_username"), "Benutzername"),
        textInput(ns("register_email"), "E-mail"),
        passwordInput(ns("register_password"), "Passwort"),
        p(textOutput(ns("register_errors"))),
        footer = tagList(
          modalButton("Abbrechen"),
          actionButton(ns("register"), "Registrieren", class="success")
        )
      ))
    })

    observeEvent(input$register, {
      shiny::isolate({
        if(!isTruthy(input$register_username)){
          output$register_errors <- renderText("Bitte Benutzernamen angeben.")
          req(F)
        }
        if(!isTruthy(input$register_password)){
          output$register_errors <- renderText("Bitte Passwort angeben.")
          req(F)
        }
        if(!isTruthy(input$register_email)){
          output$register_errors <- renderText("Bitte Email-Adresse angeben.")
          req(F)
        }
      })

      user_exists <- dbGetQuery(users_db, str_glue("SELECT count(id) as count FROM users WHERE username = '{input$register_username}'"))


      if(user_exists$count ==1){
        output$register_errors <- renderText("Benutzer existiert bereits.")
        req(F)
      }else{

        password = hashpw(input$register_password)
        query <- str_glue("INSERT INTO users (username, email, password_hash) VALUES (
                        '{input$register_username}',
                        '{input$register_email}',
                        '{password}'
                        ) RETURNING id;")

        id = dbGetQuery(users_db, query)$id
        removeModal()
        showNotification(str_glue("Benutzer {input$register_username} registriert mit id {id}."))

        confirmation_code <- stri_rand_strings(n=1, length=30, pattern="[A-Za-z0-9]")
        confirmation_code_hashed <- hashpw(confirmation_code)
        query <- str_glue("INSERT INTO email_confirmations (code, user_id) VALUES (
                        '{confirmation_code_hashed}',
                        '{id}'
                        );")

        dbGetQuery(users_db, query)

        send.mail(from = "no-reply@citrad.de",
                  to = input$register_email,
                  subject = "Registrierung CitRad Plattform",
                  body = str_glue("Sie haben sich erfolgreich auf www.citrad.de registriert. Bitte klicken Sie auf https://data.citrad.de?user_id={id}&confirm_email={confirmation_code} um Ihre Email-Adresse zu bestätigen."),
                  smtp = smtp_settings,
                  authenticate = TRUE,
                  timeout= 6000,
                  debug=F,
                  send = TRUE)
        showNotification(str_glue("Email Bestätigung verschickt an {input$register_email}."))

      }


    })






    observe({
      text <- list()
      query <- parseQueryString(session$clientData$url_search)
      #---- confirm email ----
      if (!is.null(query[['confirm_email']])) {
        user_id = query$user_id
        confirmation_code = query$confirm_email

        confirmation_code_hashed_db <- dbGetQuery(users_db, str_glue("SELECT code FROM email_confirmations WHERE user_id = '{user_id}'"))$code

        if(length(confirmation_code_hashed_db) == 1){
          valid = checkpw(confirmation_code, confirmation_code_hashed_db)
        }else{
          valid = F
        }
        if(valid){
          dbGetQuery(users_db, str_glue("UPDATE users SET email_confirmed = true WHERE id = {user_id};"))
          dbGetQuery(users_db, str_glue("UPDATE users SET activated = true WHERE id = {user_id};"))

          dbGetQuery(users_db, str_glue("DELETE FROM email_confirmations WHERE user_id = {user_id};"))

          text <- list(text, p("Ihre Email wurde bestätigt."))
        }else{
          text <- list(text, p("Der Email Aktivierungscode ist ungültig."))
        }


        showModal(modalDialog(title="E-mail bestätigt", size="m", text, footer = list(modalButton("OK"))))
      }
      showNotification("Loggen Sie sich ein um Daten hochzuladen oder Stationen zu erstellen.")
    })

    observeEvent(input$log_out, {
      userInfo(NULL)
      showNotification("Benutzer ausgeloggt")
      removeModal()
    })

    #---- delete account ----
    observeEvent(input$delete_account, {
      showModal(modalDialog(
        title= "Account löschen",
        p("Wollen Sie Ihren Account wirklich löschen?"),
        footer=list(
          actionButton(ns("cancel_delete_account"), "Abbrechen"),
          actionButton(ns("confirm_delete_account"), "Löschen", class="btn-danger")
        )
      ))
    })

    observeEvent(input$cancel_delete_account, {
      showModal(profile_modal)
    })
    observeEvent(input$confirm_delete_account, {
      dbGetQuery(users_db, str_glue("UPDATE users SET activated = false WHERE id = '{userInfo()$id}';"))
      showNotification(str_glue('Benutzer "{userInfo()$name}" wurde gelöscht'))
      userInfo(NULL)
      showNotification("Benutzer ausgeloggt")
      removeModal()
    })

    #---- change password ----
    observeEvent(input$change_password, {
      showModal(modalDialog(title = "Passwort ändern",size = "s",
                            passwordInput(ns("old_password"), "altes Passwort"),
                            passwordInput(ns("new_password"), "neues Passwort"),
                            passwordInput(ns("confirm_new_password"), "neues Passwort bestätigen"),
                            p(textOutput(ns("change_password_errors"))),
                            footer = list(
                              modalButton("Abbrechen"),
                              actionButton(ns("confirm_change_password"), "Ändern")
                            )


      ))
    })

    observeEvent(input$confirm_change_password, {
      if(!isTruthy(input$old_password)){
        output$change_password_errors <- renderText("Bitte geben Sie Ihr altes Passwort ein.")
        return()
      }
      if(!isTruthy(input$new_password)){
        output$change_password_errors <- renderText("Bitte geben Sie ein neues Passwort ein.")
        return()
      }
      if(!isTruthy(input$confirm_new_password)){
        output$change_password_errors <- renderText("Bitte bestätigen Sie das neue Passwort.")
        return()
      }
      if(input$new_password != input$confirm_new_password){
        output$change_password_errors <- renderText("Die Passwörter stimmen nicht überein.")
        return()
      }

      userinfo <- dbGetQuery(users_db, str_glue("SELECT password_hash, id FROM users WHERE id = '{userInfo()$id}'"))

      if(nrow(userinfo) == 1){
        if(checkpw(input$old_password, userinfo$password_hash)){
          password = hashpw(input$new_password)
          dbGetQuery(users_db, str_glue("UPDATE users SET password_hash = '{password}' WHERE id = {userinfo$id};"))
          showModal(profile_modal)
          showNotification("Das Passwort wurde geändert.")
        }else{
          output$change_password_errors <- renderText("Das eingegebene alte Passwort ist ungültig.")
          return()
        }
      }

    })

    #----- process login form --------------
    observe({
      #return("nanu")
      req(input$login)

      shiny::isolate({
        if(!isTruthy(input$username)){
          output$login_errors <- renderText("Bitte Benutzernamen angeben.")
          req(F)
        }
        if(!isTruthy(input$password)){
          output$login_errors <- renderText("Bitte Passwort angeben.")
          req(F)
        }


        # Retrieve the stored hashed password from the database
        userinfo <- dbGetQuery(users_db, str_glue("SELECT username, password_hash, id, email_confirmed, activated FROM users WHERE username = '{input$username}'"))

        if(nrow(userinfo) == 1){
          valid = checkpw(input$password, userinfo$password_hash)
          if(!userinfo$email_confirmed){
            output$login_errors <- renderText("Ihre Email-Adresse wurde noch nicht bestätigt.")
            return()
          }

        }else{
          valid = F
        }

        if(valid){
          if(!userinfo$activated){
            output$login_errors <- renderText("Account wurde gelöscht")
            return()
          }
          output$login_errors <- renderText("")
          removeModal()
          userInfo(tibble(name=userinfo$username, id=userinfo$id))
        }else{
          output$login_errors <- renderText("Die Kombination aus Benutzername und Passwort ist ungültig.")
          message("failed login attempt for user ", input$username)
          req(F)
        }
      })
    })

    output$userIDtext <- renderText({
      userInfo()$name
    })

    return(userInfo)
  })
}
