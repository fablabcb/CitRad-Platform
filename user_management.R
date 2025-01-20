SERVER_user_management <- function(id){
  moduleServer(id, function(input, output, session){
    ns = session$ns

    loginForm <- eventReactive(input$go_to_login, {
      showModal(modalDialog(
        size="s",
        title="Bitte einloggen",
        textInput(ns("username"), "Benutzername"),
        passwordInput(ns("password"), "Passwort"),
        p(textOutput(ns("login_errors"))),
        footer = tagList(
          actionButton(ns("login"), "Login", class="success")
        )
      ))
      return(T)
    })

    observeEvent(input$go_to_registration, {
      showModal(modalDialog(
        size="s",
        title="Registrieren",
        textInput(ns("register_username"), "Benutzername"),
        textInput(ns("register_email"), "E-mail"),
        passwordInput(ns("register_password"), "Passwort"),
        p(textOutput(ns("register_errors"))),
        footer = tagList(
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

      user_exists <- dbGetQuery(users, str_glue("SELECT count(id) as count FROM users WHERE username = '{input$register_username}'"))


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

        id = dbGetQuery(users, query)$id
        removeModal()
        showNotification(str_glue("Benutzer {input$register_username} registriert mit id {id}."))

        confirmation_code <- stri_rand_strings(n=1, length=30, pattern="[A-Za-z0-9]")
        confirmation_code_hashed <- hashpw(confirmation_code)
        query <- str_glue("INSERT INTO email_confirmations (code, user_id) VALUES (
                        '{confirmation_code_hashed}',
                        '{id}'
                        );")

        dbSendQuery(users, query)

        send.mail(from = "no-reply@citrad.de",
                  to = input$register_email,
                  subject = "Registrierung CitRad Plattform",
                  body = str_glue("Sie haben sich erfolgreich auf www.citrad.de registriert. Bitte klicken Sie auf https://data.citrad.de?user_id={id}&confirm_email={confirmation_code} um Ihre Email-Adresse zu bestätigen."),
                  smtp = smtp_settings,
                  authenticate = TRUE,
                  timeout= 6000,
                  debug=T,
                  send = TRUE)
        showNotification(str_glue("Email Bestätigung verschickt an {input$register_email}."))

      }


    })





    userID <- reactiveVal()

    observe({
      text <- list()
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[['confirm_email']])) {
        user_id = query$user_id
        confirmation_code = query$confirm_email


        confirmation_code_hashed_db <- dbGetQuery(users, str_glue("SELECT code FROM email_confirmations WHERE user_id = '{user_id}'"))$code

        if(length(confirmation_code_hashed_db) == 1){
          valid = checkpw(confirmation_code, confirmation_code_hashed_db)
        }else{
          valid = F
        }
        if(valid){
          dbSendQuery(users, str_glue("UPDATE users SET email_confirmed = true WHERE id = {user_id};"))

          dbSendQuery(users, str_glue("DELETE FROM email_confirmations WHERE user_id = {user_id};"))

          text <- list(text, p("Ihre Email wurde bestätigt."))
        }else{
          text <- list(text, p("Der Email Aktivierungscode ist ungültig."))
        }


      }
      text <- list(text, p("Loggen Sie sich ein um Daten hochzuladen oder Stationen zu erstellen."))
      showModal(modalDialog(title="Willkommen", size="m", text,
                            footer=list(
                              actionButton(ns("view_only"), "nur Daten anschauen"),
                              actionButton(ns("go_to_registration"), "Registrieren"),
                              actionButton(ns("go_to_login"), "Login")
                            )
      ))
    })

    observeEvent(input$view_only, {
      userID(NULL)
      removeModal()
    })


    #----- process login form --------------
    observe({
      #return("nanu")
      req(loginForm())
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
        userinfo <- dbGetQuery(users, str_glue("SELECT password_hash, id, email_confirmed FROM users WHERE username = '{input$username}'"))

        if(nrow(userinfo) == 1){
          valid = checkpw(input$password, userinfo$password_hash)
          if(!userinfo$email_confirmed){
            output$login_errors <- renderText("Ihre Email-Adresse wurde noch nicht bestätigt.")
            req(F)
          }
        }else{
          valid = F
        }

        if(valid){
          removeModal()
          userID(input$username)
        }else{
          output$login_errors <- renderText("Die Kombination aus Benutzername und Passwort ist ungültig.")
          message("failed login attempt for user ", input$username)
          req(F)
        }
      })
    })

    output$userIDtext <- renderText({
      userID()
    })

    return(userID)
  })
}
