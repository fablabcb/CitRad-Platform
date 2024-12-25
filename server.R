library(shiny)


function(input, output, session) {
  loginForm <- eventReactive(input$go_to_login, {
    showModal(modalDialog(
      size="s",
      title="Bitte einloggen",
      textInput("username", "Benutzername"),
      passwordInput("password", "Passwort"),
      p(textOutput("login_errors")),
      footer = tagList(
        actionButton("login", "Login", class="success")
      )
    ))
    return(T)
  })

  observeEvent(input$go_to_registration, {
    showModal(modalDialog(
      size="s",
      title="Registrieren",
      textInput("register_username", "Benutzername"),
      textInput("register_email", "E-mail"),
      passwordInput("register_password", "Passwort"),
      p(textOutput("register_errors")),
      footer = tagList(
        actionButton("register", "Registrieren", class="success")
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
                            actionButton("view_only", "nur Daten anschauen"),
                            actionButton("go_to_registration", "Registrieren"),
                            actionButton("go_to_login", "Login")
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

  output$map <- renderMaplibre({
    maplibre("./neutrino.de.json",
             center=c(14.324609,51.759617), zoom=13, maxzoom=19, minzoom=12,

    ) %>%
      add_navigation_control() %>%
      add_fullscreen_control() %>%
      add_geocoder_control(collapsed = T) %>%
      add_line_layer(id = "streets", source = splitted_streets, before_id = "label-street-pedestrian", line_opacity = 1, line_width = interpolate(property = "zoom", type = list("exponential", 2), values = c(12,19), stops = c(1,60)), line_color = match_expr(
        "maxspeed",
        values = c("30", "50", "60", "100"),
        stops = c("#1f78b4", "#33a02c","#e31a1c", "#ff7f00"),
        default = "gray"
      )
      #popup = c("name", "maxspeed"),
      #tooltip = "name", hover_options = list(line_width=4)
      ) %>%
      add_legend(position="bottom-left", legend_title = "max speed", type="categorical",
                 values = c("30", "50", "60", "100"),
                 colors = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00")) %>%
      add_layers_control(layers=list("streets", "Luftbild"), collapsible = TRUE)
  })

  map_proxy <- reactive(maplibre_proxy("map", session))
  map_click <- reactive(input$map_click)



  output$add_location_UI <- SERVER_add_location("location_form", userID, map_click, map_proxy)
  show_locations_server("show_locations", userID, map_proxy)
  SERVER_upload_data("upload_data", location_id = reactive(input$map_marker_id), show_upload=reactive(input$upload_data), reactive(userID()))
  SERVER_show_data("show_data", location_id = reactive(input$show_data_for_id), show_data=reactive(input$show_data), userID)
  SERVER_my_uploads("my_uploads", userID, reactive(input$show_uploads))


  onStop(function(){
    cat(sprintf("Session %s was closed\n", session$token))
    #dbDisconnect(users)
    #dbDisconnect(content)

  })
}
