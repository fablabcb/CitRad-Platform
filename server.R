library(shiny)


function(input, output, session) {
  loginForm <- reactive({
    showModal(modalDialog(
      size="s",
      title="Bitte einloggen",
      textInput("username", "Benutzername"),
      passwordInput("password", "Passwort"),
      p(textOutput("login_errors")),
      footer = tagList(
        actionButton("login", "login", class="success")
      )
    ))
    return(T)
  })


  userID <- reactive({
    return("nanu")
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
      userinfo <- dbGetQuery(userdb, "SELECT password_hash, id FROM users WHERE username = ?",
                                list(input$username), stringsAsFactors = FALSE)

      valid = checkpw(input$password, userinfo$password_hash)

      if(valid){
        removeModal()
        return(input$username)
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
             center=c(14.324609,51.759617), zoom=12, maxzoom=19, minzoom=12,

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

  onStop(function(){
    cat(sprintf("Session %s was closed\n", session$token))
    dbDisconnect(users)
    dbDisconnect(content)

  })
}
