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
    return("admin")
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

  source("upload_data.R", local=T)

  onStop(function(){
    cat(sprintf("Session %s was closed\n", session$token))
    dbDisconnect(userdb)
    dbDisconnect(file_uploads)

  })
}
