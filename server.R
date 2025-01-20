library(shiny)


function(input, output, session) {
  userID <- SERVER_user_management("user_management")

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
