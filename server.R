
function(input, output, session) {
  cat(sprintf("New session %s\n", session$token))

  userInfo <- SERVER_user_management("user_management", users_db, reactive(input$show_profile))
  open_admin_panel <- reactive(input$open_admin_panel)
  user_roles <- SERVER_administration("administration", reactive(userInfo()$id), users_db, open_admin_panel)


  output$map <- renderMaplibre({

    count_query =  "SELECT count(id) FROM sensor_locations;"
    query="SELECT id, username, date_created, street_name, 'street_name.hsb', user_speedlimit, osm_speedlimit, direction, oneway, lanes, location_geom FROM sensor_locations;"

    count <- suppressMessages(dbGetQuery(content, count_query))
    locations <- suppressMessages(pgGetGeom(content, query=query, geom="location_geom"))

    locations <- locations %>% mutate(link = str_glue('<p class="fs-6"><span class="badge bg-secondary">{id}</span> <b>{street_name}</b></p>
                                                      <p><button onclick="Shiny.onInputChange(\'map_marker_id\', {id}); Shiny.onInputChange(\'upload_data\', Math.random());" class="btn btn-default btn-sm btn-primary">Daten hochladen</button></p>
                                                      <p><button onclick="Shiny.onInputChange(\'show_data_for_id\', {id}); Shiny.onInputChange(\'show_data\', Math.random());" class="btn btn-default btn-sm btn-primary">Daten anzeigen</button></p>'))


    maplibre("./neutrino.de.json",
             center=c(14.324609,51.759617), zoom=13, maxzoom=19, minzoom=12,

    ) %>%
      add_navigation_control() %>%
      add_fullscreen_control() %>%
      add_geocoder_control(collapsed = T) %>%
      add_line_layer(id = "streets", source = splitted_streets, before_id = "label-street-pedestrian", line_opacity = 1, line_width = interpolate(property = "zoom", type = list("exponential", 2), values = c(12,19), stops = c(1,60)), line_color = match_expr(
        "maxspeed",
        values = c("30", "50", "60", "70", "100"),
        stops = c("#1f78b4", "#33a02c", "#ff7f00","#e31a1c", "yellow"),
        default = "gray"
      )
      #popup = c("name", "maxspeed"),
      #tooltip = "name", hover_options = list(line_width=4)
      ) %>%
      add_legend(position="bottom-left", legend_title = "max speed", type="categorical",
                 values = c("30", "50", "60", "70", "100"),
                 colors = c("#1f78b4", "#33a02c", "#ff7f00","#e31a1c", "yellow")) %>%
      add_layers_control(layers=list("streets", "Luftbild"), collapsible = TRUE) %>%
      add_circle_layer(id="sensor_locations", source=st_as_sf(locations), circle_radius = 5, max_zoom = 16, popup = "link") %>%
      add_symbol_layer(id="sensors", icon_offset = c(0,-9),
                       icon_image = "icon-citrad_arrow", source=st_as_sf(locations), symbol_placement = "point",
                       min_zoom = 16,
                       icon_size = 2,
                       icon_rotate = c("get", "direction"),
                       icon_allow_overlap = T,
                       icon_rotation_alignment = "map",
                       popup = "link")

  })

  map_proxy <- reactive(maplibre_proxy("map", session))
  map_click <- reactive(input$map_click)
  add_location <- eventReactive(input$add_location, input$add_location, ignoreInit = T, ignoreNULL = T)

  show_locations <- reactive(input$show_locations)

  hide_locations <- reactive(input$hide_locations)

  output$add_location_UI <- SERVER_add_location("location_form", reactive(userInfo()$id), add_location, map_click, map_proxy)
  output$show_locations_UI <- SERVER_show_locations("show_locations", reactive(userInfo()$id), content, show_locations, hide_locations, map_proxy)
  SERVER_upload_data("upload_data", content, location_id = reactive(input$map_marker_id), show_upload=reactive(input$upload_data), reactive(userInfo()$id))
  SERVER_show_data("show_data", content, location_id = reactive(input$show_data_for_id), show_data=reactive(input$show_data))
  SERVER_my_uploads("my_uploads", content, reactive(userInfo()$id), reactive(input$show_uploads))




  onStop(function(){
    cat(sprintf("Session %s was closed\n", session$token))

  })
}
