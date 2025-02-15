renv::restore()
library(shiny)
library(shinyjs)
library(bslib)
library(bcrypt)
library(stringi)
library(stringr)
library(mapgl)
library(dplyr)
library(tidyr)
#library(purrr)
library(RPostgreSQL)
library(sf)
library(lwgeom)
library(rpostgis)
library(ggplot2)
library(ggiraph)
library(patchwork)
library(fields)
#library(plotly)
library(viridisLite)
#library(reactable)
#library(reticulate)
library(mailR)
library(pool)
library(reactable)

source("ggplot_settings.R")
source("add_location.R")
source("show_locations.R")
source("upload_data.R")
source("show_data.R")
source("my_uploads.R")
source("user_management.R")
source("administration.R")
source("location_details.R")

source("index_binary_file.R")
source("read_csv.R")
#source("read_from_byte_index.R")
#source("process_bin_to_db.R")
source("passing_car_geometry.R")
load("osmdata_splitted.RData")

debug=T

options(shiny.maxRequestSize=100*1024^2)


drv <- dbDriver("PostgreSQL")
db <- dbPool(drv, dbname = "content", host = "db", port = 5432, user = "data_platform", password = Sys.getenv("POSTGRES_data_platform_PW"))

onStop(function() {
  poolClose(db)
})

smtp_settings <- list(host.name = "w01f6c99.kasserver.com", port = 587,
     user.name = "m0738648",
     passwd = Sys.getenv("PLATFORM_MAIL_PW"), tls = TRUE)

azimuth_to_direction <- function(azimuth) {
  directions <- c("N", "NNO", "NO", "ONO", "O", "OSO", "SO", "SSO",
                  "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  index <- round(azimuth / 22.5) %% 16 + 1
  return(directions[index])
}

sql_bool <- function(bool){
  if(bool){
    return("true")
  }else{
    return("false")
  }
}
reactable_checkmark_cell <- function(value) {
  if (value) {
    span("\U2714", class="checkmark")
  } else {
    span("×", class="cross")
  }
}

location_buttons <- '
      <div class="location_buttons">
        <p class="fs-6">
          <span class="badge bg-secondary">{id}</span>
          <b>{street_name}</b>
        </p>
        <p>
          <button onclick="Shiny.onInputChange(\'show_data_for_id\', {id}); Shiny.onInputChange(\'show_data\', Math.random());" class="btn btn-default btn-sm btn-primary" title="Daten anzeigen"><i class="fa-solid fa-chart-line"></i></button>
          <button onclick="Shiny.onInputChange(\'map_marker_id\', {id}); Shiny.onInputChange(\'upload_data\', Math.random());" class="btn btn-default btn-sm btn-primary" title="Daten hochladen"><i class="fa-solid fa-upload"></i></button>
          <button onclick="Shiny.onInputChange(\'show_location_details_for_id\', {id}); Shiny.onInputChange(\'show_location_details\', Math.random());" class="btn btn-default btn-sm btn-primary" title="Standort Details"><i class="fa-solid fa-circle-info"></i></button>
        </p>
      </div>
    '
