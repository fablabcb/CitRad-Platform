renv::restore()
library(shiny)
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
#library(lwgeom)
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

source("ggplot_settings.R")
source("add_location.R")
source("show_locations.R")
source("upload_data.R")
source("show_data.R")
source("my_uploads.R")
source("user_management.R")

source("index_binary_file.R")
source("read_csv.R")
source("read_from_byte_index.R")
#source("process_bin_to_db.R")
source("passing_car_geometry.R")
load("osmdata_splitted.RData")


options(shiny.maxRequestSize=100*1024^2)


drv <- dbDriver("PostgreSQL")
content <- dbPool(drv, dbname = "content", host = "db", port = 5432, user = "data_platform", password = Sys.getenv("POSTGRES_data_platform_PW"))
users <- dbPool(drv, dbname = "users", host = "db", port = 5432, user = "data_platform", password = Sys.getenv("POSTGRES_data_platform_PW"))

onStop(function() {
  poolClose(content)
  poolClose(users)
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
