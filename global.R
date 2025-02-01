library(shiny)
library(bslib)
library(duckdb)
library(bcrypt)
library(stringr)
library(stringi)
library(mapgl)
library(dplyr)
library(dbplyr)
library(tidyr)
library(purrr)
library(RPostgreSQL)
library(sf)
library(lwgeom)
library(rpostgis)
library(ggplot2)
library(ggiraph)
library(patchwork)
library(fields)
library(plotly)
library(viridisLite)
library(reactable)
library(reticulate)

source("ggplot_settings.R")
source("add_location.R")
source("show_locations.R")
source("upload_data.R")
source("show_data.R")
source("my_uploads.R")
source("index_binary_file.R")
source("read_csv.R")
source("read_from_byte_index.R")
source("process_bin_to_db.R")
source("passing_car_geometry.R")
source("user_management.R")
load("osmdata_splitted.RData")
library(mailR)


options(shiny.maxRequestSize=100*1024^2)


drv <- dbDriver("PostgreSQL")

smtp_settings <- list(host.name = "w01f6c99.kasserver.com", port = 587,
     user.name = "m0738648",
     passwd = Sys.getenv("PLATFORM_MAIL_PW"), tls = TRUE)

azimuth_to_direction <- function(azimuth) {
  directions <- c("N", "NNO", "NO", "ONO", "O", "OSO", "SO", "SSO",
                  "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  index <- round(azimuth / 22.5) %% 16 + 1
  return(directions[index])
}
