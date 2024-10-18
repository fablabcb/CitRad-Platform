library(duckdb)
library(bcrypt)
library(duckdb)
library(stringr)
library(mapgl)
library(dplyr)
library(dbplyr)
library(tidyr)
library(purrr)
library(RPostgreSQL)
library(sf)
library(lwgeom)
library(rpostgis)


source("add_location.R")
source("show_locations.R")
load("osmdata_splitted.RData")

options(shiny.maxRequestSize=100*1024^2)


drv <- dbDriver("PostgreSQL")
users <- dbConnect(drv, dbname = "users", host = "db", port = 5432, user = "data_platform", password = Sys.getenv("POSTGRES_data_platform_PW"))
content <- dbConnect(drv, dbname = "content", host = "db", port = 5432, user = "data_platform", password = Sys.getenv("POSTGRES_data_platform_PW"))

azimuth_to_direction <- function(azimuth) {
  directions <- c("N", "NNO", "NO", "ONO", "O", "OSO", "SO", "SSO",
                  "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  index <- round(azimuth / 22.5) %% 16 + 1
  return(directions[index])
}
