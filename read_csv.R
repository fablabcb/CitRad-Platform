library(readr)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)

# drv <- dbDriver("PostgreSQL")
# content <- dbConnect(drv, dbname = "content", host = "db", port = 5432, user = "data_platform", password = Sys.getenv("POSTGRES_data_platform_PW"))
#
# query <- str_glue("SELECT id, filename from file_uploads WHERE indexed = false  AND filetype = 'csv';")
# files <- dbGetQuery(content, query)
#
# id = 57
# filename <- files$filename[files$id == id]


read_car_detections <- function(filename, id, location_id, debug=F){
  start_time <- filename %>%
    str_extract("(?<=cars_).*") %>%
    as.POSIXct(format="%Y-%m-%d_%H-%M-%S")
  cars <- read_csv(filename, comment = "//") %>%
    rename(milliseconds=timestamp) %>%
    mutate(timestamp = start_time + milliseconds(milliseconds) - milliseconds(milliseconds[1])) %>%
    mutate(file_id=id) %>%
    mutate(location_id=location_id)
  if(debug) message("writing csv data to db")
  dbWriteTable(content, "car_detections", cars, append=T, row.names=F)
}



read_metrics <- function(filename, id, location_id, debug=F){
  if(debug) message("reading csv data from file")
  start_time <- filename %>%
    str_extract("(?<=metrics_).*") %>%
    as.POSIXct(format="%Y-%m-%d_%H-%M-%S")
  metrics <- read_csv(filename) %>%
    rename(milliseconds=timestamp) %>%
    mutate(timestamp = start_time + milliseconds(milliseconds) - milliseconds(milliseconds[1])) %>%
    mutate(file_id=id) %>%
    mutate(location_id=location_id) %>%
    select(file_id, timestamp, milliseconds, speed, speed_reverse, strength, strength_reverse, meanAmplitudeForPedestrians, meanAmplitudeForCars, meanAmplitudeForNoiseLevel, dynamic_noise_level, car_trigger_signal)

  #str_replace_all(colnames(metrics), pattern=c("^mean_amplitude$"="meanAmplitudeForNoiseLevel", "^pedestrian_mean_amplitude$"="meanAmplitudeForPedestrians", ))


  colnames(metrics) <- colnames(metrics) %>% str_remove("_[0-9]+")
  if(debug) message("writing csv data to db")
  dbWriteTable(content, "raw_metrics", metrics, append=T, row.names=F)
}

#
# data <- dbGetQuery(content, "SELECT * FROM raw_metrics;") %>% as_tibble
# colnames(data)
#
# data %>%
#   head(100000) %>%
#   ggplot() +
#   aes(x=timestamp) +
#   geom_line(aes(y=dynamic_noise_level))
#
# data %>%
#   head(100000) %>%
#   ggplot() +
#   aes(x=timestamp) +
#   geom_line(aes(y=car_trigger_signal))
