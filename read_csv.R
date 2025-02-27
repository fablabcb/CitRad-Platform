library(readr)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)

# drv <- dbDriver("PostgreSQL")
# db <- dbConnect(drv, dbname = "content", host = "db", port = 5432, user = "data_platform", password = Sys.getenv("POSTGRES_data_platform_PW"))
#
# query <- str_glue("SELECT id, filename from file_uploads WHERE indexed = false  AND filetype = 'csv';")
# files <- dbGetQuery(db, query)
#
# id = 57
# filename <- files$filename[files$id == id]


read_car_detections <- function(filename, id, location_id, debug=F){
  header <- readLines(filename, n = 1) %>%
    str_remove("^// ") %>%
    str_split(", ") %>%
    { .[[1]] }

  keys <- sapply(header, function(x) strsplit(x, "=")[[1]][1])
  values <- sapply(header, function(x) strsplit(x, "=")[[1]][2])

  metadata <- as.vector(as.list(setNames(values, keys)))
  if(is.null(metadata$fileFormat)) metadata$fileFormat <- 1
  if(!is.null(metadata$timeOffset)) metadata$timeOffset <- as.numeric(metadata$timeOffset)
  if(is.null(metadata$teensyId)){
    device_id = NA
  }else{
    # complicated way to read a hex string in big endian:
    pairs <- metadata$teensyId %>% str_remove_all("-") %>%
      str_split("") %>% {.[[1]]}
    device_id <- paste(rev(pairs[seq(1, length(pairs), 2)]), rev(pairs[seq(2, length(pairs), 2)]), sep = "") %>%
      paste(collapse = "") %>%
      strtoi(16L)
  }

  start_time <- filename %>%
    str_extract("(?<=cars_).*") %>%
    as.POSIXct(format="%Y-%m-%d_%H-%M-%S")

  cars <- read_csv(filename, comment = "//", show_col_types = FALSE) %>%
    filter(!(is.null(medianSpeed) | is.na(medianSpeed)))

  if(nrow(cars)==0){
    showNotification(id=filename, str_glue("Die Datei {filename} enthält keine Daten."), duration = NULL)
    return()
  }

  cars <- cars %>%
    rename(milliseconds=timestamp) %>%
    mutate(timestamp = start_time + milliseconds(milliseconds) - ifelse(!is.null(metadata$timeOffset), milliseconds(metadata$timeOffset), milliseconds(milliseconds[1]))) %>%
    mutate(file_id=id) %>%
    mutate(source="sensor unit") %>%
    mutate(hann_window=metadata$carTriggerSignalSmoothingFactor) %>%
    mutate(location_id=location_id)

  query <- str_glue("UPDATE file_uploads
              SET
                start_time = {postgres_time(cars$timestamp[1])},
                end_time = {postgres_time(last(cars$timestamp))},
                file_version = {metadata$fileFormat},
                processed = true,
                device_id = '{device_id}'
              WHERE id = '{id}'")
  dbGetQuery(db, query)


  if(debug) message("writing csv data to db")
  dbWriteTable(db, "car_detections", cars, append=T, row.names=F)
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
  dbWriteTable(db, "raw_metrics", metrics, append=T, row.names=F)
}

#
# data <- dbGetQuery(db, "SELECT * FROM raw_metrics;") %>% as_tibble
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
