library(dplyr)
library(RPostgreSQL)
library(rpostgis)
library(lubridate)
library(stringr)
library(iotools)
library(DBI)

read_timestamp <- function(index, con){
  seek(con, index)
  readBin(con, "integer", n=1, size=4)
}

read_spectrum <- function(index, con, num_fft_bins=1024, byte_size=1){
  seek(con, index)
  -readBin(con, "integer", n=num_fft_bins, size=byte_size, signed = F)
}

read_integrity_field <- function(index, con){
  seek(con, index)
  readBin(con, "integer", n=1, size=4, endian = "little")
}

postgres_time <- function(x){
  format(x, "TIMESTAMP '%Y-%-m-%-d %H:%M:%OS3'")
}

read_binary_file <- function(filename, byte_index=NA, read_data=F, shiny_notification=F, debug=F){
  if(debug) message("opening file")
  if(shiny_notification) showNotification(id = filename, HTML(basename(filename), "<br/>Binärdaten indexieren"), duration = NULL)
  size <- file.size(filename)
  con <- file(filename, open = "rb")
  # read file header:
  file_version <- readBin(con, "integer", n=1, size=2, signed = F)
  if(debug) message("file version: ", file_version)
  # read file header:
  start_time <- as.POSIXct(readBin(con, "integer", n=1, size=4), tz="UTC", origin="1970-01-01")
  num_fft_bins <- readBin(con, "integer", n=1, size=2, signed = F)
  if(file_version == 2) D_SIZE <- readBin(con, "integer", n=1, size=1, signed = F)
  iq_measurement <- readBin(con, "logical", n=1, size=1)
  sample_rate <- readBin(con, "integer", n=1, size=2, signed = F)
  if(file_version == 3) device_id <- readBin(con, "integer", n=1, size=4, signed=F)

  if(file_version == 1){

    # number of records:
    n <- ((size-11)/(num_fft_bins+4)) # 11 file header bytes
    n

    # read records:
    if(is.na(byte_index[1])){
      if(debug) message("reading full file")
      start_data_block <- seek(con)
      timestamp_index <- start_data_block + (0:(n-1))*(1024+4)
    }else{
      if(debug) message("reading file index")
      timestamp_index <- byte_index
    }


    if(debug) message("reading timestamps")
    millis_timestamp <- sapply(timestamp_index, read_timestamp, con=con)
    timestamps = start_time + milliseconds(millis_timestamp) - milliseconds(millis_timestamp[1])


    if(read_data){
      if(debug) message("reading data")
      data <- vapply(timestamp_index+4, FUN = read_spectrum, FUN.VALUE = numeric(num_fft_bins), num_fft_bins=num_fft_bins, con=con) %>% t()
    }else{
      if(debug) message("skipping data read")
    }
  }

  if(file_version == 2 | file_version == 3){
    # number of records:
    n <- ((size-12)/(num_fft_bins+4+8)) # 11 file header bytes
    n

    if(is.na(byte_index[1])){
      start_data_block <- seek(con)
      timestamp_index <- start_data_block + (0:(n-1))*(1024+4+4+4)
    }else{
      timestamp_index <- byte_index
    }

    if(debug) message("reading timestamps")
    integrity_field <- sapply(timestamp_index+8+num_fft_bins*D_SIZE, read_integrity_field, con=con)
    if(any(integrity_field != -1)) stop("integrity field validation failed")

    millis_timestamp <- sapply(timestamp_index, read_timestamp, con=con)
    timestamps = start_time + milliseconds(millis_timestamp) - milliseconds(millis_timestamp[1])

    if(read_data){
      if(debug) message("reading data")
      data <- vapply(timestamp_index+8, FUN = read_spectrum, FUN.VALUE = numeric(num_fft_bins), num_fft_bins=num_fft_bins, con=con, byte_size=D_SIZE) %>% t()
    }else{
      if(debug) message("skipping data read")
    }

  }
  close(con)
  out <- list(data=data, timestamp_index=timestamp_index, timestamps=timestamps, milliseconds=millis_timestamp, file_version=file_version, start_time=start_time, num_fft_bins=num_fft_bins, iq_measurement=iq_measurement, sample_rate=sample_rate, n=n)
  if(read_data){
    out$data = data
  }
  if(shiny_notification) removeNotification(filename)

  return(out)
}

#filename <- "uploads/1/2025-02-14/test_unit_2024-11-14_14-33-19.bin"
#filename <- "./uploads/nanu/2024-11-15/test_unit_2024-10-26_09-39-27.bin"

index_binary_file <- function(filename, id, location_id, read_data=F, debug=F, shiny_notification=F){

  data <- read_binary_file(filename=filename, byte_index = NA, read_data = read_data, shiny_notification = shiny_notification, debug = debug)


  with(data,{

    #----- update file_uploads -----
    if(debug) message("updating file_uploads db")
    query <- str_glue("UPDATE file_uploads
              SET num_fft_bins = {num_fft_bins},
                sample_rate = {sample_rate},
                start_time = {postgres_time(timestamps[1])},
                end_time = {postgres_time(last(timestamps))},
                file_version = {file_version},
                iq_measurement = {c('false', 'true')[iq_measurement+1]},
                indexed = true
              WHERE id = '{id}'")
    dbGetQuery(db, query)
    #dbGetQuery(db, "SELECT * FROM file_uploads WHERE indexed=true;")

    #---- write bin index ----
    if(debug) message("write bin index to db")
    index_table <- tibble(
      file_id = id,
      location_id = location_id,
      timestamp = timestamps,
      milliseconds = milliseconds,
      byte_index = timestamp_index
    )

    dbWriteTable(db, "bin_index", index_table, append=T, row.names=F)
    #read_data <- dbGetQuery(db, "SELECT * FROM bin_index WHERE file_id = 86 ORDER BY timestamp DESC limit 10;")
    if(read_data) return(data)
  })
}

#--------- test --------------------
#
# drv <- dbDriver("PostgreSQL")
# db <- dbConnect(drv, dbname = "db", host = "db", port = 5432, user = "data_platform", password = Sys.getenv("POSTGRES_data_platform_PW"))

# query <- str_glue("SELECT id, filename from file_uploads WHERE indexed = false;") # AND filetype = '.bin'
# files <- dbGetQuery(db, query)
#
# id = 42
# filename <- files$filename[files$id == id]
#
# index_binary_file(filename, debug=T)
#
#
# indexed_data <- dbGetQuery(db, "SELECT * FROM bin_index WHERE file_id=54;") %>% as_tibble
# indexed_data$file_id %>% unique

#
# dbGetQuery(db, "SELECT * FROM file_uploads WHERE indexed=true;")
