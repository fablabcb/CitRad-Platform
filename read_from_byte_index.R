
read_from_byte_index <- function(filename, byte_index, read_data=T, debug=F){
  if(debug) message("opening file")
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
  start_millis <- readBin(con, "integer", n=1, size=4)

  if(file_version == 1){

    # number of records:
    n <- ((size-11)/(num_fft_bins+4)) # 11 file header bytes

    # read records:
    timestamp_index <- byte_index

    if(debug) message("reading timestamps")
    millis_timestamp <- sapply(timestamp_index, read_timestamp, con=con)
    timestamps = start_time + milliseconds(millis_timestamp) - milliseconds(start_millis)


    if(read_data){
      if(debug) message("reading data")
      data <- vapply(timestamp_index+4, FUN = read_spectrum, FUN.VALUE = numeric(num_fft_bins), num_fft_bins=num_fft_bins, con=con) %>% t()
    }else{
      if(debug) message("skipping data read")
    }
  }

  if(file_version == 2){
    # number of records:
    n <- ((size-12)/(num_fft_bins+4+8)) # 11 file header bytes
    n

    start_data_block <- seek(con)
    timestamp_index <- byte_index

    if(debug) message("reading timestamps")
    integrity_field <- sapply(timestamp_index+8+num_fft_bins*D_SIZE, read_integrity_field, con=con)
    if(any(integrity_field != -1)) stop("integrity field validation failed")

    millis_timestamp <- sapply(timestamp_index, read_timestamp, con=con)
    timestamps = start_time + milliseconds(millis_timestamp) - milliseconds(start_millis)

    if(read_data){
      if(debug) message("reading data")
      data <- vapply(timestamp_index+8, FUN = read_spectrum, FUN.VALUE = numeric(num_fft_bins), num_fft_bins=num_fft_bins, con=con, byte_size=D_SIZE) %>% t()
    }else{
      if(debug) message("skipping data read")
    }

  }
  close(con)


  if(read_data) return(list(data=data, timestamps=timestamps))
}
