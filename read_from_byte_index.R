
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

read_from_byte_index <- function(filename, byte_index=NA, read_data=T, debug=F){
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
    if(is.na(byte_index[1])){
      if(debug) message("reading full file")
      start_data_block <- seek(con)-4
      timestamp_index <- start_data_block + (0:(n-1))*(1024+4)
    }else{
      if(debug) message("reading file index")
      timestamp_index <- byte_index
    }

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
    n <- ((size-12)/(num_fft_bins+4+8)) # 12 file header bytes
    n

    if(is.na(byte_index[1])){
      start_data_block <- seek(con)-4
      timestamp_index <- start_data_block + (0:(n-1))*(1024+4+4+4)
    }else{
      timestamp_index <- byte_index
    }

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

  if(read_data) return(list(data=data, timestamps=timestamps, milliseconds=millis_timestamp, metadata=list(file_version=file_version, start_time=start_time, num_fft_bins=num_fft_bins, iq_measurement=iq_measurement, sample_rate=sample_rate, n=n)))
}
