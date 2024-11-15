library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(runner)
library(future.apply)
library(parallel)
cl <- makeCluster(1)

plan(multisession, workers = 1)
options(future.globals.maxSize= 5**1024^3)

source("read_from_byte_index.R")
source("noise_floor.R")
source("functions.R")


process_bin_to_db <- function(filename, file_id, location_id){
  cars <- process_bin(filename, shiny_notification=T)

  data <- cars %>%
    rename(timestamp=trigger_start_time, isForward=trigger_direction, medianSpeed = car_speed) %>%
    mutate(source="R script", file_id=file_id, location_id=location_id) %>%
    select(timestamp, isForward, medianSpeed, source, file_id, location_id) %>%
    mutate(medianSpeed = abs(medianSpeed)) %>%
    arrange(timestamp)


  dbWriteTable(content, "car_detections", data, append=T, row.names=F)

}





process_bin <- function(filename, max_trigger_speed = 50, pedestrian_speed = 10, noise_dynamic_smoothing_factor = 100, signal_threshold = 20, car_trigger_signal_smoothing_factor = 21, diff_threshold = 0.2, timeshift = 0, mirror_subtract=F, version=1, width_per_minute=300, shiny_notification=F){


  data <- read_from_byte_index(filename, read_data = T, debug=T)

  timestamps <- data$timestamps
  milliseconds <- data$milliseconds
  metadata <- data$metadata
  data <- data$data
  sample_rate = metadata$file_version
  start_time = metadata$start_time
  num_fft_bins = metadata$num_fft_bins
  iq_measurement = metadata$iq_measurement
  sample_rate = metadata$sample_rate
  n = metadata$n

  timestamp_minutes = milliseconds/1000/60

  speed_conversion = (sample_rate/1024)/44.0
  speeds <- (1:1024-512) * speed_conversion
  abs_speed_above_10 <- abs(speeds) >= 10
  speed_above_10 <- speeds >= 10
  speed_below_10 <- speeds <= 10
  abs_speed_below_50 <- abs(speeds) <= 50

  noise_floor_normalized <- noise_floor - median(noise_floor)

  message("calc mean amplitude")
  mean_amplitude <- data[, abs(speeds)>pedestrian_speed] %>% future_apply(1, mean)
  mean_amplitude_max_trigger <- data[, (abs(speeds)>pedestrian_speed & abs(speeds)<max_trigger_speed)] %>% future_apply(1, mean)
  mean_amplitude_for <- data[, speeds > 0] %>% future_apply(1, mean)
  mean_amplitude_rev <- data[, speeds < 0] %>% future_apply(1, mean)

  n = noise_dynamic_smoothing_factor
  dynamic_noise_level <- simple_runmean(mean_amplitude, n)

  message("noise floor flattening")
  if(shiny_notification) showNotification(id = filename, HTML(basename(filename) , "<br/>Rauschen herausfiltern"), duration = NULL)
  data_flattened <- data %>% future_apply(1, function(x) x-noise_floor_normalized) %>% t()
  if(mirror_subtract){
    data_flattened <- mirror_subtract(data_flattened)
  }

  find_max_speed <- function(x){
    l <- length(x)
    m =l/2
    ii = 1:(m-1)
    max = -200
    max_i = m
    for(i in ii){
      if(x[m+i]>max){
        if(x[m+i]>x[m-i]){
          max = x[m+i]
          max_i = m+i
        }
      }
    }

    max_rev = -200
    max_i_rev = 0
    m=m
    for(i in ii){
      if(x[m-i]>max_rev){
        if(x[m-i]>x[m+i]){
          max_rev = x[m-i]
          max_i_rev = m-i
        }
      }
    }
    return(c(signal_strength_for=max, signal_strength_rev=max_rev, freq_i_for=max_i, freq_i_rev=max_i_rev))
  }

  signal <- data_flattened %>% future_apply(1, find_max_speed)  %>% t %>% as_tibble

  signal$speed_for <- speeds[signal$freq_i_for]
  signal$speed_rev <- speeds[signal$freq_i_rev]

  signal$relative_signal_strength_for <- signal$signal_strength_for - dynamic_noise_level
  signal$relative_signal_strength_rev <- signal$signal_strength_rev - dynamic_noise_level


  signal$signal_detected_for <- signal$relative_signal_strength_for > signal_threshold
  signal$signal_detected_rev <- signal$relative_signal_strength_rev > signal_threshold
  signal$speed_for[!signal$signal_detected_for] <- NA
  signal$speed_rev[!signal$signal_detected_rev] <- NA

  n = car_trigger_signal_smoothing_factor

  #car_trigger_signal <- runner(mean_amplitude_max_trigger, f=weighted.mean, k=n, lag=-(n-1)/2, w= dnorm((-(n-1)/2):((n-1)/2),sd=(n-1)/8), na_pad = T)
  car_trigger_signal <- runner(mean_amplitude_max_trigger, f=weighted.mean, k=n, lag=-(n-1)/2, w= hann_window((-(n-1)/2):((n-1)/2), n), na_pad = T)

  car_trigger_direction_signal <- c(NA, diff(car_trigger_signal))


  message("car stat")
  if(shiny_notification) showNotification(id = filename, HTML(basename(filename), "<br/>Erkenne Fahrzeuge"), duration = NULL)
  trigger_start <- NA
  trigger_start_shorter <- NA
  trigger_end <- NA
  trigger_max <- NA
  trigger_min <- NA
  trigger_direction <- NA

  i = 1
  j = 1
  k = 1
  metrics = list(trigger_start=as.numeric(NA), trigger_end=NA, trigger_start_time=as.POSIXct(NA), trigger_end_time=as.POSIXct(NA), trigger_length=NA, trigger_direction=NA, trigger_duration=NA, car_speed=NA, car_length=NA)
  cars <- list(forward=metrics, reverse=metrics)

  while(i <= length(car_trigger_direction_signal)){
    if(is.na(car_trigger_direction_signal[i]) | car_trigger_direction_signal[i] < diff_threshold){
      i=i+1
    }else{
      trigger_start <- i
      trigger_start_shorter <- i
      trigger_end <- i
      trigger_max <- car_trigger_direction_signal[i]
      trigger_min <- car_trigger_direction_signal[i]
      i = i+1
      while(i <= length(car_trigger_direction_signal) && !is.na(car_trigger_direction_signal[i]) && car_trigger_direction_signal[i] >= 0){
        # calc max
        if(car_trigger_direction_signal[i] > trigger_max){
          trigger_start_shorter <- i
          trigger_max = car_trigger_direction_signal[i]
        }
        i=i+1
      }
      while(i <= length(car_trigger_direction_signal) && !is.na(car_trigger_direction_signal[i]) && car_trigger_direction_signal[i] <= 0){
        # calc min
        if(car_trigger_direction_signal[i] < trigger_min){
          trigger_min = car_trigger_direction_signal[i]
          trigger_end <- i
        }
        i=i+1
      }

      if(trigger_max > -trigger_min ){
        cars$forward$trigger_start[j] <- trigger_start_shorter
        cars$forward$trigger_end[j] <- trigger_end
        cars$forward$trigger_length[j] <- trigger_end - trigger_start
        cars$forward$trigger_start_time[j] <- timestamps[trigger_start]
        cars$forward$trigger_end_time[j] <- timestamps[trigger_end]
        cars$forward$trigger_duration[j] <- milliseconds[trigger_end] - milliseconds[trigger_start]
        cars$forward$trigger_direction[j] = 1
        # search index is the first 200 entries from last trigger to this trigger
        search_index <- max(1, cars$forward$trigger_end[j-1], na.rm = T):trigger_start %>% head(200)
        # save median speed into last trigger (speed for this trigger is still in the future)
        cars$forward$car_speed[j-1] = median(signal$speed_for[search_index], na.rm=T)
        cars$forward$car_length[j-1] = round(cars$forward$car_speed[j-1] * cars$forward$trigger_duration[j-1] /3600)
        j = j+1
      }

      if(trigger_max < -trigger_min){

        cars$reverse$trigger_start[k] = trigger_start_shorter
        cars$reverse$trigger_end[k] <- trigger_end
        cars$reverse$trigger_length[k] <- trigger_end - trigger_start_shorter
        cars$reverse$trigger_start_time[k] <- timestamps[trigger_start_shorter]
        cars$reverse$trigger_end_time[k] <- timestamps[trigger_end]
        cars$reverse$trigger_duration[k] <- milliseconds[trigger_end] - milliseconds[trigger_start]
        cars$reverse$trigger_direction[k] = -1
        # search index is the last 200 entries from last trigger to this trigger
        search_index <- max(1, cars$reverse$trigger_end[k-1], na.rm = T):trigger_start_shorter %>% tail(200)
        # save median speed into this trigger
        cars$reverse$car_speed[k] = median(signal$speed_rev[search_index], na.rm=T)
        cars$reverse$car_length[k] = round(-cars$reverse$car_speed[k] * cars$reverse$trigger_duration[k] /3600, 1)
        k=k+1
      }
      # continue if up and down cycle is over
    }
  }
  cars$forward$car_speed[j-1] <- NA
  cars$forward$car_length[j-1] <- NA

  cars_tibble <<- bind_rows(as_tibble(cars$forward), as_tibble(cars$reverse))
  if(shiny_notification) showNotification(id = filename, HTML(basename(filename), "<br/>Erkennung abgeschlossen"), duration=5)

  return(cars_tibble)
}
