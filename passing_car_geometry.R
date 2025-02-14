car_geometry <- function(t0, speed, time, milliseconds, y, length){

  dt = c(NA, diff(milliseconds)) / 1000
  index <- 1:length(time)

  Cx = speed #in km/h
  Cx_ms = Cx /3.6 #in m/s
  x <- as.numeric(difftime(t0, time, "seconds"))*Cx_ms #m
  r <- sqrt((x)^2+y^2)
  dr <- c(NA, diff(r)) / dt * 3.6  # km/h

  #plot(index, dr, type="l")
  lines(index, dr, col="white")
  # abline(h=c(-Cx, Cx), col="gray")
  #abline(h=0, v=0)

  x <- x+length
  r <- sqrt(x^2+y^2)
  dr <- c(NA, diff(r)) / dt * 3.6  # km/h
  lines(index, dr, lty=2, col="white")
  axis(4, speed, labels = round(speed))

  legend("topleft", bty="n", legend = c("front", paste(length, "m")), title = "vehicle length", lty = c(1,2), col="white", text.col = "white")
  legend("topright", legend=paste(y, "m distance to sensor"), bty="n", text.col="white")

}

find_car_geometry <- function(t0, speed, time, milliseconds, y, length, speed_conversion, data){

  dt = c(NA, diff(milliseconds)) / 1000
  index <- 1:length(time)
  Cx = speed #in km/h
  Cx_ms = Cx /3.6 #in m/s

  signal_strength <- tibble(
    length = 1:50,
    mean_amplitude = NA
  )

  for(i in seq_along(signal_strength$length)){
    length = signal_strength$length[i]
    x <- as.numeric(difftime(t0, time, "seconds"))*Cx_ms #m
    x <- x+length
    r <- sqrt((x)^2+y^2)
    angle <- atan2(x, y)/(2*pi)*360
    dr <- c(NA, diff(r)) / dt * 3.6  # km/h
    bin_index <- round(dr/speed_conversion + 512)[-1]
    cells <- data[cbind(index[-1], bin_index)]
    signal_strength$mean_amplitude[i] <- mean(cells, na.rm=T)
  }
  message(signal_strength)
}
