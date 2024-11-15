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

  legend("topleft", bty="n", legend = c("front", paste(length, "m")), title = "car length", lty = c(1,2), col="white", text.col = "white")

}
