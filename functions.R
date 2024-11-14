hann_window <- function(i, n){
  (0.5 + 0.5 * cos(2 * pi * i / (n - 1))) / ((n - 1) / 2)
}

mirror_subtract <- function(x){
  un_dB <- 10^(x/10)
  diff <- un_dB - un_dB[,c(1023:1,1)]
  diff[diff<=0] <- median(diff[diff>0])
  10*log10(diff)
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
  return(c(max=max, max_rev=max_rev, max_i=max_i, max_i_rev=max_i_rev))
}

# plot(speeds, x, type="l")
# points(speeds[max_i], max, pch=20, col="red")
# points(speeds[max_i_rev], max_rev, pch=20, col="red")

simple_runmean <- function(x, n){
  runmean = vector(mode="numeric", length=length(x))
  for(i in 2:length(x)){
    runmean[i] = ((n-1) * runmean[i-1] + x[i])/n
  }
  return(runmean)
}
