eventDurationDistribution <- function(sc) {
  d <- lapply(SIDES, function(side) {
    h <- sc$event.history
    h <- h[h$KT.side == side,, drop=FALSE]
    if(nrow(h) > 0) {
      h$duration <- h$time - c(0, h$time[1:(nrow(h) - 1)])
      return(tapply(h$duration, h$event, identity, simplify=FALSE))
    } else {
      return(NULL)
    }
  })
  keys <- unique(c(names(d$L), names(d$R)))
  setNames(lapply(keys, function(key) c(d$L[[key]], d$R[[key]])), keys)
}


simulateDuration <- function(model, par, nsim=1000, ncores=6) {
  res <- mclapply(1:nsim, function(i) {
    sc <- simulate(model, par)
    d <- eventDurationDistribution(sc)
  }, mc.cores = ncores)
  
  D <- list()
  for(i in 1:nsim) {
    d <- res[[i]]
    for(k in names(d)) {
      D[[paste0(i, k)]] <- data.frame(event=k, duration=d[[k]])
    }
  }
  do.call(rbind, D)
}