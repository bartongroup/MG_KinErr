eventDurationDistribution <- function(sc) {
  d <- lapply(SIDES, function(side) {
    h <- sc$event.history
    h <- h[h$KT.side == side,, drop=FALSE]
    if(nrow(h) > 0) {
      h$dt <- h$time - c(0, h$time[1:(nrow(h) - 1)])
      # internal consistency check
      if(sum((h$dt - h$duration)^2) > 1e-16) stop("Duration disagreement")
      return(tapply(h$dt, h$event, identity, simplify=FALSE))
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


plotDurationDistribution <- function(res, par, binwidth=0.05, xmax=5) {
  breaks <- seq(0, xmax, binwidth)
  res$event <- as.character(res$event)

  x <- seq(0, 30, 0.01)
  mod <- lapply(as.character(unique(res$event)), function(ev) {
    rate <- par[[ev]]$value$rate
    y <- rate * exp(-rate * x)
    data.frame(event=ev, x=x, y=y)
  })
  mod <- do.call(rbind, mod)
  
  ggplot() +
    theme_classic() +
    geom_histogram(data=res, aes(x=duration, y=..density..), breaks=breaks) +
    geom_line(data=mod, aes(x, y), colour="red") +
    #facet_grid(event ~ ., scales = "free") +
    facet_wrap(~event, scales="free", ncol=3) +
    coord_cartesian(xlim=c(0, xmax))
}