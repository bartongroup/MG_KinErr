eventDurationDistribution <- function(sc) {
  d <- lapply(KTS, function(id) {
    h <- sc$event.history
    h <- h[h$KT.id == id,, drop=FALSE]
    if(nrow(h) > 0) {
      #h$dt <- h$time - c(0, h$time[1:(nrow(h) - 1)])
      # internal consistency check
      #if(sum((h$dt - h$duration)^2) > 1e-16) stop("Duration disagreement")
      h[, c("KT.id", "event", "duration")]
    } else {
      return(NULL)
    }
  })
  do.call(rbind, d)
}


simulateDuration <- function(model, par, nsim=1000, ncores=6) {
  res <- mclapply(1:nsim, function(i) {
    sc <- simulate(model, par)
    d <- eventDurationDistribution(sc)
  }, mc.cores = ncores)
  res <- do.call(rbind, res)
  rownames(res) <- NULL
  res
}


testGenerateFunction <- function(nsim=100000, ncores=16) {
  par <- parametersRates()
  gt <- mclapply(1:nsim, function(i) {
    d <- lapply(names(par), function(ev) {
      data.frame(
        event = ev,
        duration = generateTime(ev, par)
      )  
    })
    do.call(rbind, d)
  }, mc.cores = ncores)
  sim <- do.call(rbind, gt)
}



plotDurationDistribution <- function(res, par, binwidth=0.05, xmax=5, ncol=3) {
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
    facet_wrap(~event, scales="free", ncol=ncol) +
    coord_cartesian(xlim=c(0, xmax))
}