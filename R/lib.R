SIDES <- c("L", "R")
SIDES <- setNames(SIDES, SIDES)
KTS <- c("A", "B")
KTS <- setNames(KTS, KTS)
CONTACTS <- c("none", "lateral", "endon", "ambilink", "synlink", "biend")
EVENTS <- c("formation", "conversion", "detachment", "knockoff", "replacement")
PARAMETERS <- c("formation", "conversion", "detachment", "knockoff", "replacement")
MODELS <- c("M1", "M2")
DEBUG <- FALSE

setParameterRate <- function(par, process, rate) {
  stopifnot(process %in% PARAMETERS)
  par[[process]] <- list(
    type = "rate",
    value = list(
      rate = rate
    )
  )
  par
}

# parameters object constructor
parametersRates <- function(formation=2, conversion=2, detachment=1, replacement=2, knockoff=1e16) {
  for(r in c(formation, conversion, detachment, replacement, knockoff)) {
    stopifnot(is.numeric(r) && r > 0)
  }
  p <- list()
  p <- setParameterRate(p, "formation", formation)
  p <- setParameterRate(p, "conversion", conversion)
  p <- setParameterRate(p, "detachment", detachment)
  p <- setParameterRate(p, "knockoff", knockoff)
  p <- setParameterRate(p, "replacement", replacement)
  class(p) <- append(class(p), "parameters")
  p
}

# kinetochore object constructor
kinetochore <- function(id) {
  stopifnot(id %in% KTS)
  obj <- list(
    id = id,
    spindle = "none",
    link.spindle = "none",
    contact = "none",
    event.time = 0
  )
  class(obj) <- append(class(obj), "kinetochore")
  obj
}


# main object constructor
sisterChromatids <- function(par, time=0, model) {
  stopifnot(model %in% MODELS)
  stopifnot(is(par, "parameters"))
  KT <- lapply(KTS, function(id) kinetochore(id))
  sc <- list(
    parameters = par,
    time = time,
    model = model,
    KT = KT,
    events = NULL,
    event.history = NULL,
    state.history = NULL,
    timeline = NULL
  )
  class(sc) <- append(class(sc), "sisterChromatids")
  sc
}


# return current state of the system in a simple string or data frame
getStatus <- function(sc, return="string") {
  p <- unlist(lapply(KTS, function(id) {
    KT <- sc$KT[[id]]
    if(KT$contact == "none") {
      "---"
    } else {
      paste0(KT$contact, "-", KT$spindle)
    }
  }))
  if(return == "string") {
    sprintf("%5.2f  %11s %11s", sc$time, p[1], p[2])
  } else {
    data.frame(time=sc$time, A=p[1], B=p[2])
  }
}





formLateralAttachment <- function(sc, id, spindle=NULL) {
  if(is.null(spindle)) spindle <- SIDES[rbinom(1, 1, 0.5) + 1] # MT from a random spindle
  KT <- sc$KT[[id]]
  
  if(KT$contact == "none") {
    KT$spindle <- spindle
    KT$contact <- "lateral"
  } else {
    stop(paste("Attempt to form lateral attachment on", KT$contact, "contact"))
  }
  
  sc$KT[[id]] <- KT
  sc
}


formLinkAttachment <- function(sc, id, spindle=NULL) {
  if(is.null(spindle)) spindle <- SIDES[rbinom(1, 1, 0.5) + 1] # MT from a random spindle
  KT <- sc$KT[[id]]
  
  if(KT$contact == "endon") {
    if(spindle == KT$spindle) {
      KT$contact <- "synlink"
    } else {
      KT$contact <- "ambilink"
    }
    KT$link.spindle <- KT$spindle
    KT$spindle <- spindle
  } else {
    stop(paste("Attempt to form link attachment on", KT$contact, "contact"))
  }
  
  sc$KT[[id]] <- KT
  sc
}

convertAttachment <- function(sc, id) {
  KT <- sc$KT[[id]]
  
  if(KT$contact == "lateral") {
    KT$contact <- "endon"
  } else if(KT$contact == "synlink") {
    KT$contact <- "biend"
  } else {
    stop(paste("Attempt to convert", KT$contact, "attachment on", id))
  }
  
  sc$KT[[id]] <- KT
  sc
}


detachKT <- function(sc, id) {
  KT <- sc$KT[[id]]
  
  if(KT$contact == "endon") {
    KT$contact <- "none"
    KT$spindle <- "none"
  } else {
    stop(paste("Attempt to detach", KT$contact, "attachment on", id))
  }

  sc$KT[[id]] <- KT
  sc
}


knockoffKT <- function(sc, id) {
  KT <- sc$KT[[id]]
  
  if(KT$contact == "ambilink") {
    KT$contact <- "lateral"
  } else if (KT$contact == "biend") {
    KT$contact <- "endon"
  } else {
    stop(paste("Attempt to knockoff", KT$contact, "attachment on", id))
  }
  KT$link.spindle <- "none"
  
  sc$KT[[id]] <- KT
  sc
}


# Create the initial state (which is error state) where two microtubules from
# the left spindle extend to both kinetochores and form end-on attachment
setErrorState <- function(sc) {
  stopifnot(is(sc, "sisterChromatids"))
  stopifnot(sc$KT$A$contact == "none" && sc$KT$B$contact == "none")
  sc <- formLateralAttachment(sc, "A", "L")
  sc <- formLateralAttachment(sc, "B", "L")
  sc <- convertAttachment(sc, "A")
  sc <- convertAttachment(sc, "B")
  sc
}

# Create two initial events to start simulation from the error state
initialEvents <- function(sc) {
  P <- lapply(KTS, function(id) {
    d <- generateEvent(sc, id)
  })
  df <- do.call(rbind, P)
  df <- df[order(df$time), ]
  rownames(df) <- NULL
  sc$events <- df
  sc$timeline <- getStatus(sc, "data.frame")
  sc
}


# generate random event time from parameters
# these can be either Poisson rates, or Gaussian mean and sd
generateTime <- function(event, par) {
  stopifnot(event %in% names(par))
  p <- par[[event]]
  type <- p$type
  val <- p$value
  
  # Poisson model with exponential distirbution, there is only one parameter:
  # rate
  if(type == "rate") {
    time <- rexp(1, rate = val$rate)
  }
  
  # Gaussian model wiht mean and standard deviation
  else if (type == "gaussian") {
    time <- rnorm(1, mean = val$mean, sd = val$sd)
  }
  
  else {
    stop(paste("Unknown parameter type", type))
  }
  time
}

# for testing
generateTimeTables <- function(par, n=10000) {
  tab <- lapply(names(par), function(ev) {
    rexp(n, par[[ev]]$value$rate)
  })
  names(tab) <- names(par)
  tab
}

generateTime_ <- function(event, par) {
  t <- TIMES[[event]]
  sample(t, 1)
}


# Generate next event for agiven KT, based on the current state. Returns a data
# frame with event type, its time and a few other things.
#
# This function has to be called upon execution of the previous event. It uses
# current time to generate the time of the next event.
generateEvent <- function(sc, id) {
  # extracting value from a list takes a while, so do it once
  KT <- sc$KT[[id]]
  contact <- KT$contact
  model <- sc$model
  
  # no attachment: form lateral attachment
  if(contact == "none") {
    event <- "formation"
  } 
  
  # lateral attachment: conversion
  else if (contact %in% c("lateral", "synlink")) {
    event <- "conversion"
  }
  
  # dual attachment: knockoff
  else if (contact %in% c("ambilink", "biend")) {
    event <- "knockoff"
  }

  # end-on attachment: detach or replace
  else if (contact == "endon") {
    if(model == "M1") {
      event <- "detachment"
    } else if(model == "M2") {
      event <- "replacement"
    }
  }
  
  else {
    stop(paste("Unrecognized contact", contact, "in generateEvent."))
  }
  
  duration <- generateTime(event, sc$parameters)
  time <- sc$time + duration

  data.frame(
    KT.id = id,
    spindle = KT$spindle,
    event = event,
    time = time,
    duration = duration,
    stringsAsFactors = FALSE
  )  
}

# helper function
stateInfo <- function(KT, time) {
  data.frame(
    state = KT$contact,
    KT.id = KT$id,
    spindle = KT$spindle,
    start = KT$event.time,
    end = time
  )
}

# Execute event from top of the stack
executeEvent <- function(sc) {
  if(is.null(sc$events)) stop("No events to execute")
  
  # pop one event from the top of the event list
  ev <- sc$events[1, ]
  sc$events <- sc$events[2:nrow(sc$events),]
  
  # just to speed up the code a little bit
  event <- ev$event
  KT.id <- ev$KT.id
  #stopifnot(event %in% EVENTS)
  
  # store state info
  si <- stateInfo(sc$KT[[KT.id]], ev$time)
  sc$state.history <- rbind(sc$state.history, si)
  
  # set sc time to event time
  sc$time <- ev$time
  sc$KT[[KT.id]]$event.time <- ev$time
  
  # store event in history stack
  sc$event.history <- rbind(sc$event.history, ev)
  
  # formation: form new lateral attachment
  if(event == "formation") {
    sc <- formLateralAttachment(sc, KT.id)
  }
  
  # convertion: convert lateral to end-on
  else if(event == "conversion") {
    sc <- convertAttachment(sc, KT.id)
  }
  
  # detachment: detach end-on attachment
  else if(event == "detachment") {
    sc <- detachKT(sc, KT.id)
  }
  
  # knockoff: detach end-on attachment by lateral
  else if(event == "knockoff") {
    sc <- knockoffKT(sc, KT.id)
  }

  # replacement: replace end-on with dual
  else if(event == "replacement") {
    sc <- formLinkAttachment(sc, KT.id)
  }
  
  else {
    stop(paste("Unrecognized event", event, "in executeEvent."))
  }
  
  # update state history
  sc$timeline <- rbind(sc$timeline, getStatus(sc, "data.frame"))
  
  sc
}


# Create the next event in the event list. The list must contain only one event
# for a given KT, and this function will create and event for the other KT.
# Hence, at the end we will always have two events, one per KT.
nextEvent <- function(sc) {
  # generate event for the other KT
  if(nrow(sc$events) == 1) {
    KT.id <- sc$events[1, "KT.id"]
    other.KT <- ifelse(KT.id == "A", "B", "A")
    df <- generateEvent(sc, other.KT)
    ev <- rbind(sc$events, df)
    ev <- ev[order(ev$time), ]
    sc$events <- ev
  } else {
    stop(paste("Attempt to add a new event when there are", nrow(sc$events), "events (should be 1)."))
  }
  sc
}

finished <- function(sc) {
  sc$KT$A$contact == "endon" && sc$KT$B$contact == "endon" &&
  ((sc$KT$A$spindle == "L" && sc$KT$B$spindle == "R") ||
    (sc$KT$A$spindle == "R" && sc$KT$B$spindle == "L"))
}

sys.time.fs <- function() {
  options(digits.secs=6)
  s <- as.character(Sys.time())
  t <- unlist(strsplit(s, " "))[2]
  fs <- unlist(strsplit(t, ".", fixed=TRUE))[2]
  as.integer(fs)
}


# total duration of fully detached periods
# that is, when both KTs are detached
# This function used 'intervals' package
# I cannot install this package in conda environment
# due to conflicts.
detachedDurationIntervals <- function(sc) {
  if(sc$model != "M1") return(NA)
  sh <- sc$state.history
  sh <- sh[sh$state == "none", ]
  shA <- as.matrix(sh[sh$KT.id == "A" , c("start", "end")])
  shB <- as.matrix(sh[sh$KT.id == "B" , c("start", "end")])
  if(nrow(shA) == 0 | nrow(shB) == 0) return(0)
  shA <- Intervals(shA)
  shB <- Intervals(shB)
  int <- interval_intersection(shA, shB)
  sum(size(int))  # total size of intersection
}

# The same as above, using data.table 
detachedDistribution <- function(sc) {
  if(sc$model != "M1") return(NA)
  sh <- sc$state.history
  sh <- sh[sh$state == "none", ]
  shL <- sh[sh$KT.id == "L" , ]
  shL <- data.table(start=shL$start, end=shL$end)
  shR <- sh[sh$KT.id == "R" , ]
  shR <- data.table(start=shR$start, end=shR$end)
  if(nrow(shL) == 0 | nrow(shR) == 0) return(numeric(0)) # empty vector
  setkey(shR, start, end)
  # find overlaps
  ov <- foverlaps(shL, shR, type="any", nomatch=0)
  # find intersection of overlaps
  ov[, start := pmax(start, i.start)]
  ov[, end := pmin(end, i.end)]
  ov[, length := end - start]
  as.numeric(ov$length)  
}


simulate <- function(model, par, verbose=FALSE, max.iter=1000) {
  sc <- sisterChromatids(par, model=model)
  sc <- setErrorState(sc)
  sc <- initialEvents(sc)
  
  # main loop
  if(verbose) cat(getStatus(sc), "\n")
  for(i in 1:max.iter) {
    sc <- executeEvent(sc)
    sc <- nextEvent(sc)
    if(verbose) cat(getStatus(sc), "\n")
    if(finished(sc)) break()
  }
  
  # close state and event history
  for(KT.id in KTS) {
    si <- stateInfo(sc$KT[[KT.id]], sc$time)
    sc$state.history <- rbind(sc$state.history, si)
  }
  sc$event.history <- rbind(sc$event.history, sc$events)
  
  # cleanup ugly mess
  rownames(sc$event.history) <- NULL
  rownames(sc$state.history) <- NULL
  rownames(sc$timeline) <- NULL
  sc$detached.distribution <- detachedDistribution(sc)
  sc$detached.duration <- sum(sc$detached.distribution)
  
  return(sc)
}

