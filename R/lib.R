SIDES <- c("L", "R")
SIDES <- setNames(SIDES, SIDES)
CONTACTS <- c("none", "lateral", "endon", "dual")
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
parametersRates <- function(formation=0.5, conversion=2, detachment=1, replacement=0.5, knockoff=1e16) {
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
kinetochore <- function(side) {
  stopifnot(side %in% SIDES)
  obj <- list(
    side = side,
    spindle = "none",
    dual.spindle = "none",
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
  KT <- lapply(SIDES, function(s) kinetochore(s))
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
  p <- unlist(lapply(SIDES, function(side) {
    KT <- sc$KT[[side]]
    if(KT$contact == "none") {
      "---"
    } else {
      paste0(KT$contact, "-", KT$spindle)
    }
  }))
  if(return == "string") {
    sprintf("%5.2f  %11s %11s", sc$time, p[1], p[2])
  } else {
    data.frame(time=sc$time, left=p[1], right=p[2])
  }
}





formLateralAttachment <- function(sc, side, spindle=NULL) {
  if(is.null(spindle)) spindle <- SIDES[rbinom(1, 1, 0.5) + 1] # MT from a random spindle
  KT <- sc$KT[[side]]
  
  if(KT$contact == "none") {
    KT$spindle <- spindle
    KT$contact <- "lateral"
  } else {
    stop(paste("Attempt to form lateral attachment on", KT$contact, "contact"))
  }
  
  sc$KT[[side]] <- KT
  sc
}


formDualAttachment <- function(sc, side, spindle=NULL) {
  if(is.null(spindle)) spindle <- SIDES[rbinom(1, 1, 0.5) + 1] # MT from a random spindle
  KT <- sc$KT[[side]]
  
  if(KT$contact == "endon") {
    KT$contact <- "dual"
    KT$dual.spindle <- KT$spindle
    KT$spindle <- spindle
  } else {
    stop(paste("Attempt to form dual attachment on", KT$contact, "contact"))
  }
  
  sc$KT[[side]] <- KT
  sc
}

convertAttachment <- function(sc, side) {
  KT <- sc$KT[[side]]
  
  if(KT$contact == "lateral") {
    KT$contact <- "endon"
  } else {
    stop(paste("Attempt to convert", KT$contact, "attachment on", side))
  }
  
  sc$KT[[side]] <- KT
  sc
}


detachKT <- function(sc, side) {
  KT <- sc$KT[[side]]
  
  if(KT$contact == "endon") {
    KT$contact <- "none"
    KT$spindle <- "none"
  } else {
    stop(paste("Attempt to detach", KT$contact, "attachment on", side))
  }

  sc$KT[[side]] <- KT
  sc
}


knockoffKT <- function(sc, side) {
  KT <- sc$KT[[side]]
  
  if(KT$contact == "dual") {
    KT$contact <- "lateral"
  } else {
    stop(paste("Attempt to knockoff", KT$contact, "attachment on", side))
  }

  sc$KT[[side]] <- KT
  sc
}


# Create the initial state (which is error state) where two microtubules from
# the left spindle extend to both kinetochores and form end-on attachment
setErrorState <- function(sc) {
  stopifnot(is(sc, "sisterChromatids"))
  stopifnot(sc$KT$L$contact == "none" && sc$KT$R$contact == "none")
  sc <- formLateralAttachment(sc, "L", "L")
  sc <- formLateralAttachment(sc, "R", "L")
  sc <- convertAttachment(sc, "L")
  sc <- convertAttachment(sc, "R")
  sc
}

# Create two initial events to start simulation from the error state
initialEvents <- function(sc) {
  P <- lapply(SIDES, function(side) {
    d <- generateEvent(sc, side)
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


# Generate next event for agiven KT, based on the current state. Returns a data
# frame with event type, its time and a few other things.
#
# This function has to be called upon execution of the previous event. It uses
# current time to generate the time of the next event.
generateEvent <- function(sc, side) {
  # extracting value from a list takes a while, so do it once
  KT <- sc$KT[[side]]
  contact <- KT$contact
  model <- sc$model
  
  # no attachment: form lateral attachment
  if(contact == "none") {
    event <- "formation"
  } 
  
  # lateral attachment: conversion
  else if (contact == "lateral") {
    event <- "conversion"
  }
  
  # dual attachment: knockoff
  else if (contact == "dual") {
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
    KT.side = side,
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
    KT.side = KT$side,
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
  KT.side <- ev$KT.side
  #stopifnot(event %in% EVENTS)
  
  # store state info
  si <- stateInfo(sc$KT[[KT.side]], ev$time)
  sc$state.history <- rbind(sc$state.history, si)
  
  # set sc time to event time
  sc$time <- ev$time
  sc$KT[[KT.side]]$event.time <- ev$time
  
  # store event in history stack
  sc$event.history <- rbind(sc$event.history, ev)
  
  # formation: form new lateral attachment
  if(event == "formation") {
    sc <- formLateralAttachment(sc, KT.side)
  }
  
  # convertion: convert lateral to end-on
  else if(event == "conversion") {
    sc <- convertAttachment(sc, KT.side)
  }
  
  # detachment: detach end-on attachment
  else if(event == "detachment") {
    sc <- detachKT(sc, KT.side)
  }
  
  # knockoff: detach end-on attachment by lateral
  else if(event == "knockoff") {
    sc <- knockoffKT(sc, KT.side)
  }

  # replacement: replace end-on with dual
  else if(event == "replacement") {
    sc <- formDualAttachment(sc, KT.side)
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
    side <- sc$events[1, "KT.side"]
    other.side <- ifelse(side == "L", "R", "L")
    df <- generateEvent(sc, other.side)
    ev <- rbind(sc$events, df)
    ev <- ev[order(ev$time), ]
    sc$events <- ev
  } else {
    stop(paste("Attempt to add a new event when there are", nrow(sc$events), "events (should be 1)."))
  }
  sc
}

finished <- function(sc) {
  sc$KT$L$contact == "endon" && sc$KT$R$contact == "endon" &&
  ((sc$KT$L$spindle == "L" && sc$KT$R$spindle == "R") ||
    (sc$KT$L$spindle == "R" && sc$KT$R$spindle == "L"))
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
  
  # close state history
  for(KT.side in SIDES) {
    si <- stateInfo(sc$KT[[KT.side]], sc$time)
    sc$state.history <- rbind(sc$state.history, si)
  }
  # cleanup ugly mess
  rownames(sc$event.history) <- NULL
  rownames(sc$state.history) <- NULL
  rownames(sc$timeline) <- NULL
  sc
}

