SIDES <- c("L", "R")
SIDES <- setNames(SIDES, SIDES)
CONTACTS <- c("none", "lateral", "endon", "dual")
EVENTS <- c("formation", "conversion", "detachment", "replacement")
PARAMETERS <- c("formation", "conversion", "detachment", "delay")
MODELS <- c("independent", "release")


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
parametersRates <- function(formation, conversion, detachment, delay=1e16) {
  for(r in c(formation, conversion, detachment, delay)) {
    stopifnot(is.numeric(r) && r > 0)
  }
  p <- list()
  p <- setParameterRate(p, "formation", formation)
  p <- setParameterRate(p, "conversion", conversion)
  p <- setParameterRate(p, "detachment", detachment)
  p <- setParameterRate(p, "delay", delay)
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
    time = setNames(
      c(NA, NA, 0, NA), 
      c("formation", "conversion", "detachment", "replacement")
    )
  )
  class(obj) <- append(class(obj), "kinetochore")
  obj
}


# main object constructor
sisterChromatids <- function(par, time=0, model=c("independent", "release")) {
  model <- match.arg(model)
  stopifnot(is(par, "parameters"))
  KT <- lapply(SIDES, function(s) kinetochore(s))
  sc <- list(
    parameters = par,
    time = time,
    model = model,
    KT = KT,
    events = NULL,
    event.history = NULL,
    state.history = NULL
  )
  class(sc) <- append(class(sc), "sisterChromatids")
  sc
}


formLateralAttachment <- function(sc, side, spindle=NULL) {
  #stopifnot(side %in% SIDES)
  if(is.null(spindle)) spindle <- SIDES[rbinom(1, 1, 0.5) + 1] # MT from a random spindle
  KT <- sc$KT[[side]]
  #stopifnot(KT$contact %in% CONTACTS)
  
  # current contact
  if(KT$contact == "none") {
    KT$spindle <- spindle
    KT$contact <- "lateral"
    #KT$time[["formation"]] <- sc$time
    sc$KT[[side]] <- KT
  } else {
    stop(paste("Attempt to form lateral attachment on", KT$contact, "contact"))
  }
  sc
}


formDualAttachment <- function(sc, side, spindle=NULL) {
  #stopifnot(side %in% SIDES)
  if(is.null(spindle)) spindle <- SIDES[rbinom(1, 1, 0.5) + 1] # MT from a random spindle
  KT <- sc$KT[[side]]
  #stopifnot(KT$contact %in% CONTACTS)
  
  # current contact
  if(KT$contact == "endon") {
    KT$contact <- "dual"
    KT$dual.spindle <- KT$spindle
    KT$spindle <- spindle
    #KT$time[["replacement"]] <- sc$time
    sc$KT[[side]] <- KT
  } else {
    stop(paste("Attempt to form dual attachment on", KT$contact, "contact"))
  }
  sc
}

convertAttachment <- function(sc, side) {
  #stopifnot(side %in% SIDES)
  KT <- sc$KT[[side]]
  if(KT$contact == "lateral") {
    KT$contact <- "endon"
    #KT$time[["conversion"]] <- sc$time
    sc$KT[[side]] <- KT
  } else {
    stop(paste("Attempt to convert", KT$contact, "attachment on", side))
  }
  sc
}



detachKT <- function(sc, side) {
  #stopifnot(side %in% SIDES)
  KT <- sc$KT[[side]]
  
  # normal detachment
  if(KT$contact == "endon") {
    KT$contact <- "none"
    KT$spindle <- "none"
    #KT$time[["detachment"]] <- sc$time
    sc$KT[[side]] <- KT
  }
  
  # dual state: detachment is like lateral formation
  else if (KT$contact == "dual") {
    KT$contact <- "lateral"
    #KT$time[["formation"]] <- sc$time
    sc$KT[[side]] <- KT
  }

  else {
    stop(paste("Attempt to detach", KT$contact, "attachment on", side))
  }
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


# Generate next event for agiven KT. Returns a data frame with event type, its
# time and a few other things.
#
# This function has to be called upon execution of the previous event. It uses
# current time to generate the time of the next event.
generateEvent <- function(sc, side) {
  # extracting value from a list takes a while, so do it once
  KT <- sc$KT[[side]]
  contact <- KT$contact
  par <- sc$parameters
  model <- sc$model
  
  # no attachment: form lateral attachment
  if(contact == "none") {
    ev <- "formation"
    dt <- generateTime("formation", par)
  } 
  
  # lateral attachment: conversion
  else if (contact == "lateral") {
    ev <- "conversion"
    dt <- generateTime("conversion", par)
  }
  
  # end-on attachment: detach or replace
  else if (contact == "endon") {
    if(model == "independent") {
      ev <- "detachment"
      dt <- generateTime("detachment", par)
    } else if(model == "release") {
      ev <- "replacement"
      dt <- generateTime("formation", par)
    } else {
      stop("Unknown model")
    }
  }
    
  # dual attachment following replacement
  # remove end-on, detach end-on, leave lateral
  # this corresponds to formation of new lateral
  else if (contact == "dual") {
    ev <- "detachment"
    dt <- generateTime("delay", par)
  }
  
  data.frame(
    KT.side = side,
    spindle = KT$spindle,
    event = ev,
    time = sc$time + dt,
    stringsAsFactors = FALSE
  )  
}


initialEvents <- function(sc) {
  P <- lapply(SIDES, function(side) {
    d <- generateEvent(sc, side)
  })
  df <- do.call(rbind, P)
  df <- df[order(df$time), ]
  rownames(df) <- NULL
  sc$events <- df
  sc$state.history <- getStatus(sc, "data.frame")
  sc
}

executeEvent <- function(sc) {
  if(is.null(sc$events)) stop("No events to execute")
  
  # take one event from the top of the event list
  ev <- sc$events[1, ]
  sc$events <- sc$events[2:nrow(sc$events),]
  
  # extracting value from list takes a while, so do it once
  event <- ev$event
  KT.side <- ev$KT.side
  stopifnot(event %in% EVENTS)
  
  # set sc time to event time
  sc$time <- ev$time
  
  # store event in history stack
  sc$event.history <- rbind(sc$event.history, ev)
  rownames(sc$event.history) <- NULL
  
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
  
  # replacement: replace end-on with dual
  else if(event == "replacement") {
    sc <- formDualAttachment(sc, KT.side)
  }
  
  else {
    stop(paste("Unrecognized event", KT.event))
  }
  
  # update state history
  sc$state.history <- rbind(sc$state.history, getStatus(sc, "data.frame"))
  rownames(sc$state.history) <- NULL
  
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
  sc$KT$L$contact == "endon" &&
  sc$KT$R$contact == "endon" &&
  sc$KT$L$spindle == "L" &&
  sc$KT$R$spindle == "R"
}


simulate <- function(model, par, verbose=FALSE, max.iter=1000) {
  sc <- sisterChromatids(par, model=model)
  sc <- setErrorState(sc)
  sc <- initialEvents(sc)
  
  if(verbose) cat(getStatus(sc), "\n")
  for(i in 1:max.iter) {
    sc <- executeEvent(sc)
    sc <- nextEvent(sc)
    if(verbose) cat(getStatus(sc), "\n")
    if(finished(sc)) break()
  }
  sc
}

