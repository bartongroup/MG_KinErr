Sides <- c("L", "R")
Sides <- setNames(Sides, Sides)
Contacts <- c("none", "lateral", "endon", "dual")
Events <- c("formation", "conversion", "detachment", "replacement")

# parameters object constructor
parameters <- function(formation, conversion, detachment, delay=0) {
  p <- list(
    formation = formation,
    conversion = conversion,
    detachment = detachment,
    delay = delay
  )
  class(p) <- append(class(p), "parameters")
  p
}

# kinetochore object constructor
kinetochore <- function() {
  obj <- list(
    spindle = "none",
    contact = "none",
    time = setNames(
      c(NA, NA, 0), 
      c("formation", "conversion", "detachment")
    )
  )
  class(obj) <- append(class(obj), "kinetochore")
  obj
}


# main object constructor
sisterChromatids <- function(par, time=0, model="independent") {
  stopifnot(is(par, "parameters"))
  KT <- lapply(Sides, function(s) kinetochore())
  sc <- list(
    parameters = par,
    time = time,
    model = model,
    KT = KT,
    events = NULL,
    history = NULL
  )
  class(sc) <- append(class(sc), "sisterChromatids")
  sc
}


formLateralAttachment <- function(sc, side, spindle=NULL) {
  stopifnot(side %in% Sides)
  if(is.null(spindle)) spindle <- Sides[rbinom(1, 1, 0.5) + 1] # MT from a random spindle
  KT <- sc$KT[[side]]
  stopifnot(KT$contact %in% Contacts)
  
  if(KT$contact %in% c("none", "dual")) {
    new.contact <- "lateral"
  } else if(KT$contact == "endon") {
    new.contact <- "dual"
  } else {
    stop(paste("Attempt to form lateral attachment on", KT$contact, "contact"))
  }
  KT$spindle <- spindle
  KT$contact <- new.contact
  KT$time[["formation"]] <- sc$time
  sc$KT[[side]] <- KT
  sc
}



convertAttachment <- function(sc, side) {
  stopifnot(side %in% Sides)
  KT <- sc$KT[[side]]
  if(KT$contact == "lateral") {
    KT$contact <- "endon"
    KT$time[["conversion"]] <- sc$time
    sc$KT[[side]] <- KT
  } else {
    stop(paste("Attempt to convert", KT$contact, "attachment on", side))
  }
  sc
}



detachKT <- function(sc, side) {
  stopifnot(side %in% Sides)
  KT <- sc$KT[[side]]
  if(KT$contact %in% c("endon", "dual")) {
    KT$contact <- "none"
    KT$spindle <- "none"
    KT$time[["detachment"]] <- sc$time
    sc$KT[[side]] <- KT
  } else {
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



getStatus <- function(sc) {
  p <- unlist(lapply(Sides, function(side) {
    KT <- sc$KT[[side]]
    if(KT$contact == "none") {
      "---"
    } else {
      paste0(KT$contact, "-", KT$spindle)
    }
  }))
  sprintf("%5.2f  %11s %11s", sc$time, p[1], p[2])
}


# generate next event for agiven KT
eventTime <- function(KT, par, model) {
  stopifnot(KT$contact %in% Contacts)
  
  # no attachment: form lateral attachment
  if(KT$contact == "none") {
    ev <- "formation"
    t <- KT$time[["detachment"]] + rexp(1, par$formation)
  } 
  
  # lateral attachment: conversion
  else if (KT$contact == "lateral") {
    ev <- "conversion"
    t <- KT$time[["formation"]] + rexp(1, par$conversion)
  }
  
  # end-on attachment: detach or replace
  else if (KT$contact == "endon") {
    if(model == "independent") {
      ev <- "detachment"
      t <- KT$time[["conversion"]] + rexp(1, par$detachment)
    } else if(model == "replacement") {
      ev <- "replacement"
      t <- KT$time[["conversion"]] + rexp(1, par$formation)
    } else {
      stop("Unknown model")
    }
  }
    
  # dual attachemnt following replacement
  # remove end-on, leave lateral, so it is like formation 
  else if (KT$contact == "dual") {
    ev <- "formation"
    if(par$delay == 0) {
      t <- KT$time[["formation"]]
    } else {
      t <- KT$time[["formation"]] + rexp(1, par$delay)
    }
  }
  
  data.frame(
    event = ev,
    time = t
  )  
}


initialEvents <- function(sc) {
  P <- lapply(Sides, function(side) {
    d <- eventTime(sc$KT[[side]], sc$parameters, sc$model)
    d <- cbind(side=side, d)
  })
  df <- do.call(rbind, P)
  df <- df[order(df$time), ]
  sc$events <- df
  sc
}

executeEvent <- function(sc) {
  if(is.null(sc$events)) stop("No events to execute")
  ev <- sc$events[1, ]
  sc$history <- rbind(sc$history, ev)
  sc$events <- sc$events[2:nrow(sc$events),]
  sc$time <- ev$time
  
  # formation: form new lateral attachment
  if(ev$event == "formation") {
    sc <- formLateralAttachment(sc, ev$side)
  }
  
  # convertion: convert lateral to end-on
  else if(ev$event == "conversion") {
    sc <- convertAttachment(sc, ev$side)
  }
  
  # detachment: detach end-on attachment
  else if(ev$event == "detachment") {
    sc <- detachKT(sc, ev$side)
  }
  
  # replacement: replace end-on with dual
  else if(ev$event == "replacement") {
    sc <- formLateralAttachment(sc, ev$side)
  }
  
  else {
    stop(paste("Unrecognized event", ev$event))
  }
  sc
}


nextEvent <- function(sc) {
  # generate event for the other KT
  if(nrow(sc$events) == 1) {
    side <- sc$events[1, "side"]
    other.side <- ifelse(side == "L", "R", "L")
    df <- eventTime(sc$KT[[other.side]], sc$parameters, sc$model)
    df <- cbind(side=other.side, df)
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


simulate <- function(model = c("independent", "replacement"), verbose=TRUE) {
  model <- match.arg(model)
  par <- parameters(2, 0.5, 1)
  sc <- sisterChromatids(par)
  sc <- setErrorState(sc)
  sc <- initialEvents(sc)
  sc$model <- model
  
  if(verbose) cat(getStatus(sc), "\n")
  for(i in 1:100) {
    sc <- executeEvent(sc)
    sc <- nextEvent(sc)
    if(verbose) cat(getStatus(sc), "\n")
    if(finished(sc)) break()
  }
  sc$time
}


