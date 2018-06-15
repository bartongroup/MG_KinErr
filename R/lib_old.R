
# kinetochore object constructor
kinetochore <- function(side=c("L", "R"), MT=NULL, contact=NULL) {
  side <- match.arg(side)
  if(!is.null(MT)) stopifnot(MT%%1 == 0)
  if(!is.null(contact)) stopifnot(contact %in% c("endon", "lateral"))
  obj <- list(
    side = side,
    MT = MT,
    contact = contact
  )
  class(obj) <- append(class(obj), "kinetochore")
  obj
}

# microtubule object constructor
microtubule <- function(id, SP=c("L", "R"), KT=NULL, contact=NULL) {
  stopifnot(id%%1 == 0)
  SP <- match.arg(SP)
  if(!is.null(KT)) stopifnot(KT %in% c("L", "R"))
  if(!is.null(contact)) stopifnot(contact %in% c("endon", "lateral"))
  obj <- list(
    id = as.integer(id),
    SP = SP,
    KT = KT,
    contact = contact
  )
  class(obj) <- append(class(obj), "microtubule")
  obj
}

# spindle object constructor
spindle <- function(side=c("L", "R"), MTs=NULL) {
  side <- match.arg(side)
  obj <- list(
    side = side,
    MTs = MTs
  )
  class(obj) <- append(class(obj), "spindle")
  obj
}


# sisterChromatids object constructor
sisterChromatids_ <- function() {
  KTs <- list(
    "L" = kinetochore("L", 1, "endon"),
    "R" = kinetochore("R", 2, "endon")
  )
  MTs <- list(
    "1" = microtubule(1, "L", "L", "endon"),
    "2" = microtubule(2, "L", "R", "endon")
  )
  spindles <- list(
    "L" = spindle("L", c(1, 2)),
    "R" = spindle("R", NULL)
  )
  obj <- list(
    KTs = KTs,
    MTs = MTs,
    spindles = spindles
  )
  class(obj) <- append(class(obj), "sisterChromatids")
  obj
}


sisterChromatids <- function() {
  sides <- c("L", "R")
  sides <- setNames(sides, sides)
  KTs <- lapply(sides, kinetochore)
  MTs <- list()
  SPs <- lapply(sides, spindle)
  obj <- list(
    KTs = KTs,
    MTs = MTs,
    SPs = SPs
  )
  class(obj) <- append(class(obj), "sisterChromatids")
  obj
}


stateKT <- function(sc) {
  state <- unlist(lapply(c("L", "R"), function(side) {
    kt <- sc$KTs[[side]]
    if(!is.null(kt$MT)) {
      mt <- sc$MTs[[kt$MT]]
      paste0(mt$spindle, "-", kt$contact)
    } else {
      "empty"
    }
  }))
}

createMT <- function(sc, side=c("L", "R")) {
  side <- match.arg(side)
  MT.id <- length(sc$MTs) + 1
  MT <- microtubule(MT.id, side)
  sc$MTs[[MT.id]] <- MT
  sc$SPs[[side]]$MTs <- sort(c(sc$SPs[[side]]$MTs, MT.id))
  sc
}

attachKT <- function(sc, MT.id, KT.side=c("L", "R"), contact=c("endon", "lateral")) {
  contact <- match.arg(contact)
  KT.side <- match.arg(KT.side)
  stopifnot(is(sc, "sisterChromatids"))
  stopifnot(MT.id%%1 == 0)
  stopifnot(MT.id <= length(sc$MTs))

  if(is.null(sc$KTs[[KT.side]]$MT)) {
    sc$MTs[[MT.id]] <- microtubule(MT.id, sc$MTs[[MT.id]]$SP, KT.side, contact)
    sc$KTs[[KT.side]] <- kinetochore(KT.side, MT.id, contact)
  } else {
    stop(paste("Error: attempted attachment to non-empty KT", KT.side))
  }
  sc
}