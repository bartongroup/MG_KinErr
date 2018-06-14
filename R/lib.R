
# kinetochore object constructor
kinetochore <- function(side=c("L", "R"), MT=NULL, contact=NULL) {
  side <- match.arg(side)
  if(!is.null(MT)) stopifnot(MT%%1 == 0)
  if(!is.null(contact)) stopifnot(contact %in% c("endon", "lateral"))
  obj <- list(
    side = side,
    MT = as.integer(MT),
    contact = contact
  )
  class(obj) <- append(class(obj), "kinetochore")
  obj
}

# microtubule object constructor
microtubule <- function(id, spindle=c("L", "R"), KT=NULL, contact=NULL) {
  stopifnot(id%%1 == 0)
  spindle <- match.arg(spindle)
  if(!is.null(KT)) stopifnot(KT %in% c("L", "R"))
  if(!is.null(contact)) stopifnot(contact %in% c("endon", "lateral"))
  obj <- list(
    id = as.integer(id),
    spindle = spindle,
    KT = KT,
    contact = contact
  )
  class(obj) <- append(class(obj), "microtubule")
  obj
}

# spindle object constructor
spindle <- function(side=c("L", "R"), MTs) {
  side <- match.arg(side)
  obj <- list(
    side = side,
    MTs = MTs
  )
  class(obj) <- append(class(obj), "spindle")
  obj
}


# sisterChromatids object constructor
sisterChromatids <- function() {
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


KT_state <- function(sc) {
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


attach_KT <- function(sc, MT.id, KT.side=c("L", "R"), contact=c("endon", "lateral")) {
  contact <- match.arg(contact)
  KT.side <- match.arg(KT.side)
  stopifnot(is(sc, "sisterChromatids"))
  stopifnot(MT.id%%1 == 0)

  if(is.null(sc$KTs[[KT.side]]$MT)) {
    sc$MTs[[MT.id]] <- microtubule(MT.id, sc$MTs[[MT.id]]$spindle, KT.side, contact)
    sc$KTs[[KT.side]] <- kinetochore(KT.side, MT.id, contact)
  } else {
    stop(paste("Error: attempted attachment to non-empty KT", KT.side))
  }
  sc
}