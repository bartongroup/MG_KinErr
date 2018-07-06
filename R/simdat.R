models = c("M1", "M2")
M1.parnames <- c("formation", "conversion", "detachment")
M2.parnames <- c("replacement", "knockoff", "conversion")

# two sets of default parameters
defaultParameters <- function(set=1) {
  if(set == 1) {
    parametersRates(
      formation = 1,
      conversion = 2,
      detachment = 1,
      replacement = 1,
      knockoff = 10
    )
  } else if (set == 2) {
    parametersRates(
      formation = 0.5,
      conversion = 2,
      detachment = 1,
      replacement = 0.5,
      knockoff = 10
    )
  }
}

# returns a named vectors with default parameters
# for a given model and set
defPars <- function(model, set) {
  pars <- defaultParameters(set)
  if(model == "M1") {
    parnames = M1.parnames
  } else {
    parnames = M2.parnames
  }
  P <- lapply(parnames, function(pn) pars[[pn]]$value$rate)
  setNames(unlist(P), parnames)
}

# returns a table with default parameters
defParsTab <- function(model, set) {
  p <- defPars(model, set)
  data.frame(
    parameter = names(p),
    value = p
  )
}

defparStat <- function(deftime) {
  P <- lapply(1:2, function(set) {
    x <- deftime[deftime$set == set,]
    data.frame(
      set = set,
      mean = mean(x$time),
      median = median(x$time)
    )
  })
  df <- do.call(rbind, P)
}

# select default parameters from the sim table
# returns reduced sim table, with added columns "model" and "set"
defSelect <- function(sim, model) {
  P <- lapply(c(1,2), function(set) {
    dp <- defPars(model, set)
    for(p in names(dp)) {
      sim <- sim[sim[[p]] == dp[[p]],, drop=FALSE]
    }
    data.frame(model=model, set=set, sim)
  })
  do.call(rbind, P)
}


# for a given sim, create a table with all parameters used
getParameterGrid <- function(sim, parnames) {
  P <- setNames(lapply(parnames, function(p) {
    paste(sort(unique(sim[, p])), collapse=", ")
  }), paste(parnames, "rate"))
  df <- do.call(rbind, P)
}
