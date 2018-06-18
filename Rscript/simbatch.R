# Stand-alone script to run a batch of simulations
# Usage:
#   Rscript Rscript/simbatch.R form_rate conv_rate det_rate delay nsim ncores outfile

source("R/setup.R")
source("R/lib.R")

simulationRuns <- function(model, par, nsim, ncores) {
  T <- mclapply(1:nsim, function(i) {
    sc <- simulate(model, par, verbose=FALSE)
    sc$time
  }, mc.cores = ncores)
  df <- data.frame(
    model = model,
    time = round(unlist(T), 3)
  )
  for(nm in names(par)) {
    df[[nm]] <- par[[nm]]$value$rate
  }
  df
} 

args <- commandArgs(TRUE)

form_rate <- as.numeric(args[1])
conv_rate <- as.numeric(args[2])
det_rate <- as.numeric(args[3])
del_rate <- as.numeric(args[4])
nsim <- as.numeric(args[5])
ncores <- as.numeric(args[6])
outfile <- args[7]

par <- parametersRates(form_rate, conv_rate, det_rate, del_rate)

P <- lapply(MODELS, function(model) {
  simulationRuns(model, par, nsim, ncores)
})
df <- do.call(rbind, P)

write.table(df, file=outfile, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)