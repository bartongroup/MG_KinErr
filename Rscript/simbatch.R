# Stand-alone script to run a batch of simulations
# Usage:
#   Rscript Rscript/simbatch.R batch model par1 par2 par3 par4 nsim ncores outfile
# where parameters depend on the model:
# M1: formation_rate conversion_rate detachment_rate dummy
# M2: replacement_rate knockoff_rate conversion_rate dummy


source("R/setup.R")
source("R/lib.R")

simulationRuns <- function(model, par, nsim, ncores) {
  T <- mclapply(1:nsim, function(i) {
    sc <- simulate(model, par, verbose=FALSE)
    list(time=sc$time, det=sc$detached.distribution)
  }, mc.cores = ncores)
  time <- unlist(lapply(T, function(x) x$time))
  det <- unlist(lapply(T, function(x) x$det))
  list(
    time = time,
    detached.distribution = det
  )
} 

args <- commandArgs(TRUE)

batch <- as.integer(args[1])
model <- args[2]
par1 <- as.numeric(args[3])
par2 <- as.numeric(args[4])
par3 <- as.numeric(args[5])
par4 <- as.numeric(args[6])
nsim <- as.numeric(args[7])
ncores <- as.numeric(args[8])
outfile <- args[9]

if(model == "M1") {
  par <- parametersRates(formation=par1, conversion=par2, detachment=par3)
} else if(model == "M2") {
  par <- parametersRates(replacement=par1, knockoff=par2, conversion=par3)
} else {
  stop("Wrong model.")
}
 
res <- simulationRuns(model, par, nsim, ncores)
obj <- list(
  batch = batch,
  model = model,
  parameters = par,
  result = res
)

saveRDS(obj, file=outfile)
