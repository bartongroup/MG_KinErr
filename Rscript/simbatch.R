# Stand-alone script to run a batch of simulations
# Usage:
#   Rscript Rscript/simbatch.R model par1 par2 par3 par4 nsim ncores outfile
# where parameters depend on the model:
# M1: formation_rate conversion_rate detachment_rate dummy
# M2: replacement_rate knockoff_rate conversion_rate dummy


source("R/setup.R")
source("R/lib.R")

simulationRuns <- function(model, par, nsim, ncores) {
  T <- mclapply(1:nsim, function(i) {
    sc <- simulate(model, par, verbose=FALSE)
    data.frame(time=sc$time, detached=sc$detached.duration)
  }, mc.cores = ncores)
  T <- do.call(rbind, T)
  df <- data.frame(
    model = model,
    time = round(T$time, 3),
    detached = round(T$detached, 3)
  )
  for(nm in names(par)) {
    df[[nm]] <- par[[nm]]$value$rate
  }
  df
} 

args <- commandArgs(TRUE)

model <- args[1]
par1 <- as.numeric(args[2])
par2 <- as.numeric(args[3])
par3 <- as.numeric(args[4])
par4 <- as.numeric(args[5])
nsim <- as.numeric(args[6])
ncores <- as.numeric(args[7])
outfile <- args[8]

if(model == "M1") {
  par <- parametersRates(formation=par1, conversion=par2, detachment=par3)
} else if(model == "M2") {
  par <- parametersRates(replacement=par1, knockoff=par2, conversion=par3)
} else {
  stop("Wrong model.")
}
 
df <- simulationRuns(model, par, nsim, ncores)

write.table(df, file=outfile, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
