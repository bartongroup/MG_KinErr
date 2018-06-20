# testing model

source("R/setup.R")
source("R/lib.R")
source("R/tests.R")

args <- commandArgs(TRUE)
code <- as.numeric(args[1])
nsim <- as.numeric(args[2])
ncores <- as.numeric(args[3])
outfile <- args[4]

if(code == 1) {
  par <- parametersRates(2, 0.5, 1, 1e16)
  simtest <- simulateDuration("independent", par, nsim=nsim, ncores=ncores)
} else if(code == 2) {
  par <- parametersRates(2, 0.5, 1, 1e16)
  simtest <- simulateDuration("release", par, nsim=nsim, ncores=ncores)
} else if(code == 3) {
  par <- parametersRates(2, 0.5, 1, 1)
  simtest <- simulateDuration("release", par, nsim=nsim, ncores=ncores)
}

saveRDS(simtest, file=outfile)