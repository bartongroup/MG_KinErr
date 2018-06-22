# testing model

source("R/setup.R")
source("R/lib.R")
source("R/tests.R")

args <- commandArgs(TRUE)
code <- as.numeric(args[1])
nsim <- as.numeric(args[2])
ncores <- as.numeric(args[3])
seed <- as.integer(args[4])
outfile <- args[5]


set.seed(seed)
if(code == 1) {
  par <- parametersRates(formation=2, conversion=0.5, detachment=1)
  simtest <- simulateDuration("M1", par, nsim=nsim, ncores=ncores)
} else if(code == 2) {
  par <- parametersRates(replacement=2, conversion=0.5, knockoff=1e16)
  simtest <- simulateDuration("M2", par, nsim=nsim, ncores=ncores)
} else if(code == 3) {
  par <- parametersRates(replacement=2, conversion=0.5, knockoff=1)
  simtest <- simulateDuration("M2", par, nsim=nsim, ncores=ncores)
}

saveRDS(simtest, file=outfile)