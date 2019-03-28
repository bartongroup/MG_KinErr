# testing model

#source("R/setup.R")
source("R/lib.R")
source("R/tests.R")
library(methods)
library(parallel)

args <- commandArgs(TRUE)
code <- as.numeric(args[1])
nsim <- as.numeric(args[2])
ncores <- as.numeric(args[3])
seed <- as.integer(args[4])
outfile <- args[5]


set.seed(seed)
if(code == 1) {
  simtest <- testGenerateFunction(nsim, ncores)
} else if(code == 2) {
  par <- parametersRates(formation=0.5, conversion=2, detachment=1)
  simtest <- simulateDuration("M1", par, nsim=nsim, ncores=ncores)
} else if(code == 3) {
  par <- parametersRates(replacement=0.5, conversion=2, knockoff=1e16)
  simtest <- simulateDuration("M2", par, nsim=nsim, ncores=ncores)
} else if(code == 4) {
  par <- parametersRates(replacement=0.5, conversion=2, knockoff=1)
  simtest <- simulateDuration("M2", par, nsim=nsim, ncores=ncores)
}

saveRDS(simtest, file=outfile)