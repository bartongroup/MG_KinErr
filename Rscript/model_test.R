# testing model

source("R/setup.R")
source("R/lib.R")
source("R/tests.R")

args <- commandArgs(TRUE)
nsim <- as.numeric(args[1])
ncores <- as.numeric(args[2])
outfile <- args[3]

par <- parametersRates(2, 0.5, 1, 1e16)
simtest1 <- simulateDuration("independent", par, nsim=nsim, ncores=ncores)
simtest2 <- simulateDuration("release", par, nsim=nsim, ncores=ncores)
par2 <- parametersRates(2, 0.5, 1, 1)
simtest3 <- simulateDuration("release", par2, nsim=nsim, ncores=ncores)

save(simtest1, simtest2, simtest3, file=outfile)