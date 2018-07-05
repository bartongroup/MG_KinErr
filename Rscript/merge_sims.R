# merge simulation output files into one rds file
# Usage:
#    Rscript merge_sims.R <input_dir> <output_file>

args <- commandArgs(TRUE)
n <- length(args)
input.dir <- args[1]
output.file <- args[2]


pn <- list(
  M1 = c("formation", "conversion", "detachment"),
  M2 = c("replacement", "knockoff", "conversion")
)

plist <- function(model, par) {
  pnames <- pn[[model]]
  spar <- setNames(unlist(lapply(pnames, function(p) par[[p]]$value$rate)), pnames)
}

# read all files
files <- dir(input.dir, full.names = TRUE)
T <- list()
D <- list()
cnt <- 1
for(file in files) {
  x <- readRDS(file)
  pars <- plist(x$model, x$parameters)
  tim <- data.frame(model = x$model, time = x$result$time)
  tim <- cbind(tim, t(pars))
  T[cnt] <- tim
  det <- data.frame(model = x$model, detached = x$result$detached.distribution)
  det <- cbind(det, t(pars))
  D[cnt] <- det
  cnt <- cnt + 1
}

dat <- list(
  time = do.call(rbind, T),
  detached = do.call(rbind, D)
)


saveRDS(dat, file=output.file)