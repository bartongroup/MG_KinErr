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
  T[[x$model]][[cnt]] <- cbind(batch=x$batch, x$result$time, t(pars))
  D[[x$model]][[cnt]] <- cbind(batch=x$batch, x$result$detached.distribution, t(pars))
  cnt <- cnt + 1
}

models <- names(T)
dat <- lapply(models, function(model) {
  list(
    time = do.call(rbind, T[[model]]),
    detached = do.call(rbind, D[[model]])
  )
})
names(dat) <- models

saveRDS(dat, file=output.file)