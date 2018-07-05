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

files <- dir(input.dir)
P <- lapply(files, function(file) {
  x <- readRDS(file)
  pars <- plist(x$model, x$parameters)
  list(
    model = x$model,
    parameters = pars,
    batch = x$batch,
    time = x$result$time,
    detached = x$result$detached.distribution
  )
})

saveRDS(P, file=output.file)