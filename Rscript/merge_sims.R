# merge simulation output files

args <- commandArgs(TRUE)
n <- length(args)
input.files <- args[1:(n-1)]
output.file <- args[n]

P <- lapply(input.files, function(file) {
  if(file.exists(file)) {
    return(read.delim(file, sep="\t", header=TRUE))
  }
})
P <- do.call(rbind, P)
rownames(P) <- NULL

saveRDS(P, file=output.file)