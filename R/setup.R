library(ggplot2)
library(parallel)
library(methods)
library(data.table)

#qq.options(code.pattern = "\\$\\{CODE\\}")
N <- function(n) prettyNum(n, big.mark = ",")

# top directory for the project (multi-system)
# set this to your own
dirs <- c("/home/mgierlinski/projects/kinerr/", "/Volumes/mgierlinski/projects/kinerr/")
projectDir <- dirs[dir.exists(dirs)]
stopifnot(length(projectDir) == 1)

# Public HTML for file downloads
public_html <- "http://www.compbio.dundee.ac.uk/user/mgierlinski/kinerr/"

nbatch <- 100

