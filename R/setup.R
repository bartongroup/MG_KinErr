library(ggplot2)
library(parallel)
library(methods)

#qq.options(code.pattern = "\\$\\{CODE\\}")
N <- function(n) prettyNum(n, big.mark = ",")

# top directory for the project
projectDir <- "/home/mgierlinski/projects/kinerr/"

# Public HTML for file downloads
public_html <- "http://www.compbio.dundee.ac.uk/user/mgierlinski/kinerr/"

nbatch <- 100

FORMS <- c(1, 2, 4)
CONVS <- c(0.2, 0.5, 1)
DETS <- c(0.5, 1, 2)
