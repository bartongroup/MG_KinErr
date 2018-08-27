library(ggplot2)
library(parallel)
library(methods)
library(data.table)

#qq.options(code.pattern = "\\$\\{CODE\\}")
N <- function(n) prettyNum(n, big.mark = ",")

# top directory for the project
projectDir <- "/home/mgierlinski/projects/kinerr/"

# Public HTML for file downloads
public_html <- "http://www.compbio.dundee.ac.uk/user/mgierlinski/kinerr/"

nbatch <- 100

