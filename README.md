# Kinetochore-microtubule error correction

## Description

This repository contains the code and documentation for a simple kinetochore error correction model. The model generates of a sequence of states, representing different kinetochore/microtubule attachment configurations. Transition from one state to another is governed by the Poisson law. The final state is the correct bi-orientation. The details of the model are described in the main document (`doc/analysis.1.Rmd`).

## Instructions

First, create and activate a conda environment:

```
conda create --name kinerr --file conda-spec.txt
source activate kinerr
```

Then, use snakemake to perform all the necessary computations on the cluster:

```
snakemake -c "qsub -V -cwd -o snakelog -e snakelog -pe smp {threads}" --jobs=100
```

Next, edit the `R/setup.R` file and modify the home project directory (in our case it is on a remote system). Finally, knit the main document. 

```
rmarkdown::render("doc/analysis.1.Rmd")
```

The final report with model description and results is in the file `doc/analysis.1.html`.