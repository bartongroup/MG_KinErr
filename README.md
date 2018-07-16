# Kinetochore-microtubule error correction

This repository contains code for a simple kinetochore error correction model. Before knitting the main document (doc/analysis.1.Rmd) cluster calculation need to be done. First, a conda environment needs to be created and activated:

```
conda create --name kinerr --file conda-spec.txt
source activate kinerr
```

Then, all the necessary computations are run on the cluster using snakamake file:

```
snakemake -c "qsub -V -cwd -o snakelog -e snakelog -pe smp {threads}" --jobs=100
```

Finally, the main document can be knitted.