# Basic values

rscript = 'Rscript'
nsim = 1000
nbatch = 100
grid_nbatch = 10
test_seed = 83654

# Default model parameters

def_form = 2
def_conv = 0.5
def_det = 1
def_del = 1e16

# Ranges of values

TESTS = [1, 2, 3]
BATCHES = range(1, nbatch+1)
GBATCHES = range(1, grid_nbatch+1)

FORMS = [0.5, 1, 2, 4, 8]
CONVS = [0.125, 0.25, 0.5, 1, 2]
DETS = [0.25, 0.5, 1, 2, 4]


# sim files for default parameters
def def_simfiles(wildcards):
  files = expand("simruns/sim_defpar_{batch}.txt", batch=BATCHES)
  return files

# sim files for all range parameters
def all_simfiles(wildcards):
  files = expand("simruns/sim_F{form}_C{conv}_D{det}_{gbatch}.txt", form=FORMS, conv=CONVS, det=DETS, gbatch=GBATCHES)
  return files



###################################################################

rule all:
    input: 
      expand("simruns/sim_defpar_{batch}.txt", batch=BATCHES),
      expand("simruns/sim_F{form}_C{conv}_D{det}_{gbatch}.txt", form=FORMS, conv=CONVS, det=DETS, gbatch=GBATCHES),
      expand("data/sim_test{code}.rds", code=TESTS),
      "data/sim_defpar.rds",
      "data/sim_allpar.rds"

####################################################################

rule sim_test:
    output: "data/sim_test{code}.rds"
    threads: 16
    log: "logs/sim_test{code}.txt"
    params:
        code = "{code}"
    shell:
        "{rscript} Rscript/model_test.R {params.code} 10000 {threads} {test_seed} {output} &> log"

####################################################################

rule sim_defpar:
    output: "simruns/sim_defpar_{batch}.txt"
    threads: 8
    log: "logs/sim_defpar_{batch}.log"
    shell:
        "{rscript} Rscript/simbatch.R {def_form} {def_conv} {def_det} {def_del} {nsim} {threads} {output}  &> {log}" 

####################################################################

rule sim_allpar:
    output: "simruns/sim_F{form}_C{conv}_D{det}_{gbatch}.txt"
    threads: 8
    log: "logs/sim_F{form}_C{conv}_D{det}_{gbatch}.log"
    params:
      form = "{form}",
      conv = "{conv}",
      det = "{det}"
    shell:
        "{rscript} Rscript/simbatch.R {params.form} {params.conv} {params.det} {def_del} {nsim} {threads} {output}  &> {log}"

###################################################################

rule sim_defpar_merge:
    input: def_simfiles
    output: "data/sim_defpar.rds"
    shell:
        "{rscript} Rscript/merge_sims.R {input} {output}"

###################################################################

rule sim_allpar_merge:
    input: all_simfiles
    output: "data/sim_allpar.rds"
    shell:
        "{rscript} Rscript/merge_sims.R {input} {output}"
