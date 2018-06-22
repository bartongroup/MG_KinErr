# Basic values

rscript = 'Rscript'
nsim = 1000
nbatch = 100
grid_nbatch = 10
test_seed = 52346

# Default model parameters

def_form = 0.5
def_conv = 2
def_det = 1
def_rep = 0.5
def_ko = 1e16

# Ranges of values

MODELS = ["M1", "M2"]
TESTS = [1, 2, 3]
BATCHES = range(1, nbatch+1)
GBATCHES = range(1, grid_nbatch+1)

FORMS = [0.125, 0.25, 0.5, 1, 2]
CONVS = [0.5, 1, 2, 4, 8]
DETS = [0.25, 0.5, 1, 2, 4]
REPS = [0.125, 0.25, 0.5, 1, 2] # replacement is the same process as formation
KOS = [1e16, 0.5, 1, 2]


# sim files for default parameters
def def_simfiles(wildcards):
  files = expand("simruns/sim_{model}_defpar_{batch}.txt", model=MODELS, batch=BATCHES)
  return files

# sim files for all range parameters
def all_simfiles(wildcards):
  files1 = expand("simruns/sim_M1_F{form}_C{conv}_D{det}_{gbatch}.txt", form=FORMS, conv=CONVS, det=DETS, gbatch=GBATCHES)
  files2 = expand("simruns/sim_M2_R{rep}_K{ko}_C{conv}_{gbatch}.txt", rep=REPS, ko=KOS, conv=CONVS, gbatch=GBATCHES)
  return files1 + files2



###################################################################

rule all:
    input: 
      expand("simruns/sim_M1_defpar_{batch}.txt", batch=BATCHES),
      expand("simruns/sim_M2_defpar_{batch}.txt", batch=BATCHES),
      expand("simruns/sim_M1_F{form}_C{conv}_D{det}_{gbatch}.txt", form=FORMS, conv=CONVS, det=DETS, gbatch=GBATCHES),
      expand("simruns/sim_M2_R{rep}_K{ko}_C{conv}_{gbatch}.txt", rep=REPS, ko=KOS, conv=CONVS, gbatch=GBATCHES),
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

rule sim_defpar_m1:
    output: "simruns/sim_M1_defpar_{batch}.txt"
    threads: 8
    log: "logs/sim_M1_defpar_{batch}.log"
    shell:
        "{rscript} Rscript/simbatch.R M1 {def_form} {def_conv} {def_det} 0 {nsim} {threads} {output}  &> {log}" 

rule sim_defpar_m2:
    output: "simruns/sim_M2_defpar_{batch}.txt"
    threads: 8
    log: "logs/sim_M2_defpar_{batch}.log"
    shell:
        "{rscript} Rscript/simbatch.R M2 {def_rep} {def_ko} {def_conv} 0 {nsim} {threads} {output}  &> {log}"        

####################################################################

rule sim_m1:
    output: "simruns/sim_M1_F{form}_C{conv}_D{det}_{gbatch}.txt"
    threads: 8
    log: "logs/sim_M1_F{form}_C{conv}_D{det}_{gbatch}.log"
    params:
      form = "{form}",
      conv = "{conv}",
      det = "{det}"
    shell:
        "{rscript} Rscript/simbatch.R M1 {params.form} {params.conv} {params.det} 0 {nsim} {threads} {output}  &> {log}"

rule sim_m2:
    output: "simruns/sim_M2_R{rep}_K{ko}_C{conv}_{gbatch}.txt"
    threads: 8
    log: "logs/sim_M2_R{rep}_K{ko}_C{conv}_{gbatch}.log"
    params:
      rep = "{rep}",
      ko = "{ko}",
      conv = "{conv}"
    shell:
        "{rscript} Rscript/simbatch.R M2 {params.rep} {params.ko} {params.conv} 0 {nsim} {threads} {output}  &> {log}"


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
