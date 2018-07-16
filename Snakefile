# Basic values

rscript = 'Rscript'
nsim = 10000
nbatch = 1
test_seed = 52346

# Ranges of values

MODELS = ["M1", "M2"]
TESTS = [1, 2, 3, 4]
BATCHES = range(1, nbatch+1)

FORMS = [0.125, 0.25, 0.5, 1, 2, 4]
CONVS = [0.125, 0.25, 0.5, 1, 1.5, 2, 4]
DETS = [0.0625, 0.125, 0.25, 0.5, 1, 2, 4]
REPS = [0.125, 0.25, 0.5, 1, 2, 4] # replacement is the same process as formation
KOS = [1e16, 0.5, 1, 2, 10]


#FORMS = [0.5, 1]
#CONVS = [0.5, 1]
#DETS = [0.25, 0.5]
#REPS = [0.5, 1] # replacement is the same process as formation
#KOS = [1e16, 10]


# sim files for all range parameters
def all_simfiles(wildcards):
  files1 = expand("simruns/sim_M1_F{form}_C{conv}_D{det}_{batch}.rds", form=FORMS, conv=CONVS, det=DETS, batch=BATCHES)
  files2 = expand("simruns/sim_M2_R{rep}_K{ko}_C{conv}_{batch}.rds", rep=REPS, ko=KOS, conv=CONVS, batch=BATCHES)
  return files1 + files2



###################################################################

rule all:
    input:
      expand("simruns/sim_M1_F{form}_C{conv}_D{det}_{batch}.rds", form=FORMS, conv=CONVS, det=DETS, batch=BATCHES),
      expand("simruns/sim_M2_R{rep}_K{ko}_C{conv}_{batch}.rds", rep=REPS, ko=KOS, conv=CONVS, batch=BATCHES),
      expand("data/sim_test{code}.rds", code=TESTS),
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

rule sim_m1:
    output: "simruns/sim_M1_F{form}_C{conv}_D{det}_{batch}.rds"
    threads: 12
    log: "logs/sim_M1_F{form}_C{conv}_D{det}_{batch}.log"
    params:
      batch = "{batch}",
      form = "{form}",
      conv = "{conv}",
      det = "{det}"
    shell:
        "{rscript} Rscript/simbatch.R {params.batch} M1 {params.form} {params.conv} {params.det} 0 {nsim} {threads} {output} &> {log}"

rule sim_m2:
    output: "simruns/sim_M2_R{rep}_K{ko}_C{conv}_{batch}.rds"
    threads: 12
    log: "logs/sim_M2_R{rep}_K{ko}_C{conv}_{batch}.log"
    params:
      batch = "{batch}",
      rep = "{rep}",
      ko = "{ko}",
      conv = "{conv}"
    shell:
        "{rscript} Rscript/simbatch.R {params.batch} M2 {params.rep} {params.ko} {params.conv} 0 {nsim} {threads} {output} &> {log}"


###################################################################

rule sim_allpar_merge:
    input: all_simfiles
    output: "data/sim_allpar.rds"
    log: "logs/merge_sims.log"
    shell:
        "{rscript} Rscript/merge_sims.R simruns {output} &> {log}"
