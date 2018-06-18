configfile: "config.yaml"

rscript = config['rscript']
nsim = config['nsim']
nbatch = config['nbatch']
defpar = config['defpar']


BATCHES = range(1, nbatch+1)

def_form = defpar[0]
def_conv = defpar[1]
def_det = defpar[2]
def_del = defpar[3]


rule all:
    input: 
      expand("simruns/sim_defpar_{batch}.txt", batch=BATCHES),



####################################################################

rule sim_defpar:
    output: "simruns/sim_defpar_{batch}.txt"
    threads: 8
    log: "logs/sim_defpar_{batch}.log"
    shell:
        "{rscript} Rscript/simbatch.R {def_form} {def_conv} {def_det} {def_del} {nsim} {threads} {output}  &> {log}" 



