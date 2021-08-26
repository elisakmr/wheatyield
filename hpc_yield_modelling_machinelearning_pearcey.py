import subprocess, os

content = """#!/bin/bash
#
#SBATCH --job-name={0}
#SBATCH --output=/home/wal716/output_txt/cv_{0}.txt
#SBATCH --mem=8GB
#SBATCH --ntasks-per-node={2}
#SBATCH --time=120:00:00
module load R/3.4.0
Rscript /OSM/CBR/AF_DIGI_RS/grains_shared/git/waldner/yield_machinelearning/yield_modelling_machinelearning_pearcey.R {1}
"""

for i in range(0,100):
	in_0_job="swin_"+str(i)
	in_1_zone=str(i)
	in_2_ncpus=str(1)
	filec = content.format(in_0_job,in_1_zone,in_2_ncpus)
	with open("/home/wal716/scripts/" + in_0_job + ".sh", 'w') as f:
	    f.write(filec)
	subprocess.call(["sbatch", "/home/wal716/scripts/" + in_0_job + ".sh" ])

