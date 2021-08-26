#!/bin/bash
#SBATCH --job-name="TSSize"
##SBATCH --nodes=1
##SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=1:0:00
#SBATCH --mem=8GB
#SBATCH -a 1
#SBATCH -o //OSM/CBR/AF_DIGI_RS/grains_shared/tmp_pearcy/TSSize_%a.txt

module load R/3.4.0
Rscript //OSM/CBR/AF_DIGI_RS/grains_shared/git/waldner/yield_machinelearning/trainingset_size_yield_modelling_machinelearning_pearcey.R $SLURM_ARRAY_TASK_ID