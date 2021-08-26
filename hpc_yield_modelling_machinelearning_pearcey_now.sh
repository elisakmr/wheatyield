#!/bin/bash
#SBATCH --job-name="CVnow"
##SBATCH --nodes=1
##SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=1:0:00
#SBATCH --mem=8GB
#SBATCH -a 1
#SBATCH -o //OSM/CBR/AF_DIGI_RS/grains_shared/tmp_pearcy/cv_now_%a.txt

module load R/3.4.0
Rscript //OSM/CBR/AF_DIGI_RS/grains_shared/git/waldner/yield_machinelearning/yield_modelling_machinelearning_pearcey_now.R $SLURM_ARRAY_TASK_ID