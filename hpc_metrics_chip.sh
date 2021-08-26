#!/bin/bash
#SBATCH --job-name="metrics"
##SBATCH --nodes=1
##SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --time=9:00:00
#SBATCH --mem=16GB
#SBATCH -a 1
#SBATCH -o //OSM/CBR/AF_DIGI_RS/grains_shared/tmp_pearcy/metrics_%a.txt

module load R/3.4.0
Rscript //OSM/CBR/AF_DIGI_RS/grains_shared/git/waldner/yield_machinelearning/metrics_chip.R $SLURM_ARRAY_TASK_ID