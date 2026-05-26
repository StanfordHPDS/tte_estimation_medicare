#!/bin/bash
#SBATCH --job-name=sim_runs
#SBATCH --output=sim_%A_%a.out
#SBATCH --error=sim_%A_%a.err
#SBATCH --array=1-50 # number of jobs in parallel
#SBATCH --time=6:00:00
#SBATCH --cpus-per-task=8
#SBATCH --mem=60G # memory per job

SIM_IMAGE="$GROUP_HOME/containers/upcoding-sim_4.5.3.sif"

echo "Starting run ${SLURM_ARRAY_TASK_ID}"

apptainer exec "${SIM_IMAGE}" \
    Rscript simulation_estimates_for_manuscript.R 2 10 "${SLURM_ARRAY_TASK_ID}"
