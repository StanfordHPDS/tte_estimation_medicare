library(tidyverse)
library(here)
library(furrr)
library(uuid)
source(here("R/data_simulation/manuscript_simulation/simulation_data_helpers.R"))
source(here("R/data_simulation/manuscript_simulation/estimation_helpers.R"))

plan(multisession, workers = 8)

# This should be run from the command line as
# `Rscript simulation_estimates_for_manuscript.R [num_batches] [num_reps_per_batch]`,
# where [num_batches] and [num_reps_per_batch] are both integers.
main <- function(num_batches, num_reps_per_batch) {
  # Generate a unique ID for output files and folders
  curr_uuid <- UUIDgenerate()

  curr_out_dir <- paste0(
    "R/data_simulation/manuscript_simulation/data",
    "_", curr_uuid
  )

  generate_simulation_data(num_batches, num_reps_per_batch, curr_out_dir, curr_uuid)

  # Generate grid of all comparisons to make
  # Each comparison is defined per row
  comparison_grid <- get_comparison_grid(data_path = here(curr_out_dir))

  # Estimate RMTL for each comparison
  comparison_results <- comparison_grid |>
    mutate(
      h = hcc,
      b = batch,
      r = rep_in_batch,
      monitoring_period = monitoring_period,
      underreported_pct = underreported_pct,
      upcoded_pct = upcoded_pct,
      tm_path = tm_path,
      ma_path = ma_path,
      .keep = "none"
    ) |>
    future_pmap(run_single_comparison)

  # Aggregate results
  all_cif <- map_dfr(comparison_results, "cif_df")
  print("CIF monitoring periods:")
  print(table(all_cif$monitoring_period))

  all_rmtl_diff <- map_dfr(comparison_results, "rmtl_diff_df")
  print("RMTL monitoring periods:")
  print(table(all_rmtl_diff$monitoring_period))


  if (!dir.exists(here("R/data_simulation/manuscript_simulation/output_files"))) {
    dir.create(here("R/data_simulation/manuscript_simulation/output_files"))
  }

  write_csv(
    all_cif,
    paste0(here("R/data_simulation/manuscript_simulation/output_files/"), "cif_", curr_uuid, ".csv")
  )
  write_csv(
    all_rmtl_diff,
    paste0(here("R/data_simulation/manuscript_simulation/output_files/"), "rmtl_diff_", curr_uuid, ".csv")
  )

  # Remove files that are no longer needed
  unlink(here(curr_out_dir), recursive = TRUE)
}

args <- commandArgs(trailingOnly = TRUE)

num_batches_arg <- as.integer(args[1])
num_reps_arg <- as.integer(args[2])

main(num_batches_arg, num_reps_arg)
