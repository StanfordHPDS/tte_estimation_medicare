library(stats)
library(tidyverse)
library(khsmisc)

#' Create a grid of MA vs TM comparisons to make within each unique
#' HCC, batch, replicate, upcoding proportion, undercoding proportion based on
#' specified comparisons and created files.
#'
#' Each row contains information about the comparison and the path of one TM and one MA
#' file to compare. This is more efficient because only files needed are read in each comparison.
#'
#' @param data_path (string, default here("R/data_simulation/manuscript_simulation/data/")) where
#' to read data files from
#' @param pattern (regex string, default "^hcc\\d+_*_.*_event_and_time_labels.csv$") Event files too use
#'
#' @return a tibble of all comparisons to make
get_comparison_grid <- function(data_path,
                                pattern = "^hcc\\d+_*_.*_event_and_time_labels.csv$") {
  # Groups to compare
  ## Reference are baseline or undercoded groups
  ## Number refers to degree of undercoding
  tm_levels <- tibble(
    grp = rep("tm", 4),
    underreported_pct = c("0pt0", "0pt05", "0pt1", "0pt15")
  )
  ma_levels <- tibble(
    grp = rep("ma", 3),
    upcoded_pct = c("0pt2", "0pt25", "0pt3")
  )

  # Read in all file names
  all_event_files <- list.files(
    path = data_path,
    pattern = pattern,
    full.names = TRUE,
    recursive = TRUE
  )

  # Get file paths for all combinations of batch/replicates/HCC
  file_index_long <- tibble(p = all_event_files) |>
    rowwise() |>
    mutate(
      path = p,
      hcc = strsplit(basename(p), "_")[[1]][1],
      grp = strsplit(basename(p), "_")[[1]][2],
      monitoring_period = strsplit(basename(dirname(p)), "_")[[1]][3],
      batch = strsplit(basename(p), "_")[[1]][4],
      rep_in_batch = strsplit(basename(p), "_")[[1]][5],
      upcoded_pct = strsplit(basename(p), "_")[[1]][9],
      underreported_pct = strsplit(basename(p), "_")[[1]][7]
    )

  # Get relevant file paths for specified comparisons
  tm_index <- file_index_long |>
    semi_join(tm_levels,
      by = c("grp", "underreported_pct")
    ) |>
    filter(grp == "tm") |>
    select(hcc, batch, rep_in_batch, monitoring_period,
      underreported_pct,
      tm_path = p
    )
  ma_index <- file_index_long |>
    semi_join(ma_levels,
      by = c("grp", "upcoded_pct")
    ) |>
    filter(grp == "ma") |>
    select(hcc, batch, rep_in_batch, monitoring_period,
      upcoded_pct,
      ma_path = p
    )

  # Define all comparisons
  # Each unique combination per row to then iterate over (so a many-to-many relationship)
  comparison_grid <- tm_index |>
    inner_join(ma_index, relationship = "many-to-many")
}

#' Estimates RMTL within a monitoring period for a single HCC, batch, replicate, and comparison
#'
#' Note. Calls oena fork of khsmisc package for RMTL estimation, and saves relevant output
#'
#' @param h (string) The HCC
#' @param b (int) The batch number
#' @param r (int) The replicate number within the batch
#' @param monitoring_period (string) The monitoring period ID (i.e. "m1", "m2", or "m3")
#' @param underreported_pct (string) Degree of TM underreporting
#' @param upcoded_pct (string) Degree of MA upcoding
#' @param tm_path (string) Full path to TM file for this comparison
#' @param ma_path (string) Full path to MA file for this comparison
run_single_comparison <- function(h,
                                  b,
                                  r,
                                  monitoring_period,
                                  underreported_pct,
                                  upcoded_pct,
                                  tm_path,
                                  ma_path) {
  # comparison name for labeling
  curr_comparison <- paste0("tm_", underreported_pct, "_vs_ma_", upcoded_pct)

  # Read in TM data for this comparison
  tm_df <- read_csv(tm_path, show_col_types = FALSE) |>
    mutate(grp = "1_tm") # needs this because estimate_rmtl sorts exposures in alphanumeric order

  # Read in MA data for this comparison
  ma_df <- read_csv(ma_path, show_col_types = FALSE) |>
    mutate(grp = "2_ma") # needs this because estimate_rmtl sorts exposures in alphanumeric order

  # Combine dfs
  # Then adjust event_time by monitoring period
  combined_df <- bind_rows(tm_df, ma_df)

  # Get RMTL estimates
  rmtl_estimates_list <- estimate_rmtl(combined_df,
    exposure = grp,
    time = event_time,
    event = event_type
  )

  # Annotate output files
  curr_cif_df <- rmtl_estimates_list[["cif"]] |>
    mutate(
      hcc = h,
      batch = b,
      rep_in_batch = r,
      monitoring_period = monitoring_period,
      comparison = curr_comparison
    )

  curr_rmtl_diff_df <- rmtl_estimates_list[["rmtdiff"]] |>
    mutate(
      hcc = h,
      batch = b,
      rep_in_batch = r,
      monitoring_period = monitoring_period,
      comparison = curr_comparison
    )

  # Output
  list(
    cif_df = curr_cif_df,
    rmtl_diff_df = curr_rmtl_diff_df
  )
}
