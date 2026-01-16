library(upcoding)
library(tidyverse)
library(tools)
library(here)
library(data.table)

# Note. The main function here is generate_simulation_data; all other functions are helpers for this.

#' Generates a base seed for a simulation batch
#'
#' @param batch (int) batch number
#' @param init_seed (int) starting seed, for reproducibility across batches
#'
#' @return An integer base seed for specified batch
get_batch_seed <- function(batch, init_seed) {
  init_seed + (batch - 1) * 100000
}

## File prefix extractors

#' Extract prefix from baseline data that hasn't been upcoded
#'
#' @param f (string) a full file path
#'
#' @return the prefix from the basename of f
get_initial_prefix <- function(f) {
  tools::file_path_sans_ext(basename(f))
}

#' Extract prefix from baseline data that has been upcoded
#'
#' @param f (string) a full file path
#'
#' @return the prefix from the basename of f
get_upcoded_prefix <- function(f) {
  f <- str_remove(tools::file_path_sans_ext(basename(f)), "all_hcc_")
  f <- str_remove(tools::file_path_sans_ext(basename(f)), "_upcoded.*$")
}

#' Iteratively upcode a prior monitoring period over four time points.
#'
#' @param spec_df (dataframe, tibble) The upcoding spec df. See upcoding package for details
#' @param input_dir (string) The relative path from which to read files to upcode
#' @param out_dir (string) The relative path where upcoded files will be upcoded
#' @param pattern (string) The regex pattern to use to identify files to upcode within input_dir
#' @param starting_seed (int) The initial seed to use for upcoding for reproducibility
#' @param seed_change_interval (int) Specifies how many replicates to do before updating the seed.
#' The expectation here is that the same seed is used within a given batch and replicate
#' @param extract_prefix_fn (function) What function to use to extract the output file prefix. Expects
#' one of get_upcoded_prefix or get_initial_prefix; see these functions above.
#' @param censoring_prop (double) The proportion of rows to lose to follow up
#' @param upcoding_prop_annot (string, default=NULL) Additional string to add to output file name indicating the
#' degree of upcoding
upcode_prior_monitoring_period <- function(spec_df,
                                           input_dir,
                                           out_dir,
                                           pattern,
                                           starting_seed,
                                           seed_change_interval,
                                           extract_prefix_fn,
                                           censoring_prop,
                                           upcoding_prop_annot = NULL) {
  curr_files <- list.files(here(input_dir),
    pattern = pattern,
    full.names = TRUE
  )

  # Define seeds to use
  # We want to change these every four files, as each sequential four files correspond
  # to one simulation iteration
  seeds <- starting_seed + ((seq_along(curr_files) - 1L) %/% seed_change_interval)

  # Iterate over files and upcode
  walk2(curr_files, seeds, function(f, curr_seed) {
    curr_data <- fread(f)

    curr_out_file_prefix <- extract_prefix_fn(f)
    if (!is.null(upcoding_prop_annot)) {
      curr_out_file_prefix <- paste(curr_out_file_prefix,
        upcoding_prop_annot,
        sep = "_"
      )
    }

    upcode_all_hccs(
      curr_data,
      spec_df,
      out_dir         = out_dir,
      censoring_prop  = censoring_prop,
      num_timepoints  = 4,
      out_file_prefix = curr_out_file_prefix,
      curr_seed       = curr_seed
    )
  })
}

#' Upcode a prior monitoring period over four time points and varying upcoding proportions
#'
#' @param spec_df (dataframe, tibble) The upcoding spec df. See upcoding package for details
#' @param upcoding_props (vector of doubles) All proportions (between 0 and 1) to upcode
#' @param input_dir (string) The relative path from which to read files to upcode
#' @param out_dir (string) The relative path where upcoded files will be upcoded
#' @param pattern (string) The regex pattern to use to identify files to upcode within input_dir
#' @param starting_seed (int) The initial seed to use for upcoding for reproducibility
#' @param seed_change_interval (int) Specifies how many replicates to do before updating the seed.
#' The expectation here is that the same seed is used within a given batch and replicate
#' @param extract_prefix_fn (function) What function to use to extract the output file prefix. Expects
#' one of get_upcoded_prefix or get_initial_prefix; see these functions above.
#' @param censoring_prop (double) The proportion of rows to lose to follow up
upcode_monitoring_period_to_varying_degree <- function(spec_df,
                                                       upcoding_props,
                                                       input_dir,
                                                       out_dir,
                                                       pattern,
                                                       starting_seed,
                                                       seed_change_interval,
                                                       extract_prefix_fn,
                                                       censoring_prop) {
  walk(upcoding_props, function(p) {
    spec_df$upcoding_prop <- rep(p, 2)

    upcode_prior_monitoring_period(spec_df,
      input_dir,
      out_dir,
      pattern,
      starting_seed,
      seed_change_interval,
      extract_prefix_fn,
      censoring_prop,
      upcoding_prop_annot = paste("upcoded",
        gsub(".", "pt", as.character(p), fixed = T),
        sep = "_"
      )
    )
  })
}


#' Helper function to read and annotate event files
#'
#' This extracts information about the HCC, group, and underreporting or upcoding prop
#' from a file name of simulated data. These are then used in later simulation comparisons.
#'
#' @param f (string) The file name of the event file
#'
#' @return a tibble of the event file, with added columns for:
#'   - `hcc`: The HCC that was upcoded
#'   - `group`: The comparison group
#'   - `underreporting_prop`: The degree of underreporting
#'   - `upcoding_prop`: The degree of upcoding
#'   - `category`: The type of group (either 'reference' or 'upcoded')
read_and_annotate_event_file <- function(f) {
  curr_filename_components <- strsplit(strsplit(f, "/")[[1]][14], "_")[[1]]
  curr_monitoring_period <- strsplit(strsplit(f, "/")[[1]][13], "_")[[1]][3]
  curr_batch <- curr_filename_components[5]
  curr_rep <- curr_filename_components[6]

  if ("underreported" %in% curr_filename_components) { # TM data with underreporting
    curr_hcc <- curr_filename_components[1]
    curr_group <- curr_filename_components[2]
    curr_underreporting_prop <- as.numeric(gsub("pt", ".", curr_filename_components[8]))
    curr_upcoding_prop <- 0.05 # same for all of them
    curr_coding_category <- "reference"
  } else if ("upcoded" %in% curr_filename_components) { # upcoded MA data
    curr_hcc <- curr_filename_components[1]
    curr_group <- curr_filename_components[2]
    curr_underreporting_prop <- 0
    curr_upcoding_prop <- as.numeric(str_remove(curr_filename_components[8], "pct")) / 100
    curr_coding_category <- "upcoded"
  } else { # TM data without underreporting
    curr_hcc <- curr_filename_components[1]
    curr_group <- curr_filename_components[2]
    curr_underreporting_prop <- 0
    curr_upcoding_prop <- 0.05
    curr_coding_category <- "reference"
  }

  curr_df <- read_csv(f, show_col_types = FALSE) |>
    mutate(
      hcc = curr_hcc,
      group = curr_group,
      underreporting_prop = curr_underreporting_prop,
      upcoding_prop = curr_upcoding_prop,
      category = curr_coding_category,
      monitoring_period = curr_monitoring_period,
      batch = curr_batch,
      rep_in_batch = curr_rep
    )

  curr_df
}

count_all_hccs <- function(f) {
  hcc_total <- read_csv(f) |>
    select(-person_id) |>
    summarize(total = sum(across(everything()))) |>
    pull(total)

  hcc_total
}

#' Generate simulation data in a set number of batches and replicates per batch.
#'
#' This function (1) Generates baseline data for MA and TM separately; (2) Undercodes
#' TM data only to varying degrees specified by tm_undercoding_props; (3) Upcodes TM
#' data 5% per monitoring period sequentially over two monitoring periods;
#' (4) Upcodes two HCCs in MA data (HCC238 and HCC125) to varying degrees
#' as specified by ma_upcoding_props sequentially over two monitoring periods;
#' (5) Removes files that aren't needed for estimation.
#'
#' @param num_batches (int) The number of batches to iterate over
#' @param num_reps_per_batch (int) The number of simulation replicates in each batch
#' @param init_seed (int, default 1234) The starting seed to use per batch
#' @param num_rows (int, default 1,000,000) The number of rows to generate per file
#' @param tm_undercoding_props (vector of doubles, default c(0.05, 0.1, 0.15)) The degrees
#' of undercoding to implement for TM data only
#' @param ma_upcoding_props (vector of doubles, default c(0.2, 0.25, 0.3)) The degrees
#' of upcoding to implement for MA data only
generate_simulation_data <- function(num_batches,
                                     num_reps_per_batch,
                                     curr_out_dir,
                                     curr_uuid,
                                     init_seed = 1234,
                                     num_rows = 1000000,
                                     tm_undercoding_props = c(0.05, 0.1, 0.15),
                                     ma_upcoding_props = c(0.2, 0.25, 0.3)) {
  walk(seq_len(num_batches), function(b) {
    batch_seed <- get_batch_seed(b, init_seed)

    # Get seeds for various steps
    tm_seeds <- batch_seed + 1:num_reps_per_batch # baseline
    ma_seeds <- tm_seeds * 2 # baseline
    undercoding_seeds <- tm_seeds * 3
    tm_upcoding_starting_seed <- max(undercoding_seeds) + 1
    ma_upcoding_starting_seed <- max(tm_seeds * 200)

    # Simulate baseline TM data
    walk2(
      tm_seeds,
      seq_along(tm_seeds),
      ~ simulate_baseline_v28_hcc_dt(
        n = num_rows,
        out_dir = file.path(curr_out_dir, "baseline_data"),
        out_file_prefix = paste0("tm_data_batch", b, "_rep", .y, "_baseline_0pt0"),
        curr_seed = .x
      )
    )

    # Simulate baseline MA data
    walk2(
      ma_seeds,
      seq_along(ma_seeds),
      ~ simulate_baseline_v28_hcc_dt(
        n = num_rows,
        out_dir = file.path(curr_out_dir, "baseline_data"),
        out_file_prefix = paste0("ma_data_batch", b, "_rep", .y, "_baseline_0pt0"),
        curr_seed = .x
      )
    )

    # Undercode baseline TM data only
    baseline_data_dir <- here(file.path(curr_out_dir, "baseline_data"))
    tm_baseline_files <- list.files(
      baseline_data_dir,
      pattern = "^tm_data.*\\_baseline_0pt0.csv$",
      full.names = TRUE
    )

    ## Apply undercoding to each TM baseline file across proportions listed
    walk2(tm_baseline_files, undercoding_seeds, function(f, curr_seed) {
      curr_data <- fread(f)

      walk(tm_undercoding_props, function(prop) {
        undercode_dt(
          copy(curr_data), # use a copy because we're not sequentially undercoding
          undercoding_prop = prop,
          out_dir = file.path(curr_out_dir, "baseline_data"),
          out_file_prefix = paste0(
            strsplit(file_path_sans_ext(basename(f)), "_baseline_0pt0")[[1]][1], "_underreported"
          ),
          curr_seed = curr_seed
        )
      })
    })

    # Define upcoding spec dfs for TM and MA

    ## TM upcoding spec df
    ### As a reference, we only upcode each of these HCCs 5% overall
    tm_upcoding_spec_df <- tibble(
      hcc = c("hcc238", "hcc125"),
      approach = c("any", "lower severity"),
      upcoding_prop = c(0.05, 0.05)
    )

    ## MA upcoding spec df
    ### Upcoding props are added in loop
    ma_upcoding_spec_df <- tibble(
      hcc = c("hcc238", "hcc125"),
      approach = c("any", "lower severity"),
    )

    # TM upcoding

    ## TM: monitoring period 1, no additional censoring
    upcode_prior_monitoring_period(
      tm_upcoding_spec_df,
      input_dir = file.path(curr_out_dir, "baseline_data"),
      out_dir = file.path(curr_out_dir, "tm_upcoding_m1"),
      pattern = "tm_*",
      starting_seed = tm_upcoding_starting_seed,
      seed_change_interval = 4,
      extract_prefix_fn = get_initial_prefix,
      censoring_prop = 0,
      upcoding_prop_annot = "upcoded_0pt05"
    )

    ## TM: monitoring period 2, no additional censoring
    upcode_prior_monitoring_period(
      tm_upcoding_spec_df,
      input_dir = file.path(curr_out_dir, "tm_upcoding_m1"),
      out_dir = file.path(curr_out_dir, "tm_upcoding_m2"),
      pattern = "^all",
      starting_seed = tm_upcoding_starting_seed * 2,
      seed_change_interval = 4,
      extract_prefix_fn = get_upcoded_prefix,
      censoring_prop = 0,
      upcoding_prop_annot = "upcoded_0pt05"
    )

    # MA upcoding

    ## MA: monitoring period 1, no additional censoring
    upcode_monitoring_period_to_varying_degree(ma_upcoding_spec_df,
      ma_upcoding_props,
      input_dir = file.path(curr_out_dir, "baseline_data"),
      out_dir = file.path(curr_out_dir, "ma_upcoding_m1"),
      pattern = "ma_*",
      starting_seed = ma_upcoding_starting_seed,
      seed_change_interval = num_reps_per_batch,
      extract_prefix_fn = get_initial_prefix,
      censoring_prop = 0
    )

    ## MA: monitoring period 2, no additional censoring
    upcode_monitoring_period_to_varying_degree(ma_upcoding_spec_df,
      ma_upcoding_props,
      input_dir = file.path(curr_out_dir, "ma_upcoding_m1"),
      out_dir = file.path(curr_out_dir, "ma_upcoding_m2"),
      pattern = "^all",
      starting_seed = ma_upcoding_starting_seed * 2,
      seed_change_interval = length(ma_upcoding_props),
      extract_prefix_fn = get_upcoded_prefix,
      censoring_prop = 0
    )

    # Remove baseline data files
    # Since these take up lots of space and are no longer needed
    unlink(here(file.path(curr_out_dir, "baseline_data")), recursive = TRUE)

    # Count all HCCs for DECI estimate and write to file
    all_files_for_deci_estimates <- list.files(here(curr_out_dir),
      pattern = "^all",
      recursive = TRUE,
      full.names = TRUE
    )
    hcc_count_df <- tibble(f = all_files_for_deci_estimates) |>
      rowwise() |>
      mutate(
        path = f,
        grp = strsplit(basename(f), "_")[[1]][3],
        batch = strsplit(basename(f), "_")[[1]][5],
        rep_in_batch = strsplit(basename(f), "_")[[1]][6],
        monitoring_period = strsplit(basename(dirname(f)), "_")[[1]][3],
        upcoded_pct = as.numeric(gsub("pt", ".", strsplit(strsplit(basename(f), "_")[[1]][10], ".csv")[[1]][1])),
        underreported_pct = as.numeric(gsub("pt", ".", strsplit(basename(f), "_")[[1]][8])),
        hcc_count = map_int(f, count_all_hccs)
      )

    if (!dir.exists(here("R/data_simulation/manuscript_simulation/hcc_counts"))) {
      dir.create(here("R/data_simulation/manuscript_simulation/hcc_counts"))
    }

    write_csv(hcc_count_df, file.path(
      here("R/data_simulation/manuscript_simulation/hcc_counts"),
      paste0("hcc_counts_", curr_uuid, ".csv")
    ))

    # Remove all upcoding files that contain every HCC
    # Since they take up lots of space and are no longer needed
    files_to_remove <- list.files(
      path = here(curr_out_dir),
      pattern = "^all",
      recursive = TRUE,
      full.names = TRUE
    )
    file.remove(files_to_remove)
  })
}
