library(here)
library(tidyverse)
library(ggokabeito)

generate_cif_figures <- function() {
  unzip(here("R/data_simulation/manuscript_simulation/output_files.zip"),
    exdir = here("R/data_simulation/manuscript_simulation/")
  )

  # Aggregate all CIF files
  aggregate_cif_df <- list.files(here("R/data_simulation/manuscript_simulation/output_files/"),
    pattern = "^cif_",
    full.names = TRUE
  ) |>
    lapply(\(f) read_csv(f, show_col_types = FALSE)) |>
    bind_rows() |>
    rowwise() |>
    mutate(batch_rep = paste0(batch, "_", rep_in_batch))

  # Get mean and SE across replicates
  cif_summary_df <- aggregate_cif_df |>
    group_by(hcc, monitoring_period, comparison, exposure, batch_rep, time) |>
    summarize(
      mean_cif = mean(estimate),
      se_cif = sd(estimate),
      ci_95pct_lower = mean_cif - 1.96 * se_cif,
      ci_95pct_higher = mean_cif + 1.96 * se_cif
    ) |>
    ungroup() |>
    rowwise() |>
    mutate(
      exposure = case_when(
        exposure == "1_tm" ~ "Traditional Medicare (TM)",
        exposure == "2_ma" ~ "Medicare Advantage (MA)"
      ),
      underreporting_pct = as.numeric(gsub("pt", ".", strsplit(comparison, "_")[[1]][2])) * 100,
      upcoding_pct = as.numeric(gsub("pt", ".", strsplit(comparison, "_")[[1]][5])) * 100,
      monitoring_period = case_when(
        monitoring_period == "m1" ~ "M1",
        monitoring_period == "m2" ~ "M2"
      )
    )

  for (h in unique(cif_summary_df$hcc)) {
    for (p in unique(cif_summary_df$upcoding_pct)) {
      # Define file name according to HCC and upcoding percentage
      if (h == "hcc238") {
        curr_png_file_name <- case_when(
          p == 20 ~ "enache_figure2.png",
          p == 25 ~ "enache_supp_section2_figure1.png",
          p == 30 ~ "enache_supp_section2_figure2.png"
        )
        out_path <- case_when(
          p == 20 ~ "manuscript/images",
          p != 20 ~ "manuscript/appendix/images"
        )
      } else if (h == "hcc125") {
        curr_png_file_name <- case_when(
          p == 20 ~ "enache_supp_section2_figure3.png",
          p == 25 ~ "enache_supp_section2_figure4.png",
          p == 30 ~ "enache_supp_section2_figure5.png"
        )
        out_path <- "manuscript/appendix/images"
      }
      curr_tiff_file_name <- gsub(".png", ".tiff", curr_png_file_name)

      cif_summary_df |>
        filter(hcc == h & upcoding_pct == p) |>
        ggplot(aes(x = time, y = mean_cif, color = exposure, linetype = exposure)) +
        geom_step(linewidth = 1, na.rm = TRUE) +
        geom_point(size = 1, alpha = 0.7) +
        facet_grid(underreporting_pct ~ monitoring_period) +
        labs(
          color = "Group",
          linetype = "Group",
          x = "Time within monitoring period",
          y = "Cumulative incidence"
        ) +
        guides(y.sec = guide_none("Traditional Medicare undercoding percentage")) +
        scale_color_okabe_ito() +
        theme_light(base_size = 13) +
        theme(
          legend.position = "top",
          panel.grid.minor = element_blank()
        )

      # Save as PNG and TIFF
      ggsave(file.path(here(out_path), curr_png_file_name), width = 8, height = 5, device = "png", dpi = 300)
      ggsave(file.path(here(out_path), curr_tiff_file_name), width = 8, height = 5, device = "tiff", dpi = 300)
    }
  }
}
