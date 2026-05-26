library(here)
library(tidyverse)
library(ggokabeito)

generate_figures_of_estimates <- function() {
  # Aggregate all RMTL files
  aggregate_rmtl_df <- list.files(here("R/data_simulation/manuscript_simulation/output_files/"),
    pattern = "^rmtl_",
    full.names = TRUE
  ) |>
    lapply(\(f) read_csv(f, show_col_types = FALSE)) |>
    bind_rows() |>
    filter(exposure == "2_ma") |>
    rowwise() |>
    mutate(batch_rep = paste0(batch, "_", rep_in_batch))

  # Within monitoring period RMTL estimates
  within_rmtl_df <- aggregate_rmtl_df |>
    group_by(hcc, monitoring_period, comparison) |>
    summarise(
      mean_rmtl = mean(estimate),
      se_rmtl = sd(estimate),
      ci_95pct_lower = mean_rmtl - 1.96 * se_rmtl,
      ci_95pct_higher = mean_rmtl + 1.96 * se_rmtl
    ) |>
    ungroup() |>
    rowwise() |>
    mutate(
      underreporting_pct = as.numeric(gsub("pt", ".", strsplit(comparison, "_")[[1]][2])) * 100,
      upcoding_pct = as.numeric(gsub("pt", ".", strsplit(comparison, "_")[[1]][5])) * 100,
      monitoring_period = case_when(
        monitoring_period == "m1" ~ "M1",
        monitoring_period == "m2" ~ "M2"
      )
    )

  for (h in unique(within_rmtl_df$hcc)) {
    # Define filename and y axis maximum value depending on the HCC
    if (h == "hcc238") {
      curr_png_file_name <- "enache_figure3.png"
      out_path <- "manuscript/images"
      y_axis_max <- 0.4
    } else if (h == "hcc125") {
      curr_png_file_name <- "enache_supp_section2_figure6.png"
      out_path <- "manuscript/appendix/images"
      y_axis_max <- 0.15
    }
    curr_tiff_file_name <- gsub(".png", ".tiff", curr_png_file_name)

    within_rmtl_df |>
      filter(hcc == h) |>
      ggplot(aes(x = monitoring_period, y = mean_rmtl, color = as.factor(upcoding_pct))) +
      geom_point(size = 2) +
      facet_grid(
        underreporting_pct ~ upcoding_pct,
        labeller = labeller(
          underreporting_pct = label_value,
          upcoding_pct = label_value
        )
      ) +
      labs(
        x = "Monitoring period",
        y = expression(hat(psi)),
        color = "Medicare Advantage upcoding percentage"
      ) +
      guides(y.sec = guide_none("Traditional Medicare undercoding percentage")) +
      ylim(0, y_axis_max) +
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
