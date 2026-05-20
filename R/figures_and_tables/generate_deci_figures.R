library(here)
library(tidyverse)
library(ggokabeito)

generate_deci_figures <- function() {
  # Unzip hcc count files
  unzip(here("R/data_simulation/manuscript_simulation/hcc_counts.zip"),
    exdir = here("R/data_simulation/manuscript_simulation/")
  )

  # Aggregate all HCC count files across parallel jobs
  aggregate_hcc_df <- list.files(here("R/data_simulation/manuscript_simulation/hcc_counts/"),
    full.names = TRUE
  ) |>
    lapply(\(f) read_csv(f, show_col_types = FALSE)) |>
    bind_rows() |>
    rowwise() |>
    mutate(batch_rep = paste0(batch, "_", rep_in_batch))

  # TM data for comparison
  tm_df <- aggregate_hcc_df |>
    filter(grp == "tm") |>
    select(grp, monitoring_period, underreported_pct, batch_rep, hcc_count)

  # MA data for comparison, separately by upcoding percentage
  ma_20pct <- aggregate_hcc_df |>
    filter(upcoded_pct == 0.2) |>
    select(grp, monitoring_period, hcc_count, batch_rep)
  ma_25pct <- aggregate_hcc_df |>
    filter(upcoded_pct == 0.25) |>
    select(grp, monitoring_period, hcc_count, batch_rep)
  ma_30pct <- aggregate_hcc_df |>
    filter(upcoded_pct == 0.3) |>
    select(grp, monitoring_period, hcc_count, batch_rep)

  # Estimate DECI across replicates for each upcoding proportion separately
  combined_20pct <- ma_20pct |>
    full_join(tm_df,
      by = c("monitoring_period", "batch_rep"),
      relationship = "many-to-many",
      suffix = c("_ma", "_tm")
    ) |>
    mutate(deci = hcc_count_ma / hcc_count_tm) |>
    group_by(monitoring_period, underreported_pct) |>
    summarize(
      mean_deci = mean(deci),
      se_deci = sd(deci),
      ci_95pct_lower = mean_deci - 1.96 * se_deci,
      ci_95pct_higher = mean_deci + 1.96 * se_deci,
      upcoding_pct = 20
    )

  combined_25pct <- ma_25pct |>
    full_join(tm_df,
      by = c("monitoring_period", "batch_rep"),
      relationship = "many-to-many",
      suffix = c("_ma", "_tm")
    ) |>
    mutate(deci = hcc_count_ma / hcc_count_tm) |>
    group_by(monitoring_period, underreported_pct) |>
    summarize(
      mean_deci = mean(deci),
      se_deci = sd(deci),
      ci_95pct_lower = mean_deci - 1.96 * se_deci,
      ci_95pct_higher = mean_deci + 1.96 * se_deci,
      upcoding_pct = 25
    )

  combined_30pct <- ma_30pct |>
    full_join(tm_df,
      by = c("monitoring_period", "batch_rep"),
      relationship = "many-to-many",
      suffix = c("_ma", "_tm")
    ) |>
    mutate(deci = hcc_count_ma / hcc_count_tm) |>
    group_by(monitoring_period, underreported_pct) |>
    summarize(
      mean_deci = mean(deci),
      se_deci = sd(deci),
      ci_95pct_lower = mean_deci - 1.96 * se_deci,
      ci_95pct_higher = mean_deci + 1.96 * se_deci,
      upcoding_pct = 30
    )
  all_combined_deci_df <- bind_rows(
    combined_20pct,
    combined_25pct,
    combined_30pct
  ) |>
    mutate(
      monitoring_period = case_when(
        monitoring_period == "m1" ~ "M1",
        monitoring_period == "m2" ~ "M2"
      ),
      underreported_pct = underreported_pct * 100
    )

  # plot
  all_combined_deci_df |>
    ggplot(aes(x = monitoring_period, y = mean_deci, color = as.factor(upcoding_pct))) +
    geom_point(size = 2) +
    facet_grid(
      underreported_pct ~ upcoding_pct,
      labeller = labeller(
        underreported_pct = label_value,
        upcoding_pct = label_value
      )
    ) +
    labs(
      x = "Monitoring period",
      y = expression(widehat(DECI)^{
        "\ \u2020"
      }),
      color = "Medicare Advantage upcoding percentage"
    ) +
    guides(y.sec = guide_none("Traditional Medicare undercoding percentage")) +
    ylim(0.9, 1.22) +
    scale_color_okabe_ito() +
    theme_light(base_size = 13) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    )

  # Save as PNG and TIFF
  ggsave(here("manuscript/images/enache_figure4.png"), width = 8, height = 5, device = "png", dpi = 300)
  ggsave(here("manuscript/images/enache_figure4.tiff"), width = 8, height = 5, device = "tiff", dpi = 300)
}
