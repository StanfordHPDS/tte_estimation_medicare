library(here)
library(tidyverse)
library(ggokabeito)
source(here("R/figures_and_tables/generate_deci_figures.R"))
source(here("R/figures_and_tables/generate_cif_figures.R"))
source(here("R/figures_and_tables/generate_figures_of_estimates.R"))

main <- function() {
  if (!dir.exists(here("manuscript/images"))) {
    dir.create(here("manuscript/images"))
  }

  if (!dir.exists(here("manuscript/appendix/images"))) {
    dir.create(here("manuscript/appendix/images"))
  }

  generate_deci_figures()
  generate_cif_figures()
  generate_figures_of_estimates()
}

main()
