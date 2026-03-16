library(tictoc)
# generate initial plots
tic()
source("epi_plotting_functions/3_plot_explore_test.R")
toc()

# generate derived plots
source("epi_plotting_functions/4A_compare_ongoing_and_responsive.R")
source("epi_plotting_functions/4B_compare_ongoing_and_responsive.R")
source("epi_plotting_functions/4C_compare_ongoing_and_responsive.R")
source("epi_plotting_functions/5A_generate_patchwork_plots_for_ms.R")
source("epi_plotting_functions/5B_generate_patchwork_plots.R")
source("epi_plotting_functions/5C_generate_patchwork_plots.R")

source("epi_plotting_functions/7_create_combination_plots.R")

source("epi_plotting_functions/4D_generate_panel.R")
