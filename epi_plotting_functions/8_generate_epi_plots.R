library(tictoc)
# generate initial plots
tic()
source("3_plot_explore_test.R")
toc()

# generate derived plots
source("4A_compare_ongoing_and_responsive.R")
source("4B_compare_ongoing_and_responsive.R")
source("4C_compare_ongoing_and_responsive.R")
source("5A_generate_patchwork_plots_for_ms.R")
source("5B_generate_patchwork_plots.R")
source("5C_generate_patchwork_plots.R")

source("7_create_combination_plots.R")


