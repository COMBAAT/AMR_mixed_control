library(tictoc)
# generate initial plots
tic()
source("epi_plotting_functions/3_plot_explore_test.R")
toc()

# generate derived plots
source("epi_plotting_functions/4A_compare_outcomes_by_protocol_and_wildlife.R")
source("epi_plotting_functions/4B_compare_outcomes_by_protocol_and_vector_host_ratio.R")
source("epi_plotting_functions/4C_compute_maximum_safe_treatment_intensity.R")
source("epi_plotting_functions/4E_plot_helper.R")
source("epi_plotting_functions/4E_generate_threshold.R")

# all currently spare in G and H
source("epi_plotting_functions/4G_plot_helper.R")
source("epi_plotting_functions/4G_outcome_versus_prevalence.R")
source("epi_plotting_functions/4H_outcomes_versus_prevalence_drug_only.R")
#

source("epi_plotting_functions/5A_generate_patchwork_plots.R")
source("epi_plotting_functions/5B_generate_patchwork_plots.R")
source("epi_plotting_functions/5C_generate_patchwork_plots.R")
source("epi_plotting_functions/5D_generate_patchwork_plots.R")
source("epi_plotting_functions/5E_generate_patchwork_plots.R")

source("epi_plotting_functions/7_create_combination_plots.R")




