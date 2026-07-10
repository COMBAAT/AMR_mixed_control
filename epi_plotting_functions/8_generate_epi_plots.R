library(ggplot2)
library(patchwork)
library(metR)
source("funcs/helper_functions.R")

# Load data files --------------------------------------------------------------
load_latest_file <- TRUE
if (load_latest_file == TRUE) {
  latest_file <- get_latest_Rda_file()
  load(latest_file)
  folder_name <- gsub(".Rda", "/", latest_file)
  dir.create(folder_name)
} else {
  load("output/17June2026_n6_high_treatfreq_resolution.Rda")
  folder_name <- "output/17June2026_n6_high_treatfreq_resolution/"
  dir.create(folder_name)
}

# Set plotting defaults
this_NW_set <- c(0, 100, 200)
this_vector_measure_value <- 20
this_vector_measure <- "Baseline_vector_host_ratio"

# generate main text plots
source("epi_plotting_functions/4A_compare_outcomes_by_protocol_and_wildlife.R") # fig 2 and 3
source("epi_plotting_functions/5A_show_impact_of_insecticide.R")
source("epi_plotting_functions/4F_plot_helper.R")
source("epi_plotting_functions/4F_main_fig5_Number_treated_threshold.R")
source("epi_plotting_functions/4F_main_fig6_RiskA_threshold.R")
source("epi_plotting_functions/4F_main_fig7_Rres_threshold.R")

#older versions of the threshold plots
#source("epi_plotting_functions/4E_plot_helper.R")
#source("epi_plotting_functions/4E_generate_threshold.R")
# federica plots
source("epi_plotting_functions/Federica_plots_by_fitness.R")
source("epi_plotting_functions/Federica_plots.R")



# supp mat?
source("epi_plotting_functions/4C_compute_maximum_safe_treatment_intensity.R")
source("epi_plotting_functions/4B_compare_outcomes_by_protocol_and_vector_host_ratio.R")

#spare, too confusing to use
source("epi_plotting_functions/4G_plot_helper.R")
source("epi_plotting_functions/4G_outcome_versus_prevalence.R")
source("epi_plotting_functions/4H_outcomes_versus_prevalence_drug_only.R")

# old combination plots
source("epi_plotting_functions/7_create_combination_plots.R")

# these require 3_plot_explore_test.R to have been run
# generate exploratory plots
source("epi_plotting_functions/3_plot_explore_test.R")
source("epi_plotting_functions/5G_generate_patchwork_plots.R")
source("epi_plotting_functions/5D_generate_patchwork_plots.R") 
source("epi_plotting_functions/5C_generate_patchwork_plots.R")
source("epi_plotting_functions/5E_generate_patchwork_plots.R")






