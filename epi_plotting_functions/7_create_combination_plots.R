################################################################################
# Load required packages -------------------------------------------------------

library(dplyr)
library(gghighlight)
library(ggplot2)
library(patchwork)

# Source files and function
source("funcs/plot_helper.R")
source("funcs/helper_functions.R")
source("funcs/create_combination_plots_helper.R")
#source("funcs/plots_for_grant_helper.R")

# Load data files --------------------------------------------------------------
load_latest_file <- TRUE
if (load_latest_file == TRUE) {
  latest_file <- get_latest_Rda_file()
  load(latest_file)
  path <- gsub(".Rda", "/", latest_file)
  dir.create(path)
} else {
  load("output/April11_quick_proph.Rda")
  path <- "output/April11_quick_proph//"
  dir.create(path)
}

saved_simulations <- saved_simulations 

################################################################################
mainvecpop <- FALSE
this_vector_measure_value <- 25
this_vector_measure <- "Baseline_vector_host_ratio"
this_NW <- 100
R0_threshold <- 1.0
this_insecticide <- 0.0

################################################################################
plot_titles <- c("Curative drug", "Longlasting drug", "Ongoing prophylaxis")
labels <- c("responsive_curative", "responsive_longlasting", "proph_ongoing")
data_subsets <- list()
#use_cc <- FALSE # whether to use carrying capacity or not
spec <- paste0("_", mainvecpop)
for (option in 1:2) {
  #subset <- create_data_subsets(saved_simulations, option)
  subset <- saved_simulations %>% filter(treatment_code == option)
  scenario_choice <- show_scenarios(scenarios_df)
  print(scenario_choice)
  subset_for_plotting <- select_scenario(scenario_choice, subset, mainvecpop)
  # adjust fitness post simulation, if desired
  subset_for_plotting <- adjust_fitness(subset_for_plotting, fit_adj_new = 0.8)
  subset_for_plotting$ratio <- subset_for_plotting$Rres_final
  subset_for_plotting <- subset_for_plotting 
  data_subsets[[option]] <- subset_for_plotting
}

################################################################################

# create the selective advantage plots
pSA_vertical <- list()
pSA_inset <- list()
for (plot_choice in c("by_insecticide", "by_NW")) {
  for (option in 1:2) {
    pSA_plots <- create_selective_advantage_combination_plots(data_subsets[[option]], this_NW, this_insecticide, R0_threshold, labels[[option]], plot_titles[option], plot_choice, this_vector_measure, this_vector_measure_value)
    pSA_vertical[[option]] <- pSA_plots[[1]]
    pSA_inset[[option]] <- pSA_plots[[2]]
    my_ggsave(plot = pSA_vertical[[option]], filename = paste0(path, "pSA_vertical_", plot_choice, "_", labels[option], spec, ".pdf"), width = 5.1, height = 7.2)
    my_ggsave(plot = pSA_inset[[option]], filename = paste0(path, "pSA_inset_", plot_choice, "_", labels[option], spec, ".pdf"), width = 7.2, height = 5.1)
  }


  pSA_inset_both <- (pSA_inset[[1]] / pSA_inset[[2]]) +
    plot_layout(guides = "collect", axes = "collect", nrow = 2) +
    plot_annotation("B", caption = " ")
  my_ggsave(plot = pSA_inset_both, filename = paste0(path, "pSA_inset_both", "_", plot_choice, spec, ".pdf"), width = 5.1, height = 7.0)
}

################################################################################
p4_plots <- list()
p5_plots <- list()
panel_plots <- list()

for (option in 1:2) {
  p4_plots[[option]] <- plot_type12_yvar_by_NW_and_insectide(data_subsets[[option]], "RiskE", ymax = 5, this_NW, this_vector_measure, this_vector_measure_value) 
  p5_plots[[option]] <- plot_type12_yvar_by_NW_and_insectide(data_subsets[[option]], "Incidence", ymax = 500, this_NW, this_vector_measure, this_vector_measure_value) 
  panel_plots[[option]] <-(p5_plots[[option]] + p4_plots[[option]]) + plot_layout(guides = "collect", axes = "collect", nrow = 1, widths = c(1, 1)) + 
    plot_annotation(caption = " ", title = plot_titles[option], theme=theme(plot.title=element_text(hjust=0.5, size = 20))) 
  my_ggsave(plot = panel_plots[[option]], filename = paste0(path, "panel_", labels[option],"_", "incidence", spec, ".pdf"), width = 10.2, height = 4.5)
}

panel_both <- wrap_elements(panel_plots[[1]]) / wrap_elements(panel_plots[[2]]) 
panel_both  <- panel_both + 
  plot_annotation('A', caption = ' ')
my_ggsave(plot = panel_both, filename = paste0(path, "panel_both_", "incidence", spec, ".pdf"), width = 10.2, height = 9.0)


panel_final <- wrap_elements(panel_both) + wrap_elements(pSA_inset_both) + plot_layout(guides = "collect", axes = "collect", widths = c(2, 1.5))
my_ggsave(plot = panel_final, filename = paste0(path, "panel_figureX_with_", "incidence", spec, ".pdf"), width = 12.2, height = 9.0)

################################################################################