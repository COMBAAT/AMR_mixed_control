################################################################################
# Load required packages -------------------------------------------------------

library(dplyr)
library(gghighlight)
library(ggplot2)
library(patchwork)

# Source files and function
source("funcs/plot_helper.R")
source("funcs/helper_functions.R")
#source("funcs/plots_for_grant_helper.R")

# Load data files --------------------------------------------------------------
load_latest_file <- TRUE
if (load_latest_file == TRUE) {
  latest_file <- get_latest_Rda_file()
  load(latest_file)
  path <- gsub(".Rda", "/", latest_file)
  dir.create(path)
} else {
  load("output/Nov28_quick_proph.Rda")
  path <- "output/Nov28_quick_proph/"
  dir.create(path)
}


plot_titles <- c("Curative drug", "Prophylactic drug", "Ongoing prophylaxis")
labels <- c("responsive_quick", "responsive_proph", "proh_ongoing")
data_subsets <- list()
for (option in 1:2) {
  data_subsets[[option]] <- get_subset_for_plotting(scenarios_df, test, option, scenario = 1, fit_adj_new = 0.6)
}

################################################################################
this_K <- 4000
this_NW <- 100
################################################################################

# create the selective advantage plots
pSA_vertical <- list()
pSA_inset <- list()
for (option in 1:2) {
  pSA_plots <- create_selective_advantage_combination_plots(data_subsets[[option]], this_K, this_NW, labels[[option]], plot_titles[option])
  pSA_vertical[[option]] <- pSA_plots[[1]]
  pSA_inset[[option]] <- pSA_plots[[2]]
  ggsave(paste0(path, "pSA_vertical_", labels[option], ".pdf"), pSA_vertical[[option]], width = 5.1, height = 7.2)
  ggsave(paste0(path, "pSA_inset_", labels[option], ".pdf"), pSA_inset[[option]], width = 7.2, height = 5.1)
}

pSA_inset_both <- (pSA_inset[[1]] / pSA_inset[[2]]) + 
  plot_layout(guides = "collect", axes = "collect", nrow = 2) + 
  plot_annotation('B', caption = ' ')
ggsave(paste0(path, "pSA_inset_both.pdf"), pSA_inset_both, width = 5.1, height = 7.0)


################################################################################
################################################################################
p4_plots <- list()
p5_plots <- list()
panel_plots <- list()

for (option in 1:2) {
  p4_plots[[option]] <- plot_type12_yvar_by_NW_and_insectide(data_subsets[[option]], "RiskE", this_K, ymax = 5, this_NW) 
  p5_plots[[option]] <- plot_type12_yvar_by_NW_and_insectide(data_subsets[[option]], "Incidence", this_K, ymax = 500, this_NW) 
  panel_plots[[option]] <-(p5_plots[[option]] + p4_plots[[option]]) + plot_layout(guides = "collect", axes = "collect", nrow = 1, widths = c(1, 1)) + 
    plot_annotation(caption = " ", title = plot_titles[option], theme=theme(plot.title=element_text(hjust=0.5, size = 20))) 
  ggsave(paste0(path, "panel_", labels[option],"_", "incidence", ".pdf"), panel_plots[[option]], width = 10.2, height = 4.5)
}

panel_both <- wrap_elements(panel_plots[[1]]) / wrap_elements(panel_plots[[2]]) 
panel_both  <- panel_both + 
  plot_annotation('A', caption = ' ')
ggsave(paste0(path, "panel_both_", "incidence", ".pdf"), panel_both, width = 10.2, height = 9.0)


panel_final <- wrap_elements(panel_both) + wrap_elements(pSA_inset_both) + plot_layout(guides = "collect", axes = "collect", widths = c(2, 1.5))
panel_final
ggsave(paste0(path, "panel_figureX_with_", "incidence", ".pdf"), panel_final, width = 12.2, height = 9.0)
