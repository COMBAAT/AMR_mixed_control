
################################################################################
# Load required packages -------------------------------------------------------

library(dplyr)
library(gghighlight)
library(ggplot2)
library(patchwork)

# Source files and function
source("funcs/plot_helper.R")
source("funcs/helper_functions.R")
source("funcs/plots_for_grant_helper.R")

# Load data files --------------------------------------------------------------
load_latest_file <- FALSE
if (load_latest_file == TRUE) {
  latest_file <- get_latest_Rda_file()
  load(latest_file)
} else {
  load("output/test_merge.Rda")
}
# create a directory for the plots
path <- "output/grant_plots3/"
dir.create(path)

option <- 1
choice <- "incidence"
subset_for_plotting <- get_subset_for_plotting(scenarios_df, test, option, scenario = 1, fit_adj_new = 0.6)
label <- unique(subset_for_plotting$label)
if (label == "responsive_quick") {
  plot_title <- "Curative drug"
} 
if (label == "responsive_proph") {
  plot_title <- "Prophylactic drug"
} 
if (label == "proh_ongoing") {
  plot_title <- "Ongoing prophylaxis"
} 

# create the plots
p1_plots <- create_p1_plots(subset_for_plotting, 4000, label, plot_title)
p1_vertical <- p1_plots[[1]] 
p1_inset <- p1_plots[[2]] 

if (label == "responsive_quick") {
  p1_vertical_responsive_quick <- p1_vertical
  p1_inset_responsive_quick <- p1_inset
} 
if (label == "responsive_proph") {
  p1_vertical_responsive_proph <- p1_vertical
  p1_inset_responsive_proph <- p1_inset
} 
if (label == "proh_ongoing") {
  p1_vertical_proph_ongoing <- p1_vertical
  p1_inset_proph_ongoing <- p1_inset
} 

ggsave(paste0(path, "p1_vertical_", label, ".pdf"), p1_vertical, width = 5.1, height = 7.2)
ggsave(paste0(path, "p1_inset_", label, ".pdf"), p1_inset, width = 7.2, height = 5.1)
# ask if both inset panels are defined and if yes, combine
if (exists("p1_inset_responsive_quick") && exists("p1_inset_responsive_proph")) {
  p1_inset_both <- (p1_inset_responsive_quick / p1_inset_responsive_proph) + 
    plot_layout(guides = "collect", axes = "collect", nrow = 2) + 
    plot_annotation('B', caption = ' ')
  ggsave(paste0(path, "p1_inset_both.pdf"), p1_inset_both, width = 5.1, height = 7.0)
}

################################################################################
# create the remaining plots

################################################################################
this_K <- 4000
this_NW <- 100
p2 <- plot_type11b_selective_advantage_by_NW_and_insectide(subset_for_plotting, this_K, ymax = 3, this_NW) + ggtitle(plot_title)

p3 <- plot_type12_yvar_by_NW_and_insectide(subset_for_plotting, "prevalence", this_K, ymax = 1, this_NW) 

p4 <- plot_type12_yvar_by_NW_and_insectide(subset_for_plotting, "RiskE", this_K, ymax = 5, this_NW) 

p5 <- plot_type12_yvar_by_NW_and_insectide(subset_for_plotting, "Incidence", this_K, ymax = 500, this_NW) 

if (choice == "incidence") {
  p_choice <- p5
} else {
  p_choice <- p3
}

if (label == "responsive_quick") {
  p2_responsive_quick <- p2
  p3_responsive_quick <- p3
  p4_responsive_quick <- p4
  p5_responsive_quick <- p5
  p_choice_responsive_quick <- p_choice
} 
if (label == "responsive_proph") {
  p2_responsive_proph <- p2
  p3_responsive_proph <- p3
  p4_responsive_proph <- p4
  p5_responsive_proph <- p5
  p_choice_responsive_proph <- p_choice
} 
if (label == "proph_ongoing") {
  p2_proph_ongoing <- p2
  p3_proph_ongoing <- p3
  p4_proph_ongoing <- p4
  p5_proph_ongoing <- p5
  p_choice_proph_ongoing <- p_choice
} 

panel <- (p_choice + p4) + plot_layout(guides = "collect", axes = "collect", nrow = 1, widths = c(1, 1)) + 
  plot_annotation(caption = " ", title = plot_title, theme=theme(plot.title=element_text(hjust=0.5, size = 20))) 
panel

if (label == "responsive_quick") {
  addition <- "A"
  panel_responsive_quick <- panel 
} 
if (label == "responsive_proph") {
  addition <- "B"
  panel_responsive_proph <- panel 
} 
if (label == "proph_ongoing") {
  addition <- "C"
  panel_proph_ongoing <- panel 
} 
ggsave(paste0(path, "panel_", label,"_", choice, ".pdf"), panel, width = 10.2, height = 4.5)

# ask if panel_responsive_quick, panel_responsive_proph are defined and if yes, combine
if (exists("panel_responsive_quick") && exists("panel_responsive_proph")) {
  panel_both <- (panel_responsive_quick / panel_responsive_proph) + 
    plot_layout(guides = "collect", axes = "collect", nrow = 2) +
    plot_annotation('A', caption = ' ')
  panel_both <- wrap_elements(panel_responsive_quick) / wrap_elements(panel_responsive_proph) 
  panel_both  <- panel_both + 
    plot_annotation('A', caption = ' ')
  ggsave(paste0(path, "panel_both_", choice, ".pdf"), panel_both, width = 10.2, height = 9.0)
}


panel3 <- wrap_elements(panel_both) + wrap_elements(p1_inset_both) + plot_layout(guides = "collect", axes = "collect", widths = c(2, 1.5))
panel3
ggsave(paste0(path, "Model_figureX_with_", choice, ".pdf"), panel3, width = 12.2, height = 9.0)

################################################################################
# check plots exist before creating panel
if (exists("p2_responsive_quick") && exists("p3_responsive_quick") && exists("p4_responsive_quick") && exists("p4_responsive_proph")) {
  panel2 <- (p2_responsive_quick + ggtitle("Curative drug") + p3_responsive_quick + ggtitle("Curative drug") + plot_layout(guides = "collect", axes = "collect")) / 
    (p4_responsive_quick + ggtitle("Curative drug") + p4_responsive_proph + ggtitle("Prophylactic drug") + plot_layout(guides = "collect", axes = "collect")) + 
    #plot_layout(guides = "collect", axes = "collect", nrow = 2) + 
    plot_annotation('B', caption = ' ')
  panel2
  ggsave(paste0(path, "panel2.pdf"), panel2, width = 10.2, height = 9.0)
}
################################################################################

panel1plus2 <- wrap_elements(p1_vertical_responsive_quick) + wrap_elements(panel2) + 
  plot_layout(guides = "collect", axes = "collect", widths = c(1.7,2)) 
panel1plus2
ggsave(paste0(path, "panel1plus2.pdf"), panel1plus2, width = 12.2, height = 9.0)

