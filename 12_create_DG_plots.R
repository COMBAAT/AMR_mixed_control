library(ggplot2)
library(dplyr)
library(patchwork)
library(tictoc)

source("funcs/helper_functions.R")
source("funcs/epi_outputs.R")
source("funcs/plot_helper.R")
source("funcs/plot_settings.R")
source("plots_for_TZ_helper.R")
source("funcs/plot_settings.R")

###############################################################################
# Generate DG style plots for selected scenarios
###############################################################################
# Choose extra plotting filters
this_NW <- 0
main_vec <- TRUE
this_K_variable_value <- 50
data_for_DG_plots <- data_with_K_selection %>% 
  filter(NW == this_NW, maintain_vector_pop == main_vec,
         K_variable == this_K_variable_value)

table(data_for_DG_plots$prop_cattle_with_insecticide)

  data_for_DG_plots <- data_for_DG_plots %>% 
    filter(near(prop_cattle_with_insecticide, 0.0) |
             near(prop_cattle_with_insecticide, 0.1) |
             near(prop_cattle_with_insecticide, 0.2) |
             near(prop_cattle_with_insecticide, 0.3) |
             near(prop_cattle_with_insecticide, 0.4) |
             near(prop_cattle_with_insecticide, 0.5) )

  table(data_for_DG_plots$prop_cattle_with_insecticide)
###############################################################################
props_all <- sort(unique(data_for_DG_plots$prop_cattle_with_insecticide_factor))
base_cols <- get_base_cols(props_all, my_palette = projector_cols_warm_first)
base_cols

###############################################################################
all_plot_details <- paste0("_maintain_vec_pop_", main_vec, "_K_variable_", this_K_variable_value, "_NW_", this_NW)
this_width <- 9.5
###############################################################################
y_vars <- c("Incidence", "prevalence", "treat_insecticide_cost", 
            "sum_averted_production_losses", "R0sen_final", "RiskA", "Rres_final")
y_maxes <- c(1000, 1, 1500, 2000, 8, 8, 8)
names(y_maxes) <- y_vars
y_maxes

################################################################################
for (y_var in y_vars) {
  plot_this <- data_for_DG_plots
  y_max <- y_maxes[y_var]
  plot_this$y <- plot_this[, y_var]
  
  
  for (highlight_groups in 6:6){
    cols <- make_cols_highlight(highlight_props = props_all[1:highlight_groups], 
                                props_all = props_all,
                                base_cols = base_cols)
    
    if (y_var == "Rres_final") {
      p <- plot_panel_by_treatment_type(plot_this, y_var, y_max, rectangle = TRUE)
    } else {
      p <- plot_panel_by_treatment_type(plot_this, y_var, y_max)
    }
    
    this_plot_details <- paste0(y_var, all_plot_details)
    filename <- paste0("output/SALT_TZ/DG_plots/", this_plot_details, "_hlight_", highlight_groups, ".pdf")
    my_ggsave(p, filename, width = this_width, height = 4.5)
  }
  p
}
################################################################################


