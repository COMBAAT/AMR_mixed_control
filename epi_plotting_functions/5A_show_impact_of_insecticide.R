# Generating plots for the manuscript
source("funcs/plot_helper.R")
source("funcs/helper_functions.R")
source("epi_plotting_functions/5A_plot_helper.R")
library(patchwork)


create_combined_plot <- function(df, this_maintain_vector_pop, this_treatment_code,
                                 this_NW_set, this_vector_measure, this_vector_measure_value){
  
  df <- df %>% filter(maintain_vector_pop == this_maintain_vector_pop, 
                      treatment_code == this_treatment_code)
  
  y_vars <- c("prevalence", "Rres_final", "RiskA")
  plot_list <- list()
  for (i in seq_along(y_vars)){
    y_var <- y_vars[i]
    if (this_treatment_code != 3){
      plot_list[[i]] <- plot_y_versus_prop_cattle_with_insecticide_facet_NW(df, y_var, this_NW_set, 
                                                                            this_vector_measure, 
                                                                            this_vector_measure_value)
    } else {
      plot_list[[i]] <- plot_y_versus_prop_cattle_with_insecticide_facet_NW_ttype3(df, y_var, this_NW_set, 
                                                                                   this_vector_measure, 
                                                                                   this_vector_measure_value)
    }
  }
  combined <- plot_list[[1]] / plot_list[[2]] / plot_list[[3]] + plot_layout(axis_titles = 'collect')
  combined
}

desired_prop_insecticide <- round(seq(0, 1, by = 0.05), 3)
df_filtered_by_prop_insecticide <- saved_simulations %>%
  filter( round(prop_cattle_with_insecticide, 3) %in% desired_prop_insecticide)

for(code in 1:3){
  combined_A <- create_combined_plot(df_filtered_by_prop_insecticide, this_maintain_vector_pop = TRUE, this_treatment_code = code,
                                     this_NW_set, this_vector_measure, this_vector_measure_value) & ggtitle("Individual strategy")
  combined_B <- create_combined_plot(df_filtered_by_prop_insecticide, this_maintain_vector_pop = FALSE, this_treatment_code = code,
                                     this_NW_set, this_vector_measure, this_vector_measure_value) & ggtitle("Cooperative strategy")
  combined <- (combined_A | combined_B) + plot_layout(guides = 'collect', axes = 'collect') & 
    theme(legend.position = "bottom") 
  combined
  my_ggsave(combined, paste0("output/ms_figs/5A_main_fig4_treatment_type_", code, ".pdf"), width = 9, height = 8)
}

