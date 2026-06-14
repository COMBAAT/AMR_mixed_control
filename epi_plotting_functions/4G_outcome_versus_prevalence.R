
source("epi_plotting_functions/4G_plot_helper.R")
#########################################
for_plotting <- saved_simulations %>% 
  mutate(treat_percentage = 100 * treat_prop,
    Insecticide = as.factor(prop_cattle_with_insecticide),
         Wildlife = as.factor(NW),
         Strategy = as.factor(treatment_code),
         facet_label = paste0("Wildlife: ", NW, "\nInsecticide: ", Insecticide),
         prevalence = prevalence_new,
         Insecticide_strategy = case_when(maintain_vector_pop == FALSE ~ "Cooperative",
                                          maintain_vector_pop == TRUE ~ "Individual"))

subset_for_plotting <- for_plotting %>% 
  filter(
    Baseline_vector_host_ratio == 20
  )

subset_for_plotting_reduced_insecticide <- subset_for_plotting %>% 
  filter(near(prop_cattle_with_insecticide, 0.0) | near(prop_cattle_with_insecticide, 0.1) | 
           near(prop_cattle_with_insecticide, 0.2) | near(prop_cattle_with_insecticide, 0.3) )
########################################################
# Plot treatment coparison panels
########################################################
plot_this1 <- subset_for_plotting_reduced_insecticide 

y_vars <- c("Rres_final", "RiskA", "PF_final", "PIs_final", "CIs_final")

for(mainvecpop in c(TRUE, FALSE)) {
  for (y_var in y_vars) {
    p <- plot_treatment_comparison_panel(y_var, plot_this1, mainvecpop)
    filename = paste0("4G_spare_", y_var,"_", mainvecpop, ".pdf")
    plot_name <- paste0("output/ms_figs/", filename)
    my_ggsave(plot = p, filename = plot_name, width = 8, height = 9)
  }
  
  
  p_RiskA <- plot_treatment_comparison_panel("RiskA", plot_this1, mainvecpop)
  p_Rres <- plot_treatment_comparison_panel("Rres_final", plot_this1, mainvecpop)
  p_combined <- p_RiskA + p_Rres + plot_layout(guides = 'collect') & theme(legend.position = "bottom")
  p_combined
  
  filename = paste0("4G_spare_figSX_combined_", mainvecpop, ".pdf")
  plot_name <- paste0("output/ms_figs/", filename)
  my_ggsave(plot = p_combined, filename = plot_name, width = 12, height = 9)
}

