
source("epi_plotting_functions/4F_plot_helper.R")
#########################################

for_plotting <- saved_simulations %>% 
  mutate(treat_percentage = 100 * treat_prop,
         No_trt_cat_curtailed = case_when(No_trt_cat > 100 ~ 105, TRUE ~ No_trt_cat),
         Number_treated = cut(No_trt_cat_curtailed, 
                              breaks = c(0, 10, seq(20, 105 + 20, by = 20)), include.lowest = TRUE),
         drug_treatment = treat_prop + treatments_per_year,
    Insecticide = as.factor(prop_cattle_with_insecticide),
         Wildlife = as.factor(NW),
         Strategy = as.factor(treatment_code),
         facet_label = paste0("Wildlife: ", NW, "\nInsecticide: ", Insecticide),
         prevalence = prevalence_new,
         Insecticide_strategy = case_when(maintain_vector_pop == FALSE ~ "Cooperative",
                                          maintain_vector_pop == TRUE ~ "Individual"))

# Define the full set of levels once, from the full plotting data
all_levels <- levels(for_plotting$Number_treated)

# Create a named palette once
colour_vals <- setNames(
  scales::hue_pal()(length(all_levels)),
  all_levels
)

subset_for_plotting <- for_plotting %>% 
  filter(
    Baseline_vector_host_ratio == this_vector_measure_value
  )


########################################################
# Plot safe treatment boundaries
########################################################

#########################################
#Responsive mode 
#########################################
this_insecticide_strategy <- "Individual"

plot_this1 <- subset_for_plotting %>%
  mutate(facet_label = case_when(laXbel == "proph_ongoing" ~ "Longlasting ongoing",
                                 laXbel == "responsive_curative" ~ "Responsive curative",
                                 laXbel == "responsive_longlasting" ~ "Responsive longlasting"))

#########################################
plot_this_filtered <- plot_this1 %>% 
  filter(treatment_code != 3, 
         NW == 100, prop_cattle_with_insecticide <= 0.8, Insecticide_strategy == this_insecticide_strategy)

treat_prop_thresh <- 0.95
p1 <- plot_boundary_tidy_RiskA(plot_this_filtered, "treat_prop", treat_prop_thresh) 
p1 <- p1 + ggtitle(paste0("Maximum proportion\nof cases treated: ", treat_prop_thresh))

treat_prop_thresh <- 0.5
p2 <- plot_boundary_tidy_RiskA(plot_this_filtered, "treat_prop", treat_prop_thresh)
p2 <- p2 + ggtitle(paste0("Maximum proportion\nof cases treated: ", treat_prop_thresh))

treat_prop_thresh <- 0.25
p3 <- plot_boundary_tidy_RiskA(plot_this_filtered, "treat_prop", treat_prop_thresh)
p3 <- p3 + ggtitle(paste0("Maximum proportion\nof cases treated: ", treat_prop_thresh))

pA <- p1 + p2 +p3 + plot_layout(guides = 'collect', axes = 'collect') 
pA
filename = paste0("4F_main_fig6A_responsive_", "Insecticide_strategy_", 
                  this_insecticide_strategy, "_RiskA.pdf")
plot_name <- paste0("output/ms_figs/", filename)
my_ggsave(plot = pA, filename = plot_name, width = 11, height = 8)

################################################################################
# Plots for ongoing treatment
plot_this_filtered <- plot_this1 %>% 
  filter(treatment_code == 3, 
         NW == 100, prop_cattle_with_insecticide <= 0.8, Insecticide_strategy == this_insecticide_strategy)

treat_freq_thresh <- 6
p1 <- plot_boundary_tidy_RiskA(plot_this_filtered, "treatments_per_year", treat_freq_thresh) 
p1 <- p1 + ggtitle(paste0("Max herd treatment\nfrequency ", treat_freq_thresh, " per year"))


treat_freq_thresh <- 3
p2 <- plot_boundary_tidy_RiskA(plot_this_filtered, "treatments_per_year", treat_freq_thresh)
p2 <- p2 + ggtitle(paste0("Max herd treatment\nfrequency ", treat_freq_thresh, " per year"))


treat_freq_thresh <- 1
p3 <- plot_boundary_tidy_RiskA(plot_this_filtered, "treatments_per_year", treat_freq_thresh)
p3 <- p3 + ggtitle(paste0("Max herd treatment\nfrequency ", treat_freq_thresh, " per year"))


pB <- p1 + p2 + p3 + plot_layout(guides = 'collect') 
pB
filename = paste0("4F_main_fig6B_ongoing_", "Insecticide_strategy_", 
                  this_insecticide_strategy, "_RiskA.pdf")
plot_name <- paste0("output/ms_figs/", filename)
my_ggsave(plot = pB, filename = plot_name, width = 11, height = 4.5)
#########################################
