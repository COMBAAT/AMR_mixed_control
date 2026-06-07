
source("4E_plot_helper.R")
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
    Baseline_vector_host_ratio == 20
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

treat_prop_thresh <- 0.5
p1 <- plot_boundary_responsive_tidy(plot_this_filtered, treat_prop_thresh) 
p1 <- p1 + ggtitle(paste0("Case treatment\nproportion < ", treat_prop_thresh))
p1

treat_prop_thresh <- 0.95
p2 <- plot_boundary_responsive_tidy(plot_this_filtered, treat_prop_thresh)
p2 <- p2 + ggtitle(paste0("Case treatment\nproportion < ", treat_prop_thresh))
p2

pA <- p1 + p2 + plot_layout(guides = 'collect') 
pA
filename = paste0("Use_as_fig7_responsive_", "Insecticide_strategy_", this_insecticide_strategy, ".pdf")
plot_name <- paste0("output/ms_figs/", filename)
my_ggsave(plot = pA, filename = plot_name, width = 8, height = 8)

#########################################
#Preventive mode 
#########################################
plot_this_filtered <- plot_this1 %>% 
  filter(treatment_code == 3, 
         NW == 100, prop_cattle_with_insecticide <= 0.8, Insecticide_strategy == this_insecticide_strategy)

treat_freq_thresh <- 3
p1 <- plot_boundary_ongoing_tidy(plot_this_filtered, treat_freq_thresh) 
p1 <- p1 + ggtitle(paste0("Treatment\nfrequency <= ", treat_freq_thresh, " per year"))
p1

treat_freq_thresh <- 6
p2 <- plot_boundary_ongoing_tidy(plot_this_filtered, treat_freq_thresh)
p2 <- p2 + ggtitle(paste0("Treatment\nfrequency <= ", treat_freq_thresh, " per year"))
p2

pB <- p1 + p2 + plot_layout(guides = 'collect') 
pB
filename = paste0("Use_as_fig8_preventive_", "Insecticide_strategy_", this_insecticide_strategy, ".pdf")
plot_name <- paste0("output/ms_figs/", filename)
my_ggsave(plot = pB, filename = plot_name, width = 8, height = 6)
#########################################


#########################################
#Plot boundary by fitness
#########################################
plot_this2 <- subset_for_plotting %>%
  mutate(facet_label = case_when(laXbel == "proph_ongoing" ~ paste0("Longlasting ongoing", "\n",  Insecticide_strategy, " insecticide"),
                                 laXbel == "responsive_curative" ~ paste0("Responsive curative", "\n",  Insecticide_strategy, " insecticide"),
                                 laXbel == "responsive_longlasting" ~ paste0("Responsive longlasting", "\n",  Insecticide_strategy, " insecticide")))

#########################################
# Safe treatment threshold versus fitness for both insecticide strategies

test_0.8 <- plot_this2 %>% mutate(Rres_new = Rres_final / fit_adj * 0.8, fitness = 0.8)
test_0.6 <- plot_this2 %>% mutate(Rres_new = Rres_final / fit_adj * 0.6, fitness = 0.6)
test_0.95 <- plot_this2 %>% mutate(Rres_new = Rres_final / fit_adj * 0.95, fitness = 0.95)
test_0.8 <- plot_this2 %>% mutate(Rres_new = Rres_final / fit_adj * 0.8, fitness = 0.8)
test_0.7 <- plot_this2 %>% mutate(Rres_new = Rres_final / fit_adj * 0.7, fitness = 0.7)
test_0.9 <- plot_this2 %>% mutate(Rres_new = Rres_final / fit_adj * 0.9, fitness = 0.9)
test_0.5 <- plot_this2 %>% mutate(Rres_new = Rres_final / fit_adj * 0.5, fitness = 0.5)

test_all <- rbind(test_0.5, test_0.6, test_0.7, test_0.8, test_0.9) %>% 
  mutate(fitness = as.factor(fitness))

test_all_filtered <- test_all %>% filter(NW == 100, prop_cattle_with_insecticide <= 0.8)

p <- plot_boundary_by_fitness(test_all_filtered)
p

filename = paste0("Use_as_fig9_boundary_by_fitness", ".pdf")
plot_name <- paste0("output/ms_figs/", filename)
my_ggsave(plot = p, filename = plot_name, width = 7, height = 7)
#########################################

#########################################
# Safe treatment threshold versus wildife for both insecticide strategies
p <- plot_boundary_by_wildlife(plot_this2)
p

filename = paste0("Use_as_fig11_boundary_by_wildlife", ".pdf")
plot_name <- paste0("output/ms_figs/", filename)
my_ggsave(plot = p, filename = plot_name, width = 7, height = 7)

#########################################


########################################################
#Stratify by wildlife and colour by Number treated
########################################################
plot_this1 <- subset_for_plotting %>%
  # mutate(facet_label = case_when(laXbel == "proph_ongoing" ~ paste0("Longlasting ongoing", "\n",  Insecticide_strategy, " insecticide"),
  #                                laXbel == "responsive_curative" ~ paste0("Responsive curative", "\n",  Insecticide_strategy, " insecticide"),
  #                                laXbel == "responsive_longlasting" ~ paste0("Responsive longlasting", "\n",  Insecticide_strategy, " insecticide")))
  mutate(facet_label = case_when(laXbel == "proph_ongoing" ~ paste0("Longlasting ongoing", "\n",  "Wildlife ", Wildlife),
                                 laXbel == "responsive_curative" ~ paste0("Responsive curative", "\n",  "Wildlife ", Wildlife),
                                 laXbel == "responsive_longlasting" ~ paste0("Responsive longlasting", "\n",  "Wildlife ", Wildlife)))

plot_this_filtered0 <- plot_this1 %>% 
  filter(
    #NW == 100, 
    prop_cattle_with_insecticide <= 0.8,
    Insecticide_strategy == "Individual"
  ) 

plot_this_filtered <- plot_this_filtered0 %>% filter(treatment_code != 3) 

treat_prop_thresh <- 0.5
p1 <- plot_boundary_responsive_play(plot_this_filtered, treat_prop_thresh, all_levels, colour_vals) 
#p1 <- plot_boundary_responsive_tidy(plot_this_filtered, treat_prop_thresh, num_rows = 2) 
p1 <- p1 + ggtitle(paste0("Case treatment\nproportion < ", treat_prop_thresh))
p1

filename = paste0("Use_as_fig10_Number_treatments", ".pdf")
plot_name <- paste0("output/ms_figs/", filename)
my_ggsave(plot = p1, filename = plot_name, width = 10, height = 8)

########################################################
#Stratify by wildlife and colour by RiskA
########################################################
plot_this1 <- subset_for_plotting %>%
  # mutate(facet_label = case_when(laXbel == "proph_ongoing" ~ paste0("Longlasting ongoing", "\n",  Insecticide_strategy, " insecticide"),
  #                                laXbel == "responsive_curative" ~ paste0("Responsive curative", "\n",  Insecticide_strategy, " insecticide"),
  #                                laXbel == "responsive_longlasting" ~ paste0("Responsive longlasting", "\n",  Insecticide_strategy, " insecticide")))
  mutate(facet_label = case_when(laXbel == "proph_ongoing" ~ paste0("Longlasting ongoing", "\n",  "Wildlife ", Wildlife),
                                 laXbel == "responsive_curative" ~ paste0("Responsive curative", "\n",  "Wildlife ", Wildlife),
                                 laXbel == "responsive_longlasting" ~ paste0("Responsive longlasting", "\n",  "Wildlife ", Wildlife)))

plot_this_filtered0 <- plot_this1 %>% 
  filter(
    #NW == 100, 
    prop_cattle_with_insecticide <= 0.8,
    Insecticide_strategy == "Individual"
  ) 

plot_this_filtered <- plot_this_filtered0 %>% filter(treatment_code != 3) 

treat_prop_thresh <- 0.95
#p1 <- plot_boundary_responsive_play(plot_this_filtered, treat_prop_thresh, all_levels, colour_vals) 
p1 <- plot_boundary_responsive_tidy(plot_this_filtered, treat_prop_thresh, num_rows = 2) 
p1 <- p1 + ggtitle(paste0("Case treatment\nproportion < ", treat_prop_thresh))
p1

filename = paste0("Use_as_fig12_Wildlife_and_RiskA_0.95", ".pdf")
plot_name <- paste0("output/ms_figs/", filename)
my_ggsave(plot = p1, filename = plot_name, width = 10, height = 8)

