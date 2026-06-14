rm(list = ls()[!grepl("^(plot|df|this_NW_set|this_vector_measure|saved_simulations)", ls())])
source("funcs/plot_helper.R")
source("funcs/compare_responsive_and_ongoing_helper.R")

################################
this_NW <- 100
this_prop_insecticide = 0.0
this_vector_measure <- "Baseline_vector_host_ratio"

expanded_df <- saved_simulations %>% 
  mutate(Risk_per_treatment = case_when(No_trt_cat > 0 ~ RiskA/No_trt_cat, T ~ NA))
subset1 <- expanded_df %>% filter(maintain_vector_pop == F)

# create dataframes for plotting
plot_this <- subset1 %>%
  filter(
    Baseline_vector_host_ratio %in% c(this_vector_measure_value)
  ) %>%
  mutate(prop_cattle_with_insecticide = as.factor(prop_cattle_with_insecticide))

subset2 <- subset1 %>%
  filter(
    NW == this_NW,
    prop_cattle_with_insecticide == this_prop_insecticide
  ) %>%
  mutate_at(c("prop_cattle_with_insecticide", "NW", "Baseline_vector_host_ratio"), as.factor)

################################

p <- plot_type20(plot_this, x_var = "prevalence", y_var = "No_trt_cat", this_prop_insecticide)
p1 <- p # & theme(legend.position = "bottom")

p <- plot_type20(plot_this, x_var = "prevalence", y_var = "RiskA", this_prop_insecticide)
p2 <- p # & theme(legend.position = "bottom")

p <- plot_type20(plot_this, x_var = "prevalence", y_var = "Incidence", this_prop_insecticide) +
  coord_cartesian(ylim = c(0, 500))
p3 <- p # & theme(legend.position = "bottom")

p <- plot_type20(plot_this, x_var = "prevalence", y_var = "Incidence_C", this_prop_insecticide) +
  coord_cartesian(ylim = c(0, 1000))
p3B <- p # & theme(legend.position = "bottom")

p <- plot_type20(plot_this, x_var = "prevalence", y_var = "Incidence_P", this_prop_insecticide) +
  coord_cartesian(ylim = c(0, 200))
p3C <- p # & theme(legend.position = "bottom")

p <- plot_type20(plot_this, x_var = "prevalence", y_var = "Rres_final", this_prop_insecticide) +
  geom_hline(yintercept = 1, linetype = "dashed")
p4 <- p # & theme(legend.position = "bottom")

p <- plot_type20(plot_this, x_var = "prevalence", y_var = "number_waning_from_PI", this_prop_insecticide) +
  coord_cartesian(ylim = c(0, 60))
p5 <- p # & theme(legend.position = "bottom")

p <- (p1 / p3 / p3B / p3C / p5) + plot_layout(guides = "collect", axes = "collect") & 
  theme(legend.position = "bottom", legend.justification = "left", legend.title = element_blank())
my_ggsave(plot = p, filename = paste0("output/ms_figs/4H_spare_figX4a_plot_type20_panel_compare_treatments_by_prev_ALT", 
              "_", this_prop_insecticide, ".pdf"), width = 7, height = 11)

p <- (p1 / p2 / p4) + plot_layout(guides = "collect", axes = "collect") & 
  theme(legend.position = "bottom", legend.justification = "left", legend.title = element_blank())
my_ggsave(plot = p, filename = paste0("output/ms_figs/4H_spare_figX4b_plot_type20_panel_compare_treatments_by_prev", 
              "_", this_prop_insecticide, ".pdf"), width = 7, height = 8.5)

################################################################################
pA <- plot_type21(plot_this, x_var = "prevalence", y_var = "No_trt_cat")
pA <- pA & theme(legend.position = "bottom")
my_ggsave(plot = pA, filename = "output/ms_figs/4H_spare_plot_type21_No_trt_versus_prev_facet_prop_insect_NW.pdf", width = 6, height = 9)

pB <- plot_type21(plot_this, x_var = "prevalence", y_var = "Rres_final")
pB <- pB & theme(legend.position = "bottom")
my_ggsave(plot = pB, filename = "output/ms_figs/4H_spare_plot_type21_Rres_versus_prev_facet_prop_insect_NW.pdf", width = 6, height = 9)

pC <- plot_type21(plot_this, x_var = "prevalence", y_var = "RiskA")
pC <- pC & theme(legend.position = "bottom")
my_ggsave(plot = pC, filename = "output/ms_figs/4H_spare_plot_type21_RiskA_versus_prev_facet_prop_insect_NW.pdf", width = 6, height = 9)

combined <- pC + pB + plot_layout(guides = 'collect') & theme(legend.position = "bottom")
my_ggsave(plot = combined, filename = "output/ms_figs/4H_spare_plot_type21_combined_versus_prev_facet_prop_insect_NW.pdf", width = 13, height = 9.5)

################################################################################



