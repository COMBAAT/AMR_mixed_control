source("funcs/compare_responsive_and_ongoing_helper.R")

# combine df for each treatment type
df_all_ttype <- rbind(df_ttype1, df_ttype2, df_ttype3)

#create dataframe for plotting
plot_this <- df_all_ttype %>% filter(maintain_vector_pop == F,
                               Baseline_vector_host_ratio %in% c(30)) %>%
mutate(prop_cattle_with_insecticide = as.factor(prop_cattle_with_insecticide))

p <- plot_type20(plot_this, x_var = "prevalence", y_var = "No_trt_cat")
p <- p & theme(legend.position = "bottom") 
ggsave("output/ms_figs/plot_type20_No_trt_vers_prev_facet_NW.pdf", p, width = 7, height = 4.5)

p <- plot_type20(plot_this, x_var = "prevalence", y_var = "RiskA")
p <- p & theme(legend.position = "bottom") 
ggsave("output/ms_figs/plot_type20_RiskA_vers_prev_facet_NW.pdf", p, width = 7, height = 4.5)

################################################################################
p <- plot_type21(plot_this, x_var = "prevalence", y_var = "No_trt_cat")
p <- p & theme(legend.position = "bottom") 
ggsave("output/ms_figs/plot_type21_No_trt_versus_prev_facet_prop_insect_NW.pdf", p, width = 6, height = 9)

p <- plot_type21(plot_this, x_var = "prevalence", y_var = "RiskA")
p <- p & theme(legend.position = "bottom") 
ggsave("output/ms_figs/plot_type21_RiskA_versus_prev_facet_prop_insect_NW.pdf", p, width = 6, height = 9)

################################################################################

plot_this2 <- df_all_ttype %>% filter(maintain_vector_pop == F) %>%
  mutate(prop_cattle_with_insecticide = as.factor(prop_cattle_with_insecticide)) 

y_var = "prevalence"
this_vector_measure <- "Baseline_vector_host_ratio"

plot_this2_curative <- plot_this2 %>% filter(treatment_type == "curative")
p1 <- plot_type22_y_versus_treat_prop_facet_treatment_type(plot_this2_curative, y_var, this_NW_set = c(0), 
                                                          this_vector_measure = this_vector_measure,  ttype = 1)
p1 <- p1 + ggtitle("Curative \n")

plot_this2_longlasting <- plot_this2 %>% filter(treatment_type == "longlasting")
p2 <- plot_type22_y_versus_treat_prop_facet_treatment_type(plot_this2_longlasting, y_var, this_NW_set = c(0), 
                                                           this_vector_measure = this_vector_measure,  ttype = 2)
p2 <- p2 + ggtitle("Longlasting \n")

plot_this2_ongoing <- plot_this2 %>% filter(treatment_type == "proph_ongoing")
p3 <- plot_type22_y_versus_treat_prop_facet_treatment_type(plot_this2_ongoing, y_var, this_NW_set = c(0), 
                                                           this_vector_measure = this_vector_measure,  ttype = 3)
p3 <- p3 + ggtitle('Ongoing \n')

p123 <- p1 + p2 + p3
p_prev <- p123 + plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom") 

p1_prev <- p1
p2_prev <- p2
p3_prev <- p3

################################
y_var = "No_trt_cat"
this_vector_measure <- "Baseline_vector_host_ratio"

plot_this2_curative <- plot_this2 %>% filter(treatment_type == "curative")
p1 <- plot_type22_y_versus_treat_prop_facet_treatment_type(plot_this2_curative, y_var, this_NW_set = c(0), 
                                                           this_vector_measure = this_vector_measure,  ttype = 1)
p1 <- p1 #+ ggtitle("Curative")

plot_this2_longlasting <- plot_this2 %>% filter(treatment_type == "longlasting")
p2 <- plot_type22_y_versus_treat_prop_facet_treatment_type(plot_this2_longlasting, y_var, this_NW_set = c(0), 
                                                           this_vector_measure = this_vector_measure,  ttype = 2)
p2 <- p2 #+ ggtitle("Longlasting")

plot_this2_ongoing <- plot_this2 %>% filter(treatment_type == "proph_ongoing")
p3 <- plot_type22_y_versus_treat_prop_facet_treatment_type(plot_this2_ongoing, y_var, this_NW_set = c(0), 
                                                           this_vector_measure = this_vector_measure,  ttype = 3)
p3 <- p3 #+ ggtitle('Ongoing')

p123 <- p1 + p2 + p3
p_No_trt_cat <- p123 + plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom") 
p_No_trt_cat

p1_No_trt_cat <- p1
p2_No_trt_cat <- p2
p3_No_trt_cat <- p3

################################
y_var = "RiskA"
this_vector_measure <- "Baseline_vector_host_ratio"

plot_this2_curative <- plot_this2 %>% filter(treatment_type == "curative")
p1 <- plot_type22_y_versus_treat_prop_facet_treatment_type(plot_this2_curative, y_var, this_NW_set = c(0), 
                                                           this_vector_measure = this_vector_measure,  ttype = 1)
p1 <- p1 #+ ggtitle("Curative /n")

plot_this2_longlasting <- plot_this2 %>% filter(treatment_type == "longlasting")
p2 <- plot_type22_y_versus_treat_prop_facet_treatment_type(plot_this2_longlasting, y_var, this_NW_set = c(0), 
                                                           this_vector_measure = this_vector_measure,  ttype = 2)
p2 <- p2 #+ ggtitle("Longlasting /n")

plot_this2_ongoing <- plot_this2 %>% filter(treatment_type == "proph_ongoing")
p3 <- plot_type22_y_versus_treat_prop_facet_treatment_type(plot_this2_ongoing, y_var, this_NW_set = c(0), 
                                                           this_vector_measure = this_vector_measure,  ttype = 3)
p3 <- p3 #+ ggtitle('Ongoing')

p123 <- p1 + p2 + p3
p_RiskA <- p123 + plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom") 
p_RiskA

p1_RiskA <- p1
p2_RiskA <- p2
p3_RiskA <- p3
################################
p_prev / p_No_trt_cat / p_RiskA + plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom") 


p_comb1 <- (p1_prev + p2_prev + p3_prev) /
  (p1_No_trt_cat + p2_No_trt_cat + p3_No_trt_cat) /
  (p1_RiskA + p2_RiskA + p3_RiskA) + plot_layout(guides = 'collect', axes = 'collect') & 
  theme(legend.position = "bottom") 

p_comb2 <- (p1_prev + p2_prev + p3_prev + plot_layout(axes = 'collect')) /
  (p1_No_trt_cat + p2_No_trt_cat + p3_No_trt_cat + plot_layout(axes = 'collect')) /
  (p1_RiskA + p2_RiskA + p3_RiskA + plot_layout(axes = 'collect')) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom") 

p_comb3 <- (p1_prev + p2_prev + plot_spacer() + p3_prev + plot_layout(axes = 'collect', widths = c(1, 1, 0.1, 1))) /
  (p1_No_trt_cat + p2_No_trt_cat + plot_spacer() + p3_No_trt_cat + plot_layout(axes = 'collect', widths = c(1, 1, 0.1, 1))) /
  (p1_RiskA + p2_RiskA +  plot_spacer() + p3_RiskA + plot_layout(axes = 'collect', widths = c(1, 1, 0.1, 1))) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom") 
p_comb3
ggsave("output/ms_figs/fig2_compare_treatment_types.pdf", p_comb3, width = 11, height = 13)
