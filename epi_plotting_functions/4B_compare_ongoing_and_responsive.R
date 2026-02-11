rm(list = ls()[!grepl("^(plot|df|this_NW_set|this_vector_measure)", ls())])
source("funcs/plot_helper.R")
source("funcs/compare_responsive_and_ongoing_helper.R")

# combine df for each treatment type
df_all_ttype <- rbind(df_ttype1_F, df_ttype2_F, df_ttype3_F,
                      df_ttype1_T, df_ttype2_T, df_ttype3_T)
this_NW <- 100
this_prop_insecticide = 0.0

df_all_ttype <- df_all_ttype #%>% 
  #mutate(RiskA = PIs_final + PPs_final + CTs_final + PTs_final)

# create dataframe for plotting
plot_this <- df_all_ttype %>%
  filter(
    maintain_vector_pop == F,
    Baseline_vector_host_ratio %in% c(this_vector_measure_value)
  ) %>%
  mutate(prop_cattle_with_insecticide = as.factor(prop_cattle_with_insecticide),
         Risk_per_treatment = case_when(No_trt_cat > 0 ~ RiskA/No_trt_cat, T ~ NA))

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

p <- plot_type20(plot_this, x_var = "prevalence", y_var = "waning_from_PI", this_prop_insecticide) +
  coord_cartesian(ylim = c(0, 50))
p5 <- p # & theme(legend.position = "bottom")

p <- (p1 / p3 / p3B / p3C / p5) + plot_layout(guides = "collect", axes = "collect") & 
  theme(legend.position = "bottom", legend.justification = "left", legend.title = element_blank())
my_ggsave(plot = p, filename = paste0("output/ms_figs/4B_use_as_figX4a_plot_type20_panel_compare_treatments_by_prev_ALT", 
              "_", this_prop_insecticide, ".pdf"), width = 7, height = 11)

p <- (p1 / p2 / p4) + plot_layout(guides = "collect", axes = "collect") & 
  theme(legend.position = "bottom", legend.justification = "left", legend.title = element_blank())
my_ggsave(plot = p, filename = paste0("output/ms_figs/4B_use_as_figX4b_plot_type20_panel_compare_treatments_by_prev", 
              "_", this_prop_insecticide, ".pdf"), width = 7, height = 8.5)

################################################################################
pA <- plot_type21(plot_this, x_var = "prevalence", y_var = "No_trt_cat")
pA <- pA & theme(legend.position = "bottom")
my_ggsave(plot = pA, filename = "output/ms_figs/4B_plot_type21_No_trt_versus_prev_facet_prop_insect_NW.pdf", width = 6, height = 9)

pB <- plot_type21(plot_this, x_var = "prevalence", y_var = "Rres_final")
pB <- pB & theme(legend.position = "bottom")
my_ggsave(plot = pB, filename = "output/ms_figs/4B_plot_type21_Rres_versus_prev_facet_prop_insect_NW.pdf", width = 6, height = 9)

pC <- plot_type21(plot_this, x_var = "prevalence", y_var = "RiskA")
pC <- pC & theme(legend.position = "bottom")
my_ggsave(plot = pC, filename = "output/ms_figs/4B_plot_type21_RiskA_versus_prev_facet_prop_insect_NW.pdf", width = 6, height = 9)

combined <- pC + pB + plot_layout(guides = 'collect') & theme(legend.position = "bottom")
my_ggsave(plot = combined, filename = "output/ms_figs/4B_plot_type21_combined_versus_prev_facet_prop_insect_NW.pdf", width = 12, height = 9)

################################################################################


################################
this_vector_measure <- "Baseline_vector_host_ratio"

get_subset <- function(df_ttype1, df_ttype2, df_ttype3, ttype) {
  if (ttype == 1) {
    df <- df_ttype1_F
  }
  if (ttype == 2) {
    df <- df_ttype2_F
  }
  if (ttype == 3) {
    df <- df_ttype3_F
  }
  df
}
################################
  plots_row <- list()
  y_vars <- c("R0sen", "prevalence", "No_trt_cat", "RiskA", "Rres_final")
  n_vars <- length(y_vars)
  plots_row <- list()
  for (i in 1:n_vars) {
    y_var <- y_vars[i]
    plots_var <- list()
    for (ttype in 1:3) {
      plot_this <- get_subset(df_ttype1_F, df_ttype2_F, df_ttype3_F, ttype)
        p <- plot_type22(plot_this, y_var,
          this_NW_set = this_NW,
          this_vector_measure = this_vector_measure, ttype)
      plots_var[[ttype]] <- p
    }
    if (i == 1) {
      p1 <- plots_var[[1]] + ggtitle(my_title("curative"))
      p2 <- plots_var[[2]] + ggtitle(my_title("longlasting"))
      p3 <- plots_var[[3]] + ggtitle(my_title("ongoing"))
    } else {
      p1 <- plots_var[[1]]
      p2 <- plots_var[[2]]
      p3 <- plots_var[[3]]
    }
    plots_row[[i]] <- (p1 + p2 + plot_spacer() + p3 + plot_layout(axes = "collect", widths = c(1, 1, 0.1, 1)))
  }
    p <- plots_row[[1]] / plots_row[[2]] / plots_row[[3]] / plots_row[[4]] / plots_row[[5]] +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
    plot_height <- 12
    p <- p +
      plot_annotation(caption = paste0("NW = ", this_NW))
  plot_name <- paste0("output/ms_figs/4B_use_as_figX1_plot_type22_panel_compare_treatment_protocols.pdf")
  my_ggsave(plot = p, filename = plot_name, width = 7.5, height = plot_height)

################################


