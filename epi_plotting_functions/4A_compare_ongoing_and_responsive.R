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




################################
plots_row <- list()
y_vars <- c("R0sen", "prevalence", "Incidence")
n_vars <- length(y_vars)
plots_row <- list()
for (i in 1:n_vars) {
  y_var <- y_vars[i]
  plots_var <- list()
  for (ttype in 1:3) {
    plot_this <- get_subset(df_ttype1_F, df_ttype2_F, df_ttype3_F, ttype)
    p <- plot_type23(
      plot_this, y_var,
      this_vector_measure,
      this_vector_measure_value,
      this_NW_set,
      ttype
    )
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

p <- plots_row[[1]] / plots_row[[2]] / plots_row[[3]] +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
plot_height <- 8.5
plot_name <- paste0("output/ms_figs/4A_use_as_fig2_plot_type", 23, "_panel_compare_treatment_protocols_NEW.pdf")
my_ggsave(plot = p, filename = plot_name, width = 7.5, height = plot_height)

################################

################################
plots_row <- list()
y_vars <- c("No_trt_cat", "RiskA", "Rres_final")
n_vars <- length(y_vars)
plots_row <- list()
for (i in 1:n_vars) {
  y_var <- y_vars[i]
  plots_var <- list()
  for (ttype in 1:3) {
    plot_this <- get_subset(df_ttype1_F, df_ttype2_F, df_ttype3_F, ttype)
    p <- plot_type23(
      plot_this, y_var,
      this_vector_measure,
      this_vector_measure_value,
      this_NW_set,
      ttype
    )
    if (y_var == "Rres_final") {
      p <- p + geom_hline(yintercept = 1.0, linetype = "dashed")
    }
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

p <- plots_row[[1]] / plots_row[[2]] / plots_row[[3]] +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
plot_height <- 8.5
plot_name <- paste0("output/ms_figs/4A_use_as_fig3_plot_type", 23, "_panel_compare_treatment_protocols_NEW2.pdf")
my_ggsave(plot = p, filename = plot_name, width = 7.5, height = plot_height)
################################

plots_row <- list()
y_vars <- c("prevalence", "prevalence_wildlife", "prevalence_vectors")
n_vars <- length(y_vars)
plots_row <- list()
for (i in 1:n_vars) {
  y_var <- y_vars[i]
  plots_var <- list()
  for (ttype in 1:3) {
    plot_this <- get_subset(df_ttype1_F, df_ttype2_F, df_ttype3_F, ttype)
    p <- plot_type23(
      plot_this, y_var,
      this_vector_measure,
      this_vector_measure_value,
      this_NW_set,
      ttype
    )
    if (y_var == "Rres_final") {
      p <- p + geom_hline(yintercept = 1.0, linetype = "dashed")
    }
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

p <- plots_row[[1]] / plots_row[[2]] / plots_row[[3]] +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
plot_height <- 8.5
plot_name <- paste0("output/ms_figs/4A_use_as_figX3_plot_type", 23, "_panel_compare_treatment_protocols_NEW3.pdf")
my_ggsave(plot = p, filename = plot_name, width = 7.5, height = plot_height)
################################
