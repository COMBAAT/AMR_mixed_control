source("funcs/plot_helper.R")
source("funcs/compare_responsive_and_ongoing_helper.R")

# combine df for each treatment type
df_all_ttype <- rbind(df_ttype1, df_ttype2, df_ttype3)
this_NW <- 100

# create dataframe for plotting
plot_this <- df_all_ttype %>%
  filter(
    maintain_vector_pop == F,
    Baseline_vector_host_ratio %in% c(this_vector_measure_value)
  ) %>%
  mutate(prop_cattle_with_insecticide = as.factor(prop_cattle_with_insecticide),
         Risk_per_treatment = case_when(No_trt_cat > 0 ~ RiskA/No_trt_cat, T ~ NA))

p <- plot_type20(plot_this, x_var = "prevalence", y_var = "No_trt_cat")
p1 <- p # & theme(legend.position = "bottom")

p <- plot_type20(plot_this, x_var = "prevalence", y_var = "RiskA")
p2 <- p # & theme(legend.position = "bottom")

p <- plot_type20(plot_this, x_var = "prevalence", y_var = "Risk_per_treatment")
p3 <- p # & theme(legend.position = "bottom")

p <- plot_type20(plot_this, x_var = "prevalence", y_var = "Rres_final")
p4 <- p # & theme(legend.position = "bottom")

p <- (p1 / p2 / p3 / p4) + plot_layout(guides = "collect", axes = "collect") & 
  theme(legend.position = "bottom", legend.justification = "left", legend.title = element_blank())
ggsave("output/ms_figs/use_as_fig3_plot_type20_panel_compare_treatments_by_prev.pdf", p, width = 7, height = 11)

################################################################################
p <- plot_type21(plot_this, x_var = "prevalence", y_var = "No_trt_cat")
p <- p & theme(legend.position = "bottom")
ggsave("output/ms_figs/plot_type21_No_trt_versus_prev_facet_prop_insect_NW.pdf", p, width = 6, height = 9)

p <- plot_type21(plot_this, x_var = "prevalence", y_var = "RiskA")
p <- p & theme(legend.position = "bottom")
ggsave("output/ms_figs/plot_type21_RiskA_versus_prev_facet_prop_insect_NW.pdf", p, width = 6, height = 9)

################################################################################


################################
this_vector_measure <- "Baseline_vector_host_ratio"

get_subset <- function(df_ttype1, df_ttype2, df_ttype3, ttype) {
  if (ttype == 1) {
    df <- df_ttype1
  }
  if (ttype == 2) {
    df <- df_ttype2
  }
  if (ttype == 3) {
    df <- df_ttype3
  }
  df
}
################################
for (plot_type in c(22, 23)) {
  plots_row <- list()
  y_vars <- c("R0sen", "prevalence", "Incidence", "No_trt_cat", "RiskA")
  y_vars <- c("R0sen", "prevalence", "No_trt_cat", "RiskA")
  n_vars <- length(y_vars)
  plots_row <- list()
  for (i in 1:n_vars) {
    y_var <- y_vars[i]
    plots_var <- list()
    for (ttype in 1:3) {
      plot_this <- get_subset(df_ttype1, df_ttype2, df_ttype3, ttype)
      if (plot_type == 22) {
        p <- plot_type22_y_versus_treat_prop_facet_treatment_type(plot_this, y_var,
          this_NW_set = this_NW,
          this_vector_measure = this_vector_measure, ttype
        )
      } else if (plot_type == 23) {
        p <- plot_type23_y_versus_treat_prop_facet_treatment_type(plot_this, y_var,
          this_vector_measure,
          this_vector_measure_value,
          this_NW_set,
          ttype
        )
      }
      p
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
  if (n_vars == 3) {
    p <- plots_row[[1]] / plots_row[[2]] / plots_row[[3]] +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
    plot_height <- 8.5
    p
  } else if (n_vars == 4) {
    p <- plots_row[[1]] / plots_row[[2]] / plots_row[[3]] / plots_row[[4]] +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
    plot_height <- 9
    p
  } else if (n_vars == 5) {
    p <- plots_row[[1]] / plots_row[[2]] / plots_row[[3]] / plots_row[[4]] / plots_row[[5]] +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
    plot_height <- 12
    p
  }
  if (plot_type == 23) {
    p <- p +
      plot_annotation(caption = paste0("VHR = ", this_vector_measure_value))
    plot_name <- paste0("output/ms_figs/use_as_fig2_plot_type", plot_type, "_panel_compare_treatment_types.pdf")
  }
  if (plot_type == 22) {
    p <- p +
      plot_annotation(caption = paste0("NW = ", this_NW))
  plot_name <- paste0("output/ms_figs/use_as_fig2alt_plot_type", plot_type, "_panel_compare_treatment_types.pdf")
  }
  ggsave(plot_name, p, width = 7.5, height = plot_height)
}
for (plot_type in c(22, 23)) {
  print(plot_type)
}
################################

