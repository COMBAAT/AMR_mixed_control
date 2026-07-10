
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
plots_row <- list()
y_vars <- c("R0sen", "prevalence", "No_trt_cat", "RiskA", "Rres_final")
n_vars <- length(y_vars)
plots_row <- list()
for (i in 1:n_vars) {
  y_var <- y_vars[i]
  plots_var <- list()
  for (this_ttype in 1:3) {
    plot_this <- subset2 %>% filter(treatment_code == this_ttype)
    p <- plot_type22(plot_this, y_var,
                     this_NW_set = this_NW,
                     this_vector_measure = this_vector_measure, this_ttype)
    plots_var[[this_ttype]] <- p
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
plot_name <- paste0("output/ms_figs/4B_supp_mat_figS1_outcomes_stratified_by_vector_host_ratio.pdf")
my_ggsave(plot = p, filename = plot_name, width = 7.5, height = plot_height)

################################
