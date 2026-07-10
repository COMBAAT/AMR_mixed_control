
source("funcs/plot_helper.R")
source("funcs/compare_responsive_and_ongoing_helper.R")

# Plotting uses saved_simulations and this_vector_measure; plot functions read in but do not use this_NW_set
desired_treatments_per_year <- round(seq(0, 9, by = 0.5), 3)
df_filtered_by_treatments_per_year <- saved_simulations %>%
  filter( round(treatments_per_year, 3) %in% desired_treatments_per_year)
################################
plots_row <- list()
y_vars <- c("R0sen", "prevalence", "Incidence")
n_vars <- length(y_vars)
plots_row <- list()
for (i in 1:n_vars) {
  y_var <- y_vars[i]
  plots_var <- list()
  for (this_ttype in 1:3) {
    plot_this <- df_filtered_by_treatments_per_year %>% filter(treatment_code == this_ttype)
    p <- plot_type23(
      plot_this, y_var,
      this_vector_measure, 
      this_vector_measure_value,
      this_NW_set,
      this_ttype
    )
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

p <- plots_row[[1]] / plots_row[[2]] / plots_row[[3]] +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
plot_height <- 8.5
plot_name <- paste0("output/ms_figs/4A_main_fig2_outcomes_by_treatment_protocol_and_wildlife.pdf")
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
  for (this_ttype in 1:3) {
    plot_this <- df_filtered_by_treatments_per_year %>% filter(treatment_code == this_ttype)
    p <- plot_type23(
      plot_this, y_var,
      this_vector_measure,
      this_vector_measure_value,
      this_NW_set,
      this_ttype
    )
    if (y_var == "Rres_final") {
      p <- p + geom_hline(yintercept = 1.0, linetype = "dashed")
    }
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

p <- plots_row[[1]] / plots_row[[2]] / plots_row[[3]] +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
plot_height <- 8.5
plot_name <- paste0("output/ms_figs/4A_main_fig3_outcomes_by_treatment_protocol_and_wildlife.pdf")
my_ggsave(plot = p, filename = plot_name, width = 7.5, height = plot_height)
################################

plots_row <- list()
y_vars <- c("prevalence", "prevalence_wildlife", "prevalence_vectors")
n_vars <- length(y_vars)
plots_row <- list()
for (i in 1:n_vars) {
  y_var <- y_vars[i]
  plots_var <- list()
  for (this_ttype in 1:3) {
    plot_this <- df_filtered_by_treatments_per_year %>% filter(treatment_code == this_ttype)
    p <- plot_type23(
      plot_this, y_var,
      this_vector_measure,
      this_vector_measure_value,
      this_NW_set,
      this_ttype
    )
    if (y_var == "Rres_final") {
      p <- p + geom_hline(yintercept = 1.0, linetype = "dashed")
    }
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

p <- plots_row[[1]] / plots_row[[2]] / plots_row[[3]] +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
plot_height <- 8.5
plot_name <- paste0("output/ms_figs/4A_spare_additional_outcomes_by_treatment_protocol_and_wildlife.pdf")
my_ggsave(plot = p, filename = plot_name, width = 7.5, height = plot_height)
################################
