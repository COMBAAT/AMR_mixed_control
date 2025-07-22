# Load required packages -------------------------------------------------------

library(dplyr)
library(gghighlight)
library(ggplot2)
library(patchwork)

# Source files and function
source("funcs/plot_helper.R")
source("plot_helper_extra.R")
source("funcs/helper_functions.R")
source("funcs/output_baseline_params_and_scenarios.R")
source("funcs/epi_outputs.R")

# Load data files --------------------------------------------------------------
load_latest_file <- TRUE
if (load_latest_file == TRUE) {
  latest_file <- get_latest_Rda_file()
  load(latest_file)
  folder_name <- gsub(".Rda", "/", latest_file)
  dir.create(folder_name)
} else {
  load("output/June2curative_longlasting.Rda")
  folder_name <- "output/June2curative_longlasting/"
  dir.create(folder_name)
}

# Create data subsets --------------------------------------------------------------
# select quick treatment (1), responsive treatment with prophylactic drug (2), ongoing prophylactic treatment (3)
ttype = 3
subset <- create_data_subsets(saved_simulations, ttype)

# subset further by scenario if additional parameters varied, default is first row
scenario_choice <- show_scenarios(scenarios_df)
scenario_choice
use_cc <- FALSE
mainvecpop <- F
spec <- paste0(use_cc, "_", mainvecpop)
subset_for_plotting <- select_scenario(scenario_choice, subset, use_cc, mainvecpop)

# adjust fitness post simulation, if desired
subset_for_plotting <- adjust_fitness(subset_for_plotting, fit_adj_new = 0.8)
subset_for_plotting <- add_competition_and_invasion_columns(subset_for_plotting)

if (ttype == 1) {df_ttype1 <- subset_for_plotting}
if (ttype == 2) {df_ttype2 <- subset_for_plotting}
if (ttype == 3) {df_ttype3 <- subset_for_plotting}

# Specify K and NW for plotting
this_NW <- 100
this_NW_set <- c(0, 100, 200)

if (use_cc == TRUE) {
  subset_for_plotting <- subset_for_plotting #%>% mutate(Baseline_vector_population = equil_vector_pop_baseline) 
  this_vector_measure <- "Baseline_vector_population"
  this_vector_measure_value <- 3000
} else {
  subset_for_plotting <- subset_for_plotting #%>% mutate(Baseline_vector_host_ratio = equil_vector_pop_baseline / hosts) 
  #this_K_host_ratio <- 30
  this_vector_measure_value <- 20
  this_vector_measure <- "Baseline_vector_host_ratio"
}

subset_for_plotting_reduced_insecticide <- subset_for_plotting %>% filter(prop_cattle_with_insecticide %in% seq(0, 0.5, 0.1))

# For invasion plots, Identify the boundary where R0 closest to 1
subset_for_invasion_plots <- subset_for_plotting %>%
  group_by(treat_prop, NW, K, use_carrying_capacity, maintain_vector_pop) %>%
  mutate(R0sen_gt_1 = ifelse(R0sen_final > 1, "R0sen > 1", "R0sen < 1")) %>%
  mutate(R0sen_temp = case_when(R0sen_final < 1 ~ 0, TRUE ~ R0sen_final)) %>%
  mutate(closest_to_1_location = which.min(abs(R0sen_temp - 1)),
         closest_to_1_value = prop_cattle_with_insecticide[closest_to_1_location]) %>% ungroup()

subset_for_invasion_plots <- subset_for_invasion_plots %>%
  mutate(closest_true_false = ifelse(closest_to_1_value == prop_cattle_with_insecticide, TRUE, FALSE
  ))

# -----------------------------------------------------------------------------
# Create a list to store all the plots for post processing
plots <- list()
# -----------------------------------------------------------------------------

# Generate plots ---------------------------------------------------------------

# Plot and save baseline parameters
output_label <- "00_baseline_parameters"
p <- plot_baseline_parameters(baseline_parameters)
output_filename <- paste0(folder_name, output_label, ".pdf")
plot_label <- paste0(output_label, "_ttype", ttype, "_spec_", spec)
plots[[plot_label]] <- p
ggsave(
  filename = output_filename,
  width = my_pdfwidth(), height = my_pdfheight()
)
write.csv(baseline_parameters, file = paste0(folder_name, output_label, ".csv"))

# Plot and save scenarios
output_label <- "00_scenarios"
p <- plot_scenarios(scenarios_df)
output_filename <- paste0(folder_name, output_label, ".pdf")
plot_label <- paste0(output_label, "_ttype", ttype, "_spec_", spec)
plots[[plot_label]] <- p
ggsave(p,
  filename = output_filename,
  width = my_pdfwidth(), height = 1.5 * my_pdfheight()
)
scenarios_for_output <- get_simplified_scenarios(scenarios_df)
write.csv(scenarios_for_output, file = paste0(folder_name, output_label, ".csv"))

# ----------------------------------------
# Plot R resistant/R sensitive versus wildlife faceted by treat_prop
p <- plot_type0_ratio(subset_for_plotting, this_vector_measure, ttype)

output_label <- paste0("plot_type0_Rres_Rsen_ratio")
output_filename <- paste0(folder_name, output_label, "_ttype", ttype, "_spec_", spec, ".pdf")
plot_label <- paste0(output_label, "_ttype", ttype, "_spec_", spec)
plots[[plot_label]] <- p
 ggsave(
   filename = output_filename,
   width = my_pdfwidth(), height = my_pdfheight()
 )

# ----------------------------------------
# Plot y versus_treat_prop faceted by NW
y_vars <- c(
  "prevalence", "Incidence","R0sen", "Rsen_final", "Rres_final", "ratio", "No_trt_cat", "Prob_onward_tran",
  "RiskE", "RiskA"
)
y_vars <- c(
  "prevalence", "Incidence", "No_trt_cat", "RiskA", "RiskE", "R0sen"
)

for (y_var in y_vars) {
  p <- plot_type1_y_versus_treat_prop_facet_NW(subset_for_plotting, y_var, 
                                               this_NW_set, this_vector_measure, ttype)
  p
  output_label <- paste0("plot_type1_", y_var)
  output_filename <- paste0(folder_name, output_label, "_ttype", ttype, "_spec_", spec, ".pdf")
  plot_label <- paste0(output_label, "_ttype", ttype, "_spec_", spec)
  plots[[plot_label]] <- p
  ggsave(
    filename = output_filename,
    width = my_pdfwidth(), height = my_pdfheight()
  )
}
# ----------------------------------------

for (y_var in y_vars) {
  p <- plot_type2_y_versus_treat_prop_facet_prop_cattle_with_insecticide(subset_for_plotting_reduced_insecticide, y_var, this_NW_set, 
                                                                    this_vector_measure, this_vector_measure_value, ttype)
  p
  output_label <- paste0("plot_type2_", y_var)
  output_filename <- paste0(folder_name, output_label, "_ttype", ttype, "_spec_", spec, ".pdf")
  plot_label <- paste0(output_label, "_ttype", ttype, "_spec_", spec)
  plots[[plot_label]] <- p
  ggsave(
    filename = output_filename,
    width = my_pdfwidth(), height = my_pdfheight()
  )
}

# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by prop_cattle_with_insecticide with highlighting
y_var <- "RiskE"
threshold_var <- "prevalence"
threshold <- 0.1
p <- plot_type3_y_versus_treat_prop_facet_prop_cattle_with_insecticide_with_higlight(
  subset_for_plotting_reduced_insecticide, y_var, threshold_var, threshold, this_NW_set, 
  this_vector_measure, this_vector_measure_value, ttype
)
p
output_label <- paste0("plot_type3_", y_var)
output_filename <- paste0(folder_name, output_label, "_ttype", ttype, "_spec_", spec, ".pdf")
plot_label <- paste0(output_label, "_ttype", ttype, "_spec_", spec)
plots[[plot_label]] <- p
ggsave(
  filename = output_filename,
  width = my_pdfwidth(), height = my_pdfheight()
)

# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by NW, coloured by prop_cattle_with_insecticide
#y_vars <- c("Incidence", "prevalence", "No_trt_cat", "RiskA", "RiskE", "Rsen_final", "Rres_final", "R0sen")

for (y_var in y_vars) {
  p <- plot_type4_y_versus_treat_prop_facet_NW(subset_for_plotting_reduced_insecticide, y_var, this_NW_set, 
                                          this_vector_measure, this_vector_measure_value, ttype)
  p
  output_label <- paste0("plot_type4_", y_var)
  output_filename <- paste0(folder_name, output_label, "_ttype", ttype, "_spec_", spec, ".pdf")
  plot_label <- paste0(output_label, "_ttype", ttype, "_spec_", spec)
  plots[[plot_label]] <- p
  ggsave(
    filename = output_filename,
    width = my_pdfwidth(), height = my_pdfheight()
  )
}
# ----------------------------------------

# ----------------------------------------
# Plot y versus prop_cattle_with_insecticide faceted by NW, coloured by treat_prop
#y_vars <- c("Incidence", "prevalence", "No_trt_cat", "RiskE", "RiskA", "Rsen_final", "Rres_final")

for (y_var in y_vars) {
  if (ttype == 3) {
    plot_type5 <- plot_type5_y_versus_prop_cattle_with_insecticide_facet_NW_ttype3
  } else {
    plot_type5 <- plot_type5_y_versus_prop_cattle_with_insecticide_facet_NW
  }
  p <- plot_type5(subset_for_plotting, y_var, this_NW_set, this_vector_measure, this_vector_measure_value)

  p
  output_label <- paste0("plot_type5_", y_var)
  output_filename <- paste0(folder_name, output_label, "_ttype", ttype, "_spec_", spec, ".pdf")
  plot_label <- paste0(output_label, "_ttype", ttype, "_spec_", spec)
  plots[[plot_label]] <- p
  ggsave(
    filename = output_filename,
    width = my_pdfwidth(), height = my_pdfheight()
  )
}

# ----------------------------------------

# ----------------------------------------
plot_type10_R0sen_versus_Rsen(subset_for_plotting)
output_label <- "plot_type10_R0sen_versus_Rsen"
output_filename <- paste0(folder_name, output_label, "_ttype", ttype, "_spec_", spec, ".pdf")
plot_label <- paste0(output_label, "_ttype", ttype, "_spec_", spec)
plots[[plot_label]] <- p
ggsave(
  filename = output_filename,
  width = my_pdfwidth(), height = my_pdfheight()
)


# Invasion plots
restricted_subset <- subset_for_invasion_plots #%>% filter(R0sen > 0, ratio > 1)
plot_invasion_landscape(1, restricted_subset, ttype)
#restricted_subset <- subset_for_invasion_plots %>% filter(R0sen > 0, ratio > 1)
#plot_invasion_landscape(1, restricted_subset)
#restricted_subset <- subset_for_invasion_plots %>% filter(prevalence > 0)
plot_other_landscape(restricted_subset, "prevalence", ttype)
plot_other_landscape(restricted_subset, "ratio", ttype)
plot_other_landscape(restricted_subset, "RiskA", ttype)

# Extra plot to show relationship bewteen treatment frequency and prophylactic coverage
if (ttype == 3) {
  p <- subset_for_plotting %>% filter(prop_cattle_with_insecticide == 0) %>%
    mutate(NW = as.factor(NW)) %>%
    ggplot() +
    geom_point(aes(x = treatments_per_year, y = coverage, 
                   colour = NW), size = my_pointsize()) +
    geom_line(aes(x = treatments_per_year, y = coverage, 
                   colour = NW), linewidth = my_linewidth()) +
    ylab(my_label("coverage")) +
    xlab(my_label("treatments_per_year")) +
    labs(colour = my_label("NW")) +
    my_theme()
  ggsave("output/ms_figs/coverage_vs_frequency.pdf", p)
}

# ----------------------------------------
if (ttype == 1 & mainvecpop == TRUE) {plots1T = plots}
if (ttype == 1 & mainvecpop == FALSE) {plots1F = plots}
if (ttype == 2 & mainvecpop == TRUE) {plots2T = plots}
if (ttype == 2 & mainvecpop == FALSE) {plots2F = plots}
if (ttype == 3 & mainvecpop == TRUE) {plots3T = plots}
if (ttype == 3 & mainvecpop == FALSE) {plots3F = plots}
