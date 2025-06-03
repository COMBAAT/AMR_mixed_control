# Load required packages -------------------------------------------------------

library(dplyr)
library(gghighlight)
library(ggplot2)
library(patchwork)

# Source files and function
source("funcs/plot_helper.R")
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

# select quick treatment (1), responsive treatment with prophylactic drug (2), ongoing prophylactic treatment (3)
ttype = 1
subset <- create_data_subsets(saved_simulations, ttype)

# subset further by scenario if additional parameters varied, default is first row
scenario_choice <- show_scenarios(scenarios_df)
scenario_choice
use_cc <- FALSE
mainvecpop <- FALSE
spec <- paste0(use_cc, "_", mainvecpop)
subset_for_plotting <- select_scenario(scenario_choice, subset, use_cc, mainvecpop)

# adjust fitness post simulation, if desired
subset_for_plotting <- adjust_fitness(subset_for_plotting, fit_adj_new = 0.8)
subset_for_plotting <- add_competition_and_invasion_columns(subset_for_plotting)

# Specify K and NW for plotting
if (use_cc == TRUE) {
  subset_for_plotting <- subset_for_plotting #%>% mutate(Baseline_vector_population = equil_vector_pop_baseline) 
  this_vector_measure <- "Baseline_vector_population"
  this_vector_measure_value <- 3000
} else {
  subset_for_plotting <- subset_for_plotting #%>% mutate(Baseline_vector_host_ratio = equil_vector_pop_baseline / hosts) 
  #this_K_host_ratio <- 30
  this_vector_measure_value <- 15
  this_vector_measure <- "Baseline_vector_host_ratio"
}
this_NW <- 100
this_NW_set <- c(0, 100, 300)
subset_for_plotting_reduced_insecticide <- subset_for_plotting %>% filter(prop_cattle_with_insecticide %in% seq(0, 0.5, 0.1))




# Generate plots ---------------------------------------------------------------
# Identify the boundary where R0 closest to 1
subset_for_invasion_plots <- subset_for_plotting %>%
  group_by(treat_prop, NW, K, use_carrying_capacity, maintain_vector_pop) %>%
  mutate(R0sen_gt_1 = ifelse(R0sen_final > 1, "R0sen > 1", "R0sen < 1")) %>%
  mutate(R0sen_temp = case_when(R0sen_final < 1 ~ 0, TRUE ~ R0sen_final)) %>%
  mutate(closest_to_1_location = which.min(abs(R0sen_temp - 1)),
         closest_to_1_value = prop_cattle_with_insecticide[closest_to_1_location]) %>% ungroup()

subset_for_invasion_plots <- subset_for_invasion_plots %>%
  mutate(closest_true_false = ifelse(closest_to_1_value == prop_cattle_with_insecticide, TRUE, FALSE
  ))

# Plot the results
restricted_subset <- subset_for_invasion_plots #%>% filter(R0sen > 0, ratio > 1)
plot_invasion_landscape(1, restricted_subset)
#restricted_subset <- subset_for_invasion_plots %>% filter(R0sen > 0, ratio > 1)
#plot_invasion_landscape(1, restricted_subset)
#restricted_subset <- subset_for_invasion_plots %>% filter(prevalence > 0)
plot_other_landscape(restricted_subset, "prevalence")
plot_other_landscape(restricted_subset, "ratio")
plot_other_landscape(restricted_subset, "RiskA")





# Plot and save baseline parameters
output_label <- "00_baseline_parameters"
p <- plot_baseline_parameters(baseline_parameters)
output_filename <- paste0(folder_name, output_label, ".pdf")
ggsave(
  filename = output_filename,
  width = my_pdfwidth(), height = my_pdfheight()
)
write.csv(baseline_parameters, file = paste0(folder_name, output_label, ".csv"))

# Plot and save scenarios
output_label <- "00_scenarios"
p <- plot_scenarios(scenarios_df)
output_filename <- paste0(folder_name, output_label, ".pdf")
ggsave(p,
  filename = output_filename,
  width = my_pdfwidth(), height = 1.5 * my_pdfheight()
)
scenarios_for_output <- get_simplified_scenarios(scenarios_df)
write.csv(scenarios_for_output, file = paste0(folder_name, output_label, ".csv"))

# Plot R0 versus wildlife faceted by treat_prop
df <- subset_for_plotting
#df <- df %>% mutate(colour_var = .data[[this_vector_measure]]) 
df$colour_var <- df[, this_vector_measure]
df %>%
  mutate_at(c("prop_cattle_with_insecticide", "treat_prop", this_vector_measure, "colour_var"), as.factor) %>%
  filter(prop_cattle_with_insecticide == 0, treat_prop %in% c(0, 0.6, 0.95)) %>%
  ggplot(aes(NW, R0sen, colour = colour_var)) +
  geom_point(size = my_pointsize()) +
  geom_line(linewidth = my_linewidth()) +
  facet_wrap(~treat_prop) +
  xlab(my_label("NW")) +
  ylab(my_label("R0sen")) +
  labs(colour = my_label(this_vector_measure)) +
  my_theme()

output_label <- "plot_type0_R0sen"
output_filename <- paste0(folder_name, output_label, "_ttype", ttype, "_spec_", spec, ".pdf")
# ggsave(
#   filename = output_filename,
#   width = my_pdfwidth(), height = my_pdfheight()
# )

# ----------------------------------------
# Plot R resistant/R sensitive versus wildlife faceted by treat_prop
df <- subset_for_plotting
df$shape_variable <- df[, this_vector_measure]
lhs <- df %>%
  mutate_at(c("prop_cattle_with_insecticide", "NW", this_vector_measure, "shape_variable"), as.factor) %>%
  filter(prop_cattle_with_insecticide == 0.0) %>%
  ggplot(aes(treat_prop, ratio, colour = NW, shape = shape_variable)) +
  geom_point(size = my_pointsize()) +
  geom_line(linewidth = my_linewidth()) +
  xlab(my_label("treat_prop")) +
  ylab(my_label("ratio")) +
  labs(colour = my_label("NW"), shape = my_label(this_vector_measure)) +
  my_theme()

rhs <- lhs + ylim(c(0, 2)) + 
  geom_abline(intercept = 1.0, slope = 0, linetype = "dashed")
rhs

# use patchwork package to stick plots together
# use guides = collect to remove duplicate legends
lhs + rhs + plot_layout(ncol = 2, guides = "collect", axis_titles = "collect")

output_label <- paste0("plot_type0_Rres_Rsen_ratio")
output_filename <- paste0(folder_name, output_label, "_ttype", ttype, "_spec_", spec, ".pdf")
# ggsave(
#   filename = output_filename,
#   width = my_pdfwidth(), height = my_pdfheight()
# )

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
  plot_type1_y_versus_treat_prop_facet_NW(subset_for_plotting, y_var, this_NW_set, this_vector_measure)

  output_label <- paste0("plot_type1_", y_var)
  output_filename <- paste0(folder_name, output_label, "_ttype", ttype, "_spec_", spec, ".pdf")
  ggsave(
    filename = output_filename,
    width = my_pdfwidth(), height = my_pdfheight()
  )
}
# ----------------------------------------

for (y_var in y_vars) {
  plot_type2_y_versus_treat_prop_facet_prop_cattle_with_insecticide(subset_for_plotting_reduced_insecticide, y_var, this_NW_set, 
                                                                    this_vector_measure, this_vector_measure_value)
  output_label <- paste0("plot_type2_", y_var)
  output_filename <- paste0(folder_name, output_label, "_ttype", ttype, "_spec_", spec, ".pdf")
  # ggsave(
  #   filename = output_filename,
  #   width = my_pdfwidth(), height = my_pdfheight()
  # )
}

# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by prop_cattle_with_insecticide with highlighting
y_var <- "RiskE"
threshold_var <- "prevalence"
threshold <- 0.1
plot_type3_y_versus_treat_prop_facet_prop_cattle_with_insecticide_with_higlight(
  subset_for_plotting_reduced_insecticide, y_var, threshold_var, threshold, this_NW_set, 
  this_vector_measure, this_vector_measure_value
)
output_label <- paste0("plot_type3_", y_var)
output_filename <- paste0(folder_name, output_label, "_ttype", ttype, "_spec_", spec, ".pdf")
# ggsave(
#   filename = output_filename,
#   width = my_pdfwidth(), height = my_pdfheight()
# )

# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by NW, coloured by prop_cattle_with_insecticide
#y_vars <- c("Incidence", "prevalence", "No_trt_cat", "RiskA", "RiskE", "Rsen_final", "Rres_final", "R0sen")

for (y_var in y_vars) {
  plot_type4_y_versus_treat_prop_facet_NW(subset_for_plotting_reduced_insecticide, y_var, this_NW_set, 
                                          this_vector_measure, this_vector_measure_value)

  output_label <- paste0("plot_type4_", y_var)
  output_filename <- paste0(folder_name, output_label, "_ttype", ttype, "_spec_", spec, ".pdf")
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
  plot_type5_y_versus_prop_cattle_with_insecticide_facet_NW(subset_for_plotting, y_var, this_NW_set, 
                                                              this_vector_measure, this_vector_measure_value)

  output_label <- paste0("plot_type5_", y_var)
  output_filename <- paste0(folder_name, output_label, "_ttype", ttype, "_spec_", spec, ".pdf")
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
ggsave(
  filename = output_filename,
  width = my_pdfwidth(), height = my_pdfheight()
)

# ----------------------------------------
