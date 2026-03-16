# Load required packages -------------------------------------------------------

library(dplyr)
library(gghighlight)
library(ggplot2)
library(patchwork)
library(metR)

# Source files and function
source("funcs/plot_helper.R")
source("funcs/helper_functions.R")
source("funcs/output_baseline_params_and_scenarios.R")
source("funcs/epi_outputs.R")

# Load data files --------------------------------------------------------------
load_latest_file <- FALSE
if (load_latest_file == TRUE) {
  latest_file <- get_latest_Rda_file()
  load(latest_file)
  folder_name <- gsub(".Rda", "/", latest_file)
  dir.create(folder_name)
} else {
  load("output/6Mar2026_test1.Rda")
  folder_name <- "output/test_scripts/"
  dir.create(folder_name)
}

# Set plotting defaults
this_NW_set <- c(0, 100, 200)
this_vector_measure_value <- 20
this_vector_measure <- "Baseline_vector_host_ratio"
                                                                                            
# Create data subsets --------------------------------------------------------------
# select quick treatment (1), responsive treatment with prophylactic drug (2), ongoing prophylactic treatment (3)
which_ttypes <- c(1:3)
which_mainvecpop <- c(T, F)
which_fit_adj <- c(0.6, 0.8, 0.95)
 for (this_ttype in which_ttypes) {
   for (mainvecpop in which_mainvecpop) {
     plots <- list()
     for (this_fit_adj in which_fit_adj) {

include_plot_type1 <- TRUE
include_plot_type2 <- TRUE
include_plot_type3 <- TRUE
include_plot_type4 <- TRUE
include_plot_type5 <- TRUE
include_plot_type6 <- TRUE
include_plot_type7 <- TRUE
include_plot_type8 <- TRUE
include_plot_type10 <- TRUE
include_plot_type15 <- TRUE
include_plot_type16 <- TRUE

subset <- saved_simulations %>% filter(treatment_code == this_ttype, maintain_vector_pop == mainvecpop)
# adjust fitness post simulation, if desired
subset <- adjust_fitness(subset, fit_adj_new = this_fit_adj)
subset <- add_competition_and_invasion_columns(subset)

# For invasion plots, Identify the boundary where R0 closest to 1
subset_for_invasion_plots <- subset %>%
  group_by(treat_prop, NW, K, use_carrying_capacity, maintain_vector_pop) %>%
  mutate(R0sen_gt_1 = case_when(R0sen_final > 1 ~ "R0sen > 1", 
                                R0sen_final < 1 & R0sen_final > 1e-06 ~ "R0sen < 1",
                                R0sen_final <= 1e-06 ~ "R0sen = 0"))

subset_for_plotting <- as.data.frame(subset_for_invasion_plots)

subset_for_plotting_reduced_insecticide <- subset_for_plotting %>% 
filter(!near(prop_cattle_with_insecticide, 0.05), !near(prop_cattle_with_insecticide, 0.15), 
       !near(prop_cattle_with_insecticide, 0.25), !near(prop_cattle_with_insecticide, 0.35),
       !near(prop_cattle_with_insecticide, 0.45), !near(prop_cattle_with_insecticide, 0.55), 
       !near(prop_cattle_with_insecticide, 0.65), !near(prop_cattle_with_insecticide, 0.75),
       !near(prop_cattle_with_insecticide, 0.85), !near(prop_cattle_with_insecticide, 0.95), 
       !near(prop_cattle_with_insecticide, 0.91), !near(prop_cattle_with_insecticide, 0.6)) 

# -----------------------------------------------------------------------------
# Create a list to store all the plots for post processing
#plots <- list()
# -----------------------------------------------------------------------------

# Generate plots ---------------------------------------------------------------
common_details <- paste0("_treatment_code", this_ttype, "_mainvecpop_", mainvecpop, "_vector_value_", this_vector_measure_value)
common_details_without_mainvecpop <- paste0("_treatment_code", this_ttype, "_vector_value_", this_vector_measure_value)
common_details_with_fitness <- paste0("_treatment_code", this_ttype, "_mainvecpop_", mainvecpop, "_vector_value_", this_vector_measure_value, "_fit_adj_", this_fit_adj)
common_details_with_fitness_without_mainvecpop <- paste0("_treatment_code", this_ttype, "_vector_value_", this_vector_measure_value, "_fit_adj_", this_fit_adj)


# Plot and save baseline parameters
plot_label <- "00_baseline_parameters"
p <- plot_baseline_parameters(baseline_parameters)
output_filename <- paste0(folder_name, plot_label, ".pdf")
plot_label <- paste0(plot_label, "_treatment_code", this_ttype)
plots[[plot_label]] <- p
my_ggsave(plot = p,
  filename = output_filename,
  width = my_pdfwidth(), height = my_pdfheight()
)
write.csv(baseline_parameters, file = paste0(folder_name, plot_label, ".csv"))

# Plot and save scenarios
plot_label <- "00_scenarios"
p <- plot_scenarios(scenarios_df)
output_filename <- paste0(folder_name, plot_label, ".pdf")
plot_label <- paste0(plot_label, "_treatment_code", this_ttype, "_mainvecpop_", mainvecpop)
plots[[plot_label]] <- p
my_ggsave(plot = p,
  filename = output_filename,
  width = my_pdfwidth(), height = 1.5 * my_pdfheight()
)
scenarios_for_output <- get_simplified_scenarios(scenarios_df)
write.csv(scenarios_for_output, file = paste0(folder_name, plot_label, ".csv"))

# ----------------------------------------
# Plot R resistant/R sensitive versus wildlife faceted by treat_prop
subset_for_plotting <- subset_for_plotting 
p <- plot_type0_ratio(subset_for_plotting, this_vector_measure, this_ttype)

plot_label <- paste0("plot_type0_Rres_Rsen_ratio", common_details_with_fitness_without_mainvecpop)
output_filename <- paste0(folder_name, plot_label, ".pdf")
plots[[plot_label]] <- p
 my_ggsave(plot = p,
   filename = output_filename,
   width = my_pdfwidth(), height = my_pdfheight()
 )

# ----------------------------------------
# Plot y versus_treat_prop faceted by NW
y_vars <- c(
  "prevalence", "Incidence", "No_trt_cat", "RiskA", "RiskE", "R0sen"
)

if (include_plot_type1 == TRUE) {
for (y_var in y_vars) {
  p <- plot_type1_y_versus_treat_prop_facet_NW(subset_for_plotting, y_var, 
                                               this_NW_set, this_vector_measure, this_ttype)
  plot_label <- paste0("plot_type1_", y_var, common_details_without_mainvecpop)
  output_filename <- paste0(folder_name, plot_label, ".pdf")
  plots[[plot_label]] <- p
  my_ggsave(plot = p,
    filename = output_filename,
    width = my_pdfwidth(), height = my_pdfheight()
  )
}
}
# ----------------------------------------
 if (include_plot_type2 == TRUE) {
for (y_var in y_vars) {
  p <- plot_type2_y_versus_treat_prop_facet_prop_cattle_with_insecticide(subset_for_plotting_reduced_insecticide, y_var, this_NW_set,
                                                                    this_vector_measure, this_vector_measure_value, this_ttype)
  plot_label <- paste0("plot_type2_", y_var, common_details)
  output_filename <- paste0(folder_name, plot_label, ".pdf")
  plots[[plot_label]] <- p
  my_ggsave(plot = p,
    filename = output_filename,
    width = my_pdfwidth(), height = my_pdfheight()
  )
}
 }

# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by prop_cattle_with_insecticide with highlighting
# y_var <- "RiskE"
 if (include_plot_type3 == TRUE) {
threshold_var <- "prevalence"
threshold <- 0.1
p <- plot_type3_y_versus_treat_prop_facet_prop_cattle_with_insecticide_with_higlight(
  subset_for_plotting_reduced_insecticide, y_var, threshold_var, threshold, this_NW_set,
  this_vector_measure, this_vector_measure_value, this_ttype
)
plot_label <- paste0("plot_type3_", y_var, common_details)
output_filename <- paste0(folder_name, plot_label, ".pdf")
plots[[plot_label]] <- p
my_ggsave(plot = p,
  filename = output_filename,
  width = my_pdfwidth(), height = my_pdfheight()
)
}
# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by NW, coloured by prop_cattle_with_insecticide
y_vars <- c("Incidence", "prevalence", "No_trt_cat", "RiskA", "Rres_final", "R0sen")

 if (include_plot_type4 == TRUE) {
for (y_var in y_vars) {
  p <- plot_type4_y_versus_treat_prop_facet_NW(subset_for_plotting_reduced_insecticide, y_var, this_NW_set, 
                                          this_vector_measure, this_vector_measure_value, this_ttype)
  if (y_var == "Rres_final") {
    p <- p + geom_abline(intercept = 1.0, slope = 0, linetype = "dashed")
  plot_label <- paste0("plot_type4_", y_var, common_details_with_fitness)
  } else {
    plot_label <- paste0("plot_type4_", y_var, common_details)
  }
  output_filename <- paste0(folder_name, plot_label, ".pdf")
  plots[[plot_label]] <- p
  my_ggsave(plot = p,
    filename = output_filename,
    width = my_pdfwidth(), height = my_pdfheight()
  )
}
 }
# ----------------------------------------
y_vars = c("Rres_final")
 if (include_plot_type6 == TRUE) {
for (y_var in y_vars) {
  p <- plot_type6_y_versus_treat_prop_facet_NW(subset_for_plotting_reduced_insecticide, y_var, this_NW_set, 
                                                  this_vector_measure, this_vector_measure_value, this_ttype)
plot_label <- paste0("plot_type6_", y_var, common_details_with_fitness)
output_filename <- paste0(folder_name, plot_label, ".pdf")
plots[[plot_label]] <- p
my_ggsave(plot = p,
  filename = output_filename,
  width = my_pdfwidth(), height = my_pdfheight()
)
}
 }
# ----------------------------------------

# ----------------------------------------

# ----------------------------------------
# Plot y versus prop_cattle_with_insecticide faceted by NW, coloured by treat_prop
y_vars <- c("Incidence", "prevalence", "No_trt_cat", "RiskA", "Rres_final")
 if (include_plot_type5 == TRUE) {
for (y_var in y_vars) {
  if (this_ttype == 3) {
    plot_type5 <- plot_type5_y_versus_prop_cattle_with_insecticide_facet_NW_ttype3
  } else {
    plot_type5 <- plot_type5_y_versus_prop_cattle_with_insecticide_facet_NW
  }
  p <- plot_type5(subset_for_plotting, y_var, this_NW_set, this_vector_measure, this_vector_measure_value)

  if (y_var == "Rres_final") {
    p <- p + geom_abline(intercept = 1.0, slope = 0, linetype = "dashed")
    plot_label <- paste0("plot_type5_", y_var, common_details_with_fitness)
  } else {
    plot_label <- paste0("plot_type5_", y_var, common_details)
  }
  output_filename <- paste0(folder_name, plot_label, ".pdf")
  plots[[plot_label]] <- p
  my_ggsave(plot = p,
    filename = output_filename,
    width = my_pdfwidth(), height = my_pdfheight()
  )
}
}
# ----------------------------------------

# ----------------------------------------
 if (include_plot_type10 == TRUE) {
p <- plot_type10_R0sen_versus_Rsen(subset_for_plotting)
plot_label <- paste0("plot_type10_R0sen_versus_Rsen", common_details)
output_filename <- paste0(folder_name, plot_label, ".pdf")
plots[[plot_label]] <- p
my_ggsave(plot = p,
  filename = output_filename,
  width = my_pdfwidth(), height = my_pdfheight()
)
}

 if (include_plot_type15 == TRUE) {
# Invasion plots
prev_threshold <- 1.0
restricted_subset <- subset_for_invasion_plots %>% 
  filter(!near(treat_prop, 0.05), !near(treat_prop, 0.15), !near(treat_prop, 0.25), !near(treat_prop, 0.35),
         !near(treat_prop, 0.45), !near(treat_prop, 0.55), !near(treat_prop, 0.65), !near(treat_prop, 0.75),
         !near(treat_prop, 0.85), !near(treat_prop, 0.95), !near(treat_prop, 0.91)) 

#p <- plot_invasion_landscape(prev_threshold, restricted_subset, this_ttype, mainvecpop)
with_contours = TRUE
p <- plot_invasion_landscape(prev_threshold, restricted_subset, this_ttype, mainvecpop, panel_type = "all", with_contours)
plot_label <- paste0("plot_type15_invasion_", prev_threshold, common_details_with_fitness)
output_filename <- paste0(folder_name, plot_label, ".pdf")
my_ggsave(plot = p,
  filename = output_filename,
  width = 2.0 * my_pdfwidth(), height = 1.85 * my_pdfheight()
)
plots[[plot_label]] <- p

with_contours = TRUE
p <- plot_invasion_landscape(prev_threshold, restricted_subset, this_ttype, mainvecpop, panel_type = "single", with_contours)
plot_label <- paste0("plot_type15_invasion_", prev_threshold, common_details_with_fitness, "_single_panel", "_with_contours_", with_contours)
output_filename <- paste0(folder_name, plot_label, ".pdf")
my_ggsave(plot = p,
  filename = output_filename,
  width = 1.65 * my_pdfwidth(), height = 1.65 * my_pdfheight()
)
plots[[plot_label]] <- p
}

 if (include_plot_type16 == TRUE) {
   restricted_subset <- subset_for_invasion_plots %>% 
     filter(!near(treat_prop, 0.05), !near(treat_prop, 0.15), !near(treat_prop, 0.25), !near(treat_prop, 0.35),
            !near(treat_prop, 0.45), !near(treat_prop, 0.55), !near(treat_prop, 0.65), !near(treat_prop, 0.75),
            !near(treat_prop, 0.85), !near(treat_prop, 0.95), !near(treat_prop, 0.91)) 
y_vars <- c("RiskA")
for (y_var in y_vars) {
p <- plot_other_landscape(restricted_subset, y_var, max_value = 13.0, this_ttype)
plot_label <- paste0("plot_type16_landscape", common_details)
output_filename <- paste0(folder_name, plot_label, ".pdf")
my_ggsave(plot = p,
  filename = output_filename,
  width = 2.0 * my_pdfwidth(), height = 1.85 * my_pdfheight()
)
plots[[plot_label]] <- p
}

y_vars <- c("RiskA")
print(" ")
print(paste0(this_ttype, mainvecpop))
print(" ")
for (y_var in y_vars) {
  p <- plot_other_landscape(restricted_subset, y_var, max_value = 13.0, this_ttype, panel_type = "single")
  plot_label <- paste0("plot_type16_landscape", common_details, "_single_panel")
  output_filename <- paste0(folder_name, plot_label, ".pdf")
  my_ggsave(plot = p,
    filename = output_filename,
    width = 1.65 * my_pdfwidth(), height = 1.65 * my_pdfheight()
  )
  plots[[plot_label]] <- p
}
}

# Extra plot to show relationship bewteen treatment frequency and prophylactic coverage
if (this_ttype == 3) {
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
  my_ggsave(plot = p, filename = "output/ms_figs/coverage_vs_frequency.pdf", width = my_pdfwidth(), height = my_pdfheight())
}

# ----------------------------------------
y_vars = c("Rres_final")
 if (include_plot_type7 == TRUE) {
for (y_var in y_vars) {
  p <- plot_type7_y_versus_treat_prop_facet_NW(subset_for_plotting, y_var, this_NW_set, 
                                                 this_vector_measure, this_vector_measure_value, this_ttype)
  plot_label <- paste0("plot_type7_", y_var, common_details_with_fitness)
  output_filename <- paste0(folder_name, plot_label, ".pdf")
  plots[[plot_label]] <- p
  my_ggsave(plot = p,
    filename = output_filename,
    width = my_pdfwidth(), height = 2.0 * my_pdfheight()
  )
}
 }
# ----------------------------------------
 if (include_plot_type8 == TRUE) {
p <- plot_type8(subset_for_plotting, this_ttype)
plot_label <- paste0("plot_type8_", "max_treat_prop", common_details_with_fitness)
output_filename <- paste0(folder_name, plot_label, ".pdf")
plots[[plot_label]] <- p
my_ggsave(plot = p,
  filename = output_filename,
  width = my_pdfwidth(), height = my_pdfheight()
)
}
# ----------------------------------------
if (this_ttype == 1 & mainvecpop == TRUE) {plots1T = plots}
if (this_ttype == 1 & mainvecpop == FALSE) {plots1F = plots}
if (this_ttype == 2 & mainvecpop == TRUE) {plots2T = plots}
if (this_ttype == 2 & mainvecpop == FALSE) {plots2F = plots}
if (this_ttype == 3 & mainvecpop == TRUE) {plots3T = plots}
if (this_ttype == 3 & mainvecpop == FALSE) {plots3F = plots}

}
  }
}
