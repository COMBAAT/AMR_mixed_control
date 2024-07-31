# Load required packages -------------------------------------------------------

library(dplyr)
library(gghighlight)
library(ggplot2)
library(patchwork)

# Source files and function
source("funcs/plot_helper.R")
source("funcs/helper_functions.R")
source("funcs/output_baseline_params_and_scenarios.R")

# Load data files --------------------------------------------------------------
load_latest_file <- TRUE
if (load_latest_file == TRUE) {
  latest_file <- get_latest_Rda_file()
  load(latest_file)
  plot_descriptor <- test$descriptor[1]
  folder_name <- gsub(".Rda", "/", latest_file)
  dir.create(folder_name)
} else {
  load("output/quick_treatment_option4.Rda")
  plot_descriptor <- test$descriptor[1]
  folder_name <- "output/merged/"
  dir.create(folder_name)
}


reduced_scenarios <- scenarios_df %>%
  select(-NW, -K, -treat_prop, -prop_cattle_with_insecticide) %>%
  distinct()
reduced_scenarios
selected_row <- 1
subset_for_plotting <- left_join(reduced_scenarios[selected_row, ], test)

# Generate plots ---------------------------------------------------------------
# Plot and save baseline parameters
output_label <- "00_baseline_parameters"
p <- plot_baseline_parameters(baseline_parameters)
plot_name <- "00_baseline_parameters.pdf"
ggsave(
  filename = paste0(folder_name, output_label, ".pdf"),
  width = my_pdfwidth(), height = my_pdfheight()
)
write.csv(baseline_parameters, file = paste0(folder_name, output_label, ".csv"))

# Plot and save scenarios
output_label <- "00_scenarios"
p <- plot_scenarios(scenarios_df)
ggsave(p,
  filename = paste0(folder_name, output_label, ".pdf"),
  width = my_pdfwidth(), height = 1.5 * my_pdfheight()
)
scenarios_for_output <- get_simplified_scenarios(scenarios_df)
write.csv(scenarios_for_output, file = paste0(folder_name, output_label, ".csv"))

# Plot R0 versus wildlife faceted by treat_prop
subset_for_plotting %>%
  mutate_at(c("prop_cattle_with_insecticide", "treat_prop", "K"), as.factor) %>%
  filter(prop_cattle_with_insecticide == 0, treat_prop %in% c(0, 0.6, 0.95)) %>%
  ggplot(aes(NW, R0sen, colour = K)) +
  geom_point(size = my_pointsize()) +
  geom_line(linewidth = my_linewidth()) +
  facet_wrap(~treat_prop) +
  xlab(my_label("NW")) +
  ylab(my_label("R0sen")) +
  labs(colour = my_label("K")) +
  my_theme()

plot_name <- paste0("plot_type0_R0sen", "_", plot_descriptor, ".pdf")
ggsave(
  filename = paste0(folder_name, plot_name),
  width = my_pdfwidth(), height = my_pdfheight()
)

# ----------------------------------------
# Plot R resistant/R sensitive versus wildlife faceted by treat_prop
lhs <- subset_for_plotting %>%
  mutate_at(c("prop_cattle_with_insecticide", "NW", "K"), as.factor) %>%
  filter(prop_cattle_with_insecticide == 0) %>%
  ggplot(aes(treat_prop, Rres_final / Rsen_final, colour = NW, shape = K)) +
  geom_point(size = my_pointsize()) +
  geom_line(linewidth = my_linewidth()) +
  xlab(my_label("treat_prop")) +
  ylab("R resistant / R sensitive") +
  labs(colour = my_label("NW"), shape = my_label("K")) +
  my_theme()

rhs <- lhs + ylim(c(0, 2)) + geom_abline(intercept = 1, slope = 0, linetype = "dashed")
rhs

# use patchwork package to stick plots together
# use guides = collect to remove duplicate legends
lhs + rhs + plot_layout(ncol = 2, guides = "collect")

plot_name <- paste0("plot_type0_R_res_R_sen_ratio", "_", plot_descriptor, ".pdf")
ggsave(
  filename = paste0(folder_name, plot_name),
  width = my_pdfwidth(), height = my_pdfheight()
)

# ----------------------------------------
# Plot y versus_treat_prop faceted by NW
y_vars <- c(
  "R0sen", "prevalence", "Incidence", "No_trt_cat", "Prob_onward_tran",
  "RiskE", "RiskA"
)

for (y_var in y_vars) {
  plot_type1_y_versus_treat_prop_facet_NW(subset_for_plotting, y_var)

  plot_name <- paste0("plot_type1_", y_var, "_", plot_descriptor, ".pdf")
  ggsave(
    filename = paste0(folder_name, plot_name),
    width = my_pdfwidth(), height = my_pdfheight()
  )
}
# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by prop_cattle_with_insecticide

y_var <- "RiskA"
this_K <- 10000
plot_type2_y_versus_treat_prop_facet_prop_cattle_with_insecticide(subset_for_plotting, this_K, y_var)
plot_name <- paste0("plot_type2_", y_var, "_", plot_descriptor, ".pdf")
ggsave(
  filename = paste0(folder_name, plot_name),
  width = my_pdfwidth(), height = my_pdfheight()
)

y_var <- "RiskE"
this_K <- 10000
plot_type2_y_versus_treat_prop_facet_prop_cattle_with_insecticide(subset_for_plotting, this_K, y_var)
plot_name <- paste0("plot_type2_", y_var, "_", plot_descriptor, ".pdf")
ggsave(
  filename = paste0(folder_name, plot_name),
  width = my_pdfwidth(), height = my_pdfheight()
)

# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by prop_cattle_with_insecticide with highlighting
y_var <- "RiskE"
this_K <- 10000
threshold_var <- "prevalence"
threshold <- 0.1
plot_type3_y_versus_treat_prop_facet_prop_cattle_with_insecticide_with_higlight(
  subset_for_plotting, this_K, y_var, threshold_var, threshold
)
plot_name <- paste0("plot_type3_", y_var, "_", plot_descriptor, ".pdf")
ggsave(
  filename = paste0(folder_name, plot_name),
  width = my_pdfwidth(), height = my_pdfheight()
)

# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by NW, coloured by prop_cattle_with_insecticide
y_vars <- c("Incidence", "prevalence", "No_trt_cat", "RiskE")
this_K <- 6000

for (y_var in y_vars) {
  plot_type4_y_versus_treat_prop_facet_NW(subset_for_plotting, y_var, this_K)

  plot_name <- paste0("plot_type4_", y_var, "_", plot_descriptor, ".pdf")
  ggsave(
    filename = paste0(folder_name, plot_name),
    width = my_pdfwidth(), height = my_pdfheight()
  )
}
# ----------------------------------------

# ----------------------------------------
# Plot y versus prop_cattle_with_insecticide faceted by NW, coloured by treat_prop
y_vars <- c("Incidence", "prevalence", "No_trt_cat", "RiskE")
this_K <- 6000

for (y_var in y_vars) {
  plot_type5_y_versus_prop_cattle_with_insecticide_facet_NW(subset_for_plotting, y_var, this_K)

  plot_name <- paste0("plot_type5_", y_var, "_", plot_descriptor, ".pdf")
  ggsave(
    filename = paste0(folder_name, plot_name),
    width = my_pdfwidth(), height = my_pdfheight()
  )
}
# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by NW, coloured by prop_cattle_with_insecticide
y_vars <- c("Incidence", "prevalence", "No_trt_cat", "RiskE")

for (y_var in y_vars) {
  plot_type6_y_versus_treat_prop_facet_NW_K(subset_for_plotting, y_var)

  plot_name <- paste0("plot_type6_", y_var, "_", plot_descriptor, ".pdf")
  ggsave(
    filename = paste0(folder_name, plot_name),
    width = my_pdfwidth(), height = 2 * my_pdfheight()
  )
}