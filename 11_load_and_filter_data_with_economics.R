library(ggplot2)
library(dplyr)
library(patchwork)
library(tictoc)

source("funcs/helper_functions.R")
source("funcs/epi_outputs.R")
source("funcs/plot_helper.R")
source("funcs/plot_settings.R") # this will change the colour palette away from the default
source("15_plots_for_SALT_TZ_helper.R")

# Load data with cost analysis -------------------------------------------------
load("output/output_with_economics/14Feb2026_test1_eco.Rda")

nrow(all_data_with_cost_analysis)
ncol(all_data_with_cost_analysis)

###############################################################################
choices <- unique(all_data_with_cost_analysis$use_carrying_capacity)
choices
if (choices > 1) {
  stop("you need to choice whether to use carrying capacity")
} else if (choices == TRUE) {
  print(paste0("use_carrying_capacity = ", choices))
  data_with_K_selection <- all_data_with_cost_analysis %>% 
    mutate(K_variable = K) #%>% filter(K_variable == K_variable_value)
} else {
  print(paste0("use_carrying_capacity = ", choices))
  data_with_K_selection <- all_data_with_cost_analysis %>% 
    mutate(K_variable = K_host_ratio) #%>% filter(K_variable == K_variable_value)
} 

# Create extra variables as factors of continuous variables for plotting
data_with_K_selection <- data_with_K_selection %>%
  mutate(
    prop_cattle_with_insecticide = round(prop_cattle_with_insecticide, 2),
    NW_factor = as.factor(NW),
    prop_cattle_with_insecticide_factor = as.factor(prop_cattle_with_insecticide),
    Baseline_vector_host_ratio_factor = as.factor(Baseline_vector_host_ratio),
    cov_lt_0.3 = prop_cattle_with_insecticide < 0.3, 
    Rres_gt_1 = case_when(Rres_final > 1 ~ "yes", TRUE ~ "no"),
    Insecticide_strategy = 
             case_when(maintain_vector_pop == FALSE ~ "Cooperative",
                       maintain_vector_pop == TRUE ~ "Individual"),
    NW_jitter = case_when(Baseline_vector_host_ratio == 15 ~ NW - 10,
                                   Baseline_vector_host_ratio == 25 ~ NW,
                                   Baseline_vector_host_ratio == 35 ~ NW + 10,
                                   TRUE ~ NW),
    drug_use_in_protocol = 
      case_when(treat_prop + treatments_per_year == 0 ~ "no trypanocide",
                treatments_per_year > 0 ~ "prophylactic",
                TRUE ~ treatment_type))

nrow(data_with_K_selection)
ncol(data_with_K_selection)
###############################################################################
