## ------------------------------------------------------ LOAD LIBRARIES
#rm(list = ls())
library(crayon)
library(codetools)
library(dplyr)
library(deSolve)
library(tidyr)
library(purrr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(cowplot)
library(stringr)
library(tictoc)
library(tidyr)
library(gridExtra)
library(Benchmarking)

## ------------------------------------------------------ LOAD FUNCTIONS
source("1_set_user_inputs.R")
source("funcs/set_params.R")
source("funcs/set_inits.R")
source("funcs/qual_check.R")
source("funcs/helper_functions.R")
source("funcs/epi_outputs.R")
source("funcs/quick_plot.R")
source("funcs/output_baseline_params_and_scenarios.R")
source("funcs/AAT_AMR_dens_dep.R")
source("funcs/r0_intuitive.R")
source("funcs/r0_NGM.R")
source("funcs/r0_helper.R")
source("funcs/plot_helper.R")
source("Cost_analysis.v2.R")
#source("diagnotics.R")
#source("sensitivity_analysis.R")
source("plots_economic_analysis.R")
source("fertility_update.r")
source("fertility_helper_functions.R")
source("run_fertility_model_core_function.R")
source("plot_helper_fertility.R")
source("fast_sim_helper.R")

# Load data files --------------------------------------------------------------
load_latest_file <- TRUE
if (load_latest_file == TRUE) {
  latest_file <- get_latest_Rda_file()
  load(latest_file)
  folder_name <- gsub(".Rda", "/", latest_file)
  dir.create(folder_name)
} else {
  load("output/Aug01_merged2.Rda")
  folder_name <- "output/Aug01_merged2/"
  dir.create(folder_name)
}

test <- saved_simulations
test <- test %>% 
  rename(CEsX_final = CEXs_final, CErX_final = CEXr_final) %>%
  filter(#prop_cattle_with_insecticide %in% c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
         K_host_ratio == 50)

test <- test %>%
  select(NC, treat_prop, NW, prop_cattle_with_insecticide, proph_ongoing, 
                         K, treatment_type, K_host_ratio, maintain_vector_pop,
                         cattle_infection_period, cattle_treatment_period,
                         max_time, Deaths_due_disease, treatment_q, treatment_p, 
                        Incidence, prevalence, contains("final")) 
 
################################################################################
# test <- read.csv("test.csv") # Added by LM
# test <- test %>% filter(NW %in% c(0, 100)) %>%
#   select(NC, treat_prop, NW, prop_cattle_with_insecticide, proph_ongoing,
#          K, treatment_type,
#          cattle_infection_period, cattle_treatment_period,
#          max_time, Deaths_due_disease, treatment_q, treatment_p,
#          Incidence, prevalence, contains("final"))
################################################################################
#--------------------------------------------------------------------------------------
# View inputs
option <- 1 # 1 = responsive curative drug, 2 = responsive proph, 3 = preventive treatment
system <- "Agro-pastoral" #"Agro-pastoral" or "Dairy"
subset <- create_data_subsets(test, option)
nrow(subset)
subset <- subset
tic()
cost_df <- economic_analysis(subset, system, option) # System can be "Dairy" or "Agro-pastoral"
toc()
new_filename <- gsub("output/", "cost_analysis_outputs/cost_df_", latest_file)
save(cost_df, file = new_filename)
load(new_filename)
#da_agro <- economic_analysis(subset, system, option)
#ism_agro <- economic_analysis(subset, system, option)
#proph_agro <- economic_analysis(subset, system, option)
#cost_df <- cost_df %>% filter(treat_prop <= 0.6)

if (option == 3) { 
  cost_df <-  cost_df %>% 
    mutate(
      treat_prop = proph_ongoing * 365.25
    ) %>%
    filter(treatment_type == "proph") %>%
    #remove duplicate rows
    distinct()
}

LM_analysis <- TRUE
if (LM_analysis) { 
  plot_type4_y_versus_treat_prop_facet_NW_3 <- plot_type4_y_versus_treat_prop_facet_NW_3_VHR
  this_K <- 25
  cost_df <- cost_df %>% mutate(VHR = 0.5 * K_host_ratio)
} else {
  plot_type4_y_versus_treat_prop_facet_NW_3 <- plot_type4_y_versus_treat_prop_facet_NW_3_K
  this_K <- 5000
}

this_NW_set <- c(0, 100)
plot_type4_y_versus_treat_prop_facet_NW_BCR(cost_df, "BCR_scenario", this_K, this_NW_set, 30)
plot_type4_y_versus_treat_prop_facet_NW_BCR(cost_df, "BCR_scenario2", this_K, this_NW_set, 30) # Added by LM

plot_type4_y_versus_treat_prop_facet_NW_3(cost_df, "baseline_cost", this_K, this_NW_set, 2000)
plot_type4_y_versus_treat_prop_facet_NW_3(cost_df, "Incidence", this_K, this_NW_set, 1000)
plot_type4_y_versus_treat_prop_facet_NW_3(cost_df, "treat_insecticide_cost", this_K, this_NW_set, 1520)

plot_type4_y_versus_treat_prop_facet_NW_3(cost_df, "net_benefit", this_K, this_NW_set, 2000) # Adjusted by LM

# Calf plots
plot_type4_y_versus_treat_prop_facet_NW_3(cost_df, "calves_per_year_per_female", this_K, this_NW_set, 1) + ylim(0.6, 0.8) 
plot_type4_y_versus_treat_prop_facet_NW_3(cost_df, "Xcalves_per_adult_female", this_K, this_NW_set, 1) + ylim(0.6, 0.8) 
plot_type4_y_versus_treat_prop_facet_NW_3(cost_df, "Xcalf_revenue_herd", this_K, this_NW_set, 3000) + ylim(2500, 3100)
plot_type4_y_versus_treat_prop_facet_NW_3(cost_df, "averted_cost_calves_per_year_per_herd", this_K, this_NW_set, 500)
plot_type4_y_versus_treat_prop_facet_NW_3(cost_df, "averted_cost_calves_per_year_per_herd2", this_K, this_NW_set, 500)

plot_type4_y_versus_treat_prop_facet_NW_3(cost_df, "Xmilk_days_per_year_per_adult_female", this_K, this_NW_set, 300)
plot_type4_y_versus_treat_prop_facet_NW_3(cost_df, "Xmilk_revenue_herd", this_K, this_NW_set, 3500) + ylim(2500, 3500)
plot_type4_y_versus_treat_prop_facet_NW_3(cost_df, "Xmilk_revenue_herd", this_K, this_NW_set, 3500) + ylim(2500, 3500)

plot_type4_y_versus_treat_prop_facet_NW_3(cost_df, "milk_losses_averted", this_K, this_NW_set, 500)
plot_type4_y_versus_treat_prop_facet_NW_3(cost_df, "milk_losses_averted2", this_K, this_NW_set, 500)

plot_type4_y_versus_treat_prop_facet_NW_3(cost_df, "sum_averted_production_losses", this_K, this_NW_set, 2000)
plot_type4_y_versus_treat_prop_facet_NW_3(cost_df, "sum_averted_production_losses2", this_K, this_NW_set, 2000)


plot_type4_y_versus_treat_prop_facet_NW_1(cost_df, "prevalence", this_K, this_NW_set)
##===============================================================================

#Plot of individual production losses
if (option %in% c(1,2)) {
  cost_df_mod <- cost_df %>%
    filter(treat_prop == 0, prop_cattle_with_insecticide == 0, NW == this_NW_set)
} else if (option == 3) {
  cost_df_mod <- cost_df %>%
    filter(proph_ongoing == 0, prop_cattle_with_insecticide == 0, NW == this_NW_set)
}

# A table of the baseline production losses and proportions
cost_df_mod %>%
  mutate(
    total_loss = cost_milk_prod_loss_baseline +
      cost_draught_power_loss_baseline +
      cost_mortality_loss_calf_baseline +
      cost_mortality_loss_adult_baseline +
      calves_per_year_per_baseline
  ) %>%
  mutate(
    prop_milk_prod_loss = cost_milk_prod_loss_baseline / total_loss,
    prop_draught_power_loss = cost_draught_power_loss_baseline / total_loss,
    prop_mortality_loss_calf = cost_mortality_loss_calf_baseline / total_loss,
    prop_mortality_loss_adult = cost_mortality_loss_adult_baseline / total_loss,
    prop_fertility_loss = calves_per_year_per_baseline / total_loss
  ) %>%
  select(NW, cost_milk_prod_loss_baseline, cost_draught_power_loss_baseline,
         cost_mortality_loss_calf_baseline, cost_mortality_loss_adult_baseline,
         calves_per_year_per_baseline,
         prop_milk_prod_loss, prop_draught_power_loss,
         prop_mortality_loss_calf, prop_mortality_loss_adult,
         prop_fertility_loss)

#Convert to long format for plotting
cost_df_long <- cost_df_mod %>%
  mutate(scenario = paste0("NW=", NW, ", K=", K)) %>%
  pivot_longer(
    cols = c(
      "cost_milk_prod_loss_baseline",
      "cost_draught_power_loss_baseline",
      "cost_mortality_loss_calf_baseline",
      "cost_mortality_loss_adult_baseline",
      "calves_per_year_per_baseline"
    ),
    names_to = "loss_component",
    values_to = "loss_value"
  ) %>%
  group_by(scenario) %>%
  mutate(
    total_loss = sum(loss_value, na.rm = TRUE),
    proportion = loss_value / total_loss,
    label = paste0(round(proportion * 100, 1), "%")
  ) %>%
  ungroup()

#Labels for loss components
labels = c(
  "cost_mortality_loss_calf_baseline" = "Calf mortality ",
  "cost_mortality_loss_adult_baseline" = "Adult mortality ",
  "cost_milk_prod_loss_baseline" = "Milk production ",
  "calves_per_year_per_baseline" = "Fertility losses",
  "cost_draught_power_loss_baseline" = "Draft power"
)

#Plot
ggplot(cost_df_long, aes(x = loss_component, y = proportion, fill = loss_component)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            size = 4, color = "white") +
  facet_wrap(~ scenario) +
  labs(
    x = "",
    y = "% of loss contribution",
    title = ""
  ) +
  scale_x_discrete(labels = labels) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
  my_theme_1() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.text = element_blank() #element_text(face = "bold")
  )

#===============================================================================
# Sensitivity analysis
#===============================================================================

if (option == 1) {
  parameters <- c(
    "cost_quick_curative",
    "cost_hire_oxen",
    "cost_insecticide",
    "cost_per_litre",
    "adult_sale",
    "calf_sale"
    )
  label_map <- c(
    cost_quick_curative   = "Diminazene cost",
    cost_hire_oxen   = "Return per draught oxen",
    cost_insecticide      = "Insecticide cost",
    cost_per_litre = "Return per milk of litre",
    adult_sale = "Adult cattle sale price",
    calf_sale = "Calf sale price")
  
  } else if (option %in% c(2,3)) {
  parameters <- c(
    "cost_proph",
    "cost_hire_oxen",
    "cost_insecticide",
    "cost_per_litre",
    "adult_sale",
    "calf_sale"
    )
  label_map <- c(
    cost_proph            = "Isometamedium cost",
    cost_hire_oxen   = "Return per draught oxen",
    cost_insecticide      = "Insecticide cost",
    cost_per_litre = "Return per milk litre",
    adult_sale = "Adult cattle sale price",
    calf_sale = "Calf sale price")
} 

percent_changes <- -0.5 # -0.5 = halving, 1 = doubling%
sens_df <- sensitivity_analysis(subset, parameters, percent_changes, system, option)
change_dir <- ifelse(percent_changes < 0, "_minus_0", "_plus_1")

treat_val <- 0.1
scenario_long <- sens_df %>%
  filter(K == 5000, NW == 0, treat_prop == treat_val)  

drug_type <- ifelse(
  option == 1,
  "cost_quick_curative",
  "cost_proph")

scenario_plot <- scenario_long %>%
  filter(K == 5000, NW == 0, treat_prop == treat_val) %>%
  pivot_longer(cols = starts_with("cost_change_"),
               names_to = "parameter",
               values_to = "percent_change") %>%
  mutate(parameter = gsub("cost_change_", "", parameter),
         parameter = gsub(change_dir, "", parameter),
         parameter = case_when(
           parameter == drug_type ~ 
             paste0(
               "Drug cost (insecticide coverage = ",
               prop_cattle_with_insecticide * 100,
               "%)"
             ),
           parameter == "cost_insecticide" ~ 
             paste0("Insecticide cost (insecticide coverage = ", prop_cattle_with_insecticide*100, "%)"),
           parameter == "cost_per_litre" ~ "Milk price",
           parameter == "cost_hire_oxen" ~ "Ox hire",
           parameter == "adult_sale" ~ "Adult cattle sale price",
           parameter == "calf_sale" ~ "Calf sale price",
           TRUE ~ parameter
         )) %>%
  # Keep only unique rows for Milk price and Ox hire
  filter(!(parameter %in% c("Milk price", "Ox hire", "Adult cattle sale price", "Calf sale price") & prop_cattle_with_insecticide != 0))

ggplot(scenario_plot, aes(x = parameter,
                          y = percent_change)) +
  geom_col(fill = "grey40") +
  coord_flip() +
  scale_y_continuous(limits = c(-1, 1)) +
  geom_text(aes(label = round(percent_change, 2)),
            hjust = ifelse(scenario_plot$percent_change > 0, -0.1, 1.1),
            size = 5) +
  labs(x = "",
       y = "Proportional change in BCR",
       title = paste("Sensitivity at treat prop =", treat_val, " (K=5000, NW=0)")) +
  my_theme_1() 
#===============================================================================


# PART TWO: DIAGNOSTIC INTEGRATION COST ANALYSIS
#===============================================================================
diag_df <- diag_cost_func(cost_df, treatment_type = ifelse(option == 1, "quick", "proph"))
plot_type4_y_versus_treat_prop_facet_NW_3( diag_df, "total_treat_insect_diag_cost", this_K, this_NW_set, ylim_max = 7000)





