library(ggplot2)
library(dplyr)
library(patchwork)
library(tictoc)

source("funcs/helper_functions.R")
source("funcs/epi_outputs.R")
source("funcs/plot_helper.R")
source("funcs/plot_settings.R")
source("plots_for_TZ_helper.R")
source("funcs/plot_settings.R")

# Cost analysis scripts
source("funcs/set_params.R")
source("Cost_analysis.v2.r")
source("fast_sim_helper.R")
source("fertility_update.r")
source("run_fertility_model_core_function.R")


# Load data files --------------------------------------------------------------
load_latest_file <- TRUE
if (load_latest_file == TRUE) {
  latest_file <- get_latest_Rda_file()
  print(latest_file)
  load(latest_file)
  folder_name <- gsub(".Rda", "/", latest_file)
  dir.create(folder_name)
} else {
  load("output/Jan_mortality_fix_and_protection_80_combined_1.Rda")
  folder_name <- "output/Jan_mortality_fix_and_protection_80_combined_1/"
  dir.create(folder_name)
}

nrow(saved_simulations)
table(saved_simulations$prop_cattle_with_insecticide)
table(saved_simulations$K_host_ratio)
table(saved_simulations$maintain_vector_pop)


# temp fix until code is rerun with fixed append_epi_outputs_to_df()
corrected_data <- append_epi_outputs_to_df(saved_simulations)
corrected_data <- corrected_data %>% 
  mutate(prevalence = prevalence_new, Incidence = Incidence_new) %>% 
  rename(CEsX_final = CEXs_final, CErX_final = CEXr_final)

data_for_analysis <- corrected_data

subsets <- list()
for (ttype in 1:3){
  subset <- create_data_subsets(data_for_analysis, ttype)
  print(nrow(subset))
  subset$treatment_code <- ttype
  subsets[[ttype]] <- subset
}
all_data_no_cost_analysis <- data.frame(rbind(subsets[[1]], subsets[[2]], subsets[[3]])) 


###############################################################################
# Add on cost analyses
for (option in 1:3){
  system <- "Agro-pastoral" #"Agro-pastoral" or "Dairy"
  this_subset <- subsets[[option]]
  #this_subset <- head(this_subset, 20)
  print(nrow(this_subset))
  tic()
  this_cost_df <- economic_analysis(this_subset, system, option) # System can be "Dairy" or "Agro-pastoral"
  toc()
  if (option == 1){cost_df1 <- this_cost_df}
  if (option == 2){cost_df2 <- this_cost_df}
  if (option == 3){cost_df3 <- this_cost_df}
}

all_data_with_cost_analysis <- rbind(cost_df1, cost_df2, cost_df3)

###############################################################################
choices <- unique(all_data_with_cost_analysis$use_carrying_capacity)
if (choices > 1) {
  stop("you need to choice whether to use carrying capacity")
} else if (choices == TRUE) {
  K_variable_value <- 5000
  print(paste0("use_carrying_capacity = ", choices))
  all_data_with_cost_analysis <- all_data_with_cost_analysis %>% 
    mutate(K_variable = K) #%>% filter(K_variable == K_variable_value)
} else {
  K_variable_value <- 50
  print(paste0("use_carrying_capacity = ", choices))
  all_data_with_cost_analysis <- all_data_with_cost_analysis %>% 
    mutate(K_variable = K_host_ratio) #%>% filter(K_variable == K_variable_value)
} 

###############################################################################
# Choose extra plotting filters
this_NW <- 0
main_vec <- TRUE
this_K_variable_value <- 50
all_data_for_plotting <- all_data_with_cost_analysis %>% 
  filter(NW == this_NW, maintain_vector_pop == main_vec,
         K_variable == this_K_variable_value)

if (main_vec == TRUE) {
  all_data_for_plotting <- all_data_for_plotting %>% 
    filter(near(prop_cattle_with_insecticide, 0.0) |
             near(prop_cattle_with_insecticide, 0.1) |
             near(prop_cattle_with_insecticide, 0.2) |
             near(prop_cattle_with_insecticide, 0.3) |
             near(prop_cattle_with_insecticide, 0.4) |
             near(prop_cattle_with_insecticide, 0.5)) %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW"), as.factor) 
} else {
  all_data_for_plotting <- all_data_with_cost_analysis %>% 
    filter(near(prop_cattle_with_insecticide, 0.0) |
             near(prop_cattle_with_insecticide, 0.05) |
             near(prop_cattle_with_insecticide, 0.1) |
             near(prop_cattle_with_insecticide, 0.15) |
             near(prop_cattle_with_insecticide, 0.2) |
             near(prop_cattle_with_insecticide, 0.25)) %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW"), as.factor) 
}

###############################################################################
props_all <- sort(unique(all_data_for_plotting$prop_cattle_with_insecticide))
base_cols <- get_base_cols(props_all, my_palette = projector_cols_warm_first)

###############################################################################
all_plot_details <- paste0("_maintain_vec_pop_", main_vec, "_K_variable_", K_variable_value, "_NW_", this_NW)
###############################################################################

################################################################################
plot_this <- all_data_for_plotting
y_var <- "Incidence"
y_max <- 1000
plot_this$y <- plot_this[, y_var]

for (highlight_groups in 6:6){
  cols <- make_cols_highlight(highlight_props = props_all[1:highlight_groups], 
                              props_all = props_all,
                              base_cols = base_cols)
  
  p <- plot_panel_by_treatment_type(plot_this, y_var, y_max)
  
  this_plot_details <- paste0(y_var, all_plot_details)
  filename <- paste0("output/SALT_TZ/", this_plot_details, "_hlight_", highlight_groups, ".pdf")
  my_ggsave(p, filename, width = 9.5, height = 4.5)
}
p
################################################################################

plot_this <- all_data_for_plotting
y_var <- "prevalence"
y_max <- 1
plot_this$y <- plot_this[, y_var]

for (highlight_groups in 6:6){
  cols <- make_cols_highlight(highlight_props = props_all[1:highlight_groups], 
                              props_all = props_all,
                              base_cols = base_cols)
  
  p <- plot_panel_by_treatment_type(plot_this, y_var, y_max)
  
  this_plot_details <- paste0(y_var, all_plot_details)
  filename <- paste0("output/SALT_TZ/", this_plot_details, "_hlight_", highlight_groups, ".pdf")
  my_ggsave(p, filename, width = 9.5, height = 4.5)
}
p
################################################################################
# plot economics outputs
################################################################################

plot_this <- all_data_for_plotting
y_var <- "treat_insecticide_cost"
y_max <- 1500
plot_this$y <- plot_this[, y_var]

for (highlight_groups in 6:6){
  cols <- make_cols_highlight(highlight_props = props_all[1:highlight_groups], 
                              props_all = props_all,
                              base_cols = base_cols)
  
  p <- plot_panel_by_treatment_type(plot_this, y_var, y_max)
  
  this_plot_details <- paste0(y_var, all_plot_details)
  filename <- paste0("output/SALT_TZ/", this_plot_details, "_hlight_", highlight_groups, ".pdf")
  my_ggsave(p, filename, width = 9.5, height = 4.5)
}
p

################################################################################
plot_this <- all_data_for_plotting
y_var <- "sum_averted_production_losses"
y_max <- 2000
plot_this$y <- plot_this[, y_var]

for (highlight_groups in 6:6){
  cols <- make_cols_highlight(highlight_props = props_all[1:highlight_groups], 
                              props_all = props_all,
                              base_cols = base_cols)
  
  p <- plot_panel_by_treatment_type(plot_this, y_var, y_max)
  
  this_plot_details <- paste0(y_var, all_plot_details)
  filename <- paste0("output/SALT_TZ/", this_plot_details, "_hlight_", highlight_groups, ".pdf")
  my_ggsave(p, filename, width = 9.5, height = 4.5)
}
p

################################################################################

plot_this <- all_data_for_plotting
y_var <- "R0sen_final"
y_max <- 8
plot_this$y <- plot_this[, y_var]

for (highlight_groups in 6:6){
  cols <- make_cols_highlight(highlight_props = props_all[1:highlight_groups], 
                              props_all = props_all,
                              base_cols = base_cols)
  
  p <- plot_panel_by_treatment_type(plot_this, y_var, y_max)
  
  this_plot_details <- paste0(y_var, all_plot_details)
  filename <- paste0("output/SALT_TZ/", this_plot_details, "_hlight_", highlight_groups, ".pdf")
  my_ggsave(p, filename, width = 9.5, height = 4.5)
}
p

################################################################################
plot_this <- all_data_for_plotting
y_var <- "RiskA"
y_max <- 8
plot_this$y <- plot_this[, y_var]

for (highlight_groups in 6:6){
  cols <- make_cols_highlight(highlight_props = props_all[1:highlight_groups], 
                              props_all = props_all,
                              base_cols = base_cols)
  
  p <- plot_panel_by_treatment_type(plot_this, y_var, y_max)
  
  this_plot_details <- paste0(y_var, all_plot_details)
  filename <- paste0("output/SALT_TZ/", this_plot_details, "_hlight_", highlight_groups, ".pdf")
  my_ggsave(p, filename, width = 9.5, height = 4.5)
}
p

################################################################################
 
plot_this <- all_data_for_plotting
y_var <- "Rres_final"
y_max <- 8
plot_this$y <- plot_this[, y_var]

for (highlight_groups in 6:6){
  cols <- make_cols_highlight(highlight_props = props_all[1:highlight_groups], 
                              props_all = props_all,
                              base_cols = base_cols)
  
  p <- plot_panel_by_treatment_type(plot_this, y_var, y_max)
  
  this_plot_details <- paste0(y_var, all_plot_details)
  filename <- paste0("output/SALT_TZ/", this_plot_details, "_hlight_", highlight_groups, ".pdf")
  my_ggsave(p, filename, width = 9.5, height = 4.5)
}
p
#############
for (highlight_groups in 6:6){
  cols <- make_cols_highlight(highlight_props = props_all[1:highlight_groups], 
                              props_all = props_all,
                              base_cols = base_cols)
  
  p <- plot_panel_by_treatment_type(plot_this, y_var, y_max, rectangle = TRUE)
  
  this_plot_details <- paste0(y_var, "_with_shading", all_plot_details)
  filename <- paste0("output/SALT_TZ/", this_plot_details, "_hlight_", highlight_groups, ".pdf")
  my_ggsave(p, filename, width = 9.5, height = 4.5)
}
p
#############
y_max <- 2
for (highlight_groups in 6:6){
  cols <- make_cols_highlight(highlight_props = props_all[1:highlight_groups], 
                              props_all = props_all,
                              base_cols = base_cols)
  
  p <- plot_panel_by_treatment_type(plot_this, y_var, y_max, rectangle = TRUE)
  
  this_plot_details <- paste0(y_var, "_zoom", all_plot_details)
  filename <- paste0("output/SALT_TZ/", this_plot_details, "_hlight_", highlight_groups, ".pdf")
  my_ggsave(p, filename, width = 9.5, height = 4.5)
}
p
################################################################################

################################################################################

plot_this <- all_data_for_plotting
y_var <- "BCR_scenario"
y_max <- max(plot_this$BCR_scenario)
plot_this$y <- plot_this[, y_var]

for (highlight_groups in 6:6){
  cols <- make_cols_highlight(highlight_props = props_all[1:highlight_groups], 
                              props_all = props_all,
                              base_cols = base_cols)
  
  p <- plot_panel_by_treatment_type(plot_this, y_var, y_max, rectangle = TRUE)
  
  this_plot_details <- paste0(y_var, all_plot_details)
  filename <- paste0("output/SALT_TZ/", this_plot_details, "_hlight_", highlight_groups, ".pdf")
  my_ggsave(p, filename, width = 9.5, height = 4.5)
}
p
################################################################################

################################################################################
################################################################################
################################################################################

props_all <- sort(unique(all_data_with_cost_analysis$prop_cattle_with_insecticide))
length(props_all)
base_cols <- get_base_cols(props_all, my_palette = projector_cols_warm_first)

highlight_groups <- length(props_all)
cols <- make_cols_highlight(highlight_props = props_all[1:highlight_groups], 
                            props_all = props_all,
                            base_cols = base_cols)

################################################################################
# Examine frontier 
df_slope_1 <- data.frame(x = c(0, 1500))
df_slope_1$y <- df_slope_1$x
df_slope_1

df_slope_2.5 <- data.frame(x = c(0, 1500))
df_slope_2.5$y <- 2.5 * df_slope_2.5$x
df_slope_2.5

all_data_for_frontier <- all_data_with_cost_analysis %>%
  filter(treat_prop < 0.6) %>% 
  #filter(treatments_per_year < 7) %>%
  filter(K_variable == 50) 

all_data_BCR_max <- all_data_for_frontier %>%
  group_by(NW, treatment_type) %>% 
  summarise(BCR_max = max(BCR_scenario, na.rm = TRUE))

p <- all_data_for_frontier %>% filter(prevalence < 0.1) %>%
  mutate_at(c("prop_cattle_with_insecticide", "NW"), as.factor) %>%
  ggplot() +
  geom_point(aes(x = treat_insecticide_cost, y = sum_averted_production_losses, 
                 colour = prop_cattle_with_insecticide), size = 1) +
  coord_cartesian(ylim = c(0, 1800)) +
  facet_wrap(~ NW + treatment_code) +
  labs(colour = " Insecticide\n coverage") +
  xlab(my_label("treat_insecticide_cost")) +
  ylab(my_label("sum_averted_production_losses")) +
  scale_color_manual(values = cols) +
  my_theme_SALT_TZ() + 
# Add shading for regions of BCR < 2.5 and < 1
  geom_ribbon(data = df_slope_1, aes(x = x, ymin = 0, ymax = y),
    fill = "grey40", alpha = 0.3) +
  geom_ribbon(data = df_slope_2.5, aes(x = x, ymin = 0, ymax = y),
    fill = "grey60", alpha = 0.3) +
  # Overlay original points again
  geom_point(aes(x = treat_insecticide_cost, y = sum_averted_production_losses, 
                 colour = prop_cattle_with_insecticide), size = 1) +
# Add slope at max BCR
geom_abline(data = all_data_BCR_max, 
              aes(intercept = 0, slope = BCR_max), linewidth = 0.5) +
  geom_text(data = all_data_BCR_max,
    aes( x = 50, y = 1650, label = round(BCR_max, 1)),
    inherit.aes = FALSE,
    size = 3) 
p

p + gghighlight(prevalence < 0.1)

plot_details <- paste0("frontier_", "maintain_vec_pop_", main_vec, "_K_variable_", K_variable_value)
filename <- paste0("output/SALT_TZ/", plot_details, ".pdf")
my_ggsave(plot = p, filename = filename, width = 9, height = 9)
################################################################################
# Extract best BCR values subject to prevalence of Incidence restrictions
df <- all_data_with_cost_analysis
best_by_scenario <- df %>%
  filter(treat_prop < 0.8) %>%
  mutate(prop_cattle_with_insecticide = as.factor(prop_cattle_with_insecticide)) %>%
  select(NW, K_variable, maintain_vector_pop, treatment_code, BCR_scenario, 
         treat_prop, proph_ongoing, prop_cattle_with_insecticide,
         treat_insecticide_cost, sum_averted_production_losses) %>%
  group_by(NW, K_variable, maintain_vector_pop, treatment_code) %>%
  slice_max(order_by = BCR_scenario, n = 1, with_ties = TRUE) %>%   # keep one row
  ungroup()

glimpse(best_by_scenario)

best_by_scenario %>% ggplot() +
  geom_point(aes(x = treat_insecticide_cost, y = sum_averted_production_losses, 
                 colour = prop_cattle_with_insecticide), size = 1) +
  coord_cartesian(ylim = c(0, 1800)) +
  facet_wrap(~ NW + treatment_code) +
  labs(colour = " Insecticide\n coverage") +
  xlab(my_label("treat_insecticide_cost")) +
  ylab(my_label("sum_averted_production_losses")) +
  scale_color_manual(values = cols) +
  my_theme_SALT_TZ() + 
  # Add shading for regions of BCR < 2.5 and < 1
  geom_ribbon(data = df_slope_1, aes(x = x, ymin = 0, ymax = y),
              fill = "grey40", alpha = 0.3) +
  geom_ribbon(data = df_slope_2.5, aes(x = x, ymin = 0, ymax = y),
              fill = "grey60", alpha = 0.3) +
  # Overlay original points again
  geom_point(aes(x = treat_insecticide_cost, y = sum_averted_production_losses, 
                 colour = prop_cattle_with_insecticide), size = 1) +
  # Add slope at max BCR
  geom_abline(data = all_data_BCR_max, 
              aes(intercept = 0, slope = BCR_max), linewidth = 0.5) +
  geom_text(data = all_data_BCR_max,
            aes( x = 50, y = 1650, label = round(BCR_max, 1)),
            inherit.aes = FALSE,
            size = 3) 
p
