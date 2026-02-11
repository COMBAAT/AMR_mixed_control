################################################################################
# Frontier plots
################################################################################
# create dataframes for shaded areas
df_slope_1 <- data.frame(x = c(0, 1800))
df_slope_1$y <- df_slope_1$x
df_slope_1

df_slope_2.5 <- data.frame(x = c(0, 1800))
df_slope_2.5$y <- 2.5 * df_slope_2.5$x
df_slope_2.5

###############################################################################
# Choose extra plotting filters
this_NW <- 0
main_vec <- TRUE
this_K_variable_value <- 50

data_for_frontier_plots <- data_with_K_selection %>%
  filter(K_variable == this_K_variable_value) %>%
  filter(maintain_vector_pop == main_vec) 
####################
# pull out best values for each NW and treatment type
all_data_BCR_max <- data_for_frontier_plots %>%
  group_by(NW_factor, treatment_type) %>% 
  summarise(BCR_max = max(BCR_scenario, na.rm = TRUE))

####################
p <- data_for_frontier_plots %>%
  ggplot() +
  geom_point(aes(x = treat_insecticide_cost, y = sum_averted_production_losses, 
                 colour = prop_cattle_with_insecticide_factor), size = 1) +
  coord_cartesian(ylim = c(0, 1800)) +
  facet_wrap(~ NW_factor + treatment_type) +
  labs(colour = " Insecticide\n coverage") +
  xlab(my_label("treat_insecticide_cost")) +
  ylab(my_label("sum_averted_production_losses")) +
  #scale_color_manual(values = cols) +
  my_theme_SALT_TZ() + 
  # Add shading for regions of BCR < 2.5 and < 1
  geom_ribbon(data = df_slope_1, aes(x = x, ymin = 0, ymax = y),
              fill = "grey40", alpha = 0.3) +
  geom_ribbon(data = df_slope_2.5, aes(x = x, ymin = 0, ymax = y),
              fill = "grey60", alpha = 0.3) +
  # Overlay original points again
  geom_point(aes(x = treat_insecticide_cost, 
                 y = sum_averted_production_losses, 
                 colour = prop_cattle_with_insecticide_factor), size = 1) +
  # Add slope at max BCR
  geom_abline(data = all_data_BCR_max, 
              aes(intercept = 0, slope = BCR_max), linewidth = 0.5) +
  geom_text(data = all_data_BCR_max,
            aes(x = 1350, y = 1000, label = round(BCR_max, 1)),
            inherit.aes = FALSE,
            size = 4) 
p

plot_details <- paste0("frontier_faceted_", "maintain_vec_pop_", main_vec, "_K_variable_", this_K_variable_value)
filename <- paste0("output/SALT_TZ/Frontier_plots/", plot_details, ".pdf")
my_ggsave(plot = p, filename = filename, width = 9, height = 9)
################################################################################


################################################################################
# Examine frontier 
frontier_width <- 8
frontier_height <- 5
################################################################################

prevalence_filter <- 0.5
treat_prop_filter <- 0.6

best_by_scenario <- data_for_frontier_plots %>%
  filter(treat_prop < treat_prop_filter, 
         prevalence < prevalence_filter) %>%
  group_by(NW, K_variable, maintain_vector_pop, treatment_code) %>%
  slice_max(order_by = BCR_scenario, n = 1, with_ties = FALSE) %>%   # keep one row
  ungroup()

nrow(best_by_scenario)
glimpse(best_by_scenario)

##########################
test_vars <- c("prop_cattle_with_insecticide_factor",
               "Baseline_vector_host_ratio_factor",
               "Insecticide_strategy",
               "NW_factor", "drug_use_in_protocol", "Rres_gt_1") 

test_var_names <- c("Insecticide \ncoverage", "Vector host \nratio", 
                    "Insecticide \nstrategy", "Wildlife", "Protocol", "Rres > 1")

for (i in 1:length(test_vars)){
  test_var <- test_vars[i]
  test_var_name <- test_var_names[i]
  p <- ggplot(data = best_by_scenario) +
    ggtitle(" ") +
    geom_point(aes(x = treat_insecticide_cost, 
                   y = sum_averted_production_losses,
                   colour = get(test_var)), size = 3) +
    labs(colour = test_var_name) +
    coord_cartesian(ylim = c(0, 1800), xlim = c(0, 1800)) +
    xlab(my_label("treat_insecticide_cost")) +
    ylab(my_label("sum_averted_production_losses")) +
    my_theme_SALT_TZ() + 
    # Add shading for regions of BCR < 2.5 and < 1
    geom_ribbon(data = df_slope_1, aes(x = x, ymin = 0, ymax = y),
                fill = "grey40", alpha = 0.3) +
    geom_ribbon(data = df_slope_2.5, aes(x = x, ymin = 0, ymax = y),
                fill = "grey60", alpha = 0.3) 
  print(p)
  
  plot_details <- paste0("frontier_", "treat_prop_filter_", treat_prop_filter, "prev_filter_", prevalence_filter, "_", test_var)
  filename <- paste0("output/SALT_TZ/Frontier_plots/", plot_details, ".pdf")
  my_ggsave(plot = p, filename = filename, 
            width = frontier_width, height = frontier_height)
}

##############################################################
