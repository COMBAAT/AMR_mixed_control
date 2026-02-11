################################################################################
# Trend plots of BCR against wildlife
################################################################################
trend_width <- 8 
trend_height <- 5
pad_title <- function(x, n = 20) {
  sprintf("%-*s", n, x)
}

# ################################################################################
filtered_by_insecticide <- data_with_K_selection %>%
  filter(!near(prop_cattle_with_insecticide, 0.15),
         !near(prop_cattle_with_insecticide, 0.25),
         !near(prop_cattle_with_insecticide, 0.35),
         !near(prop_cattle_with_insecticide, 0.45),
         !near(prop_cattle_with_insecticide, 0.55))

# ################################################################################
prevalence_filter <- 0.5
Rres_filter <- 10
BCR_filter <- 1.0
treat_prop_filter <- 0.6

all_filtered <- filtered_by_insecticide %>%
  filter(treat_prop < treat_prop_filter, 
         prevalence < prevalence_filter,
         BCR_scenario > BCR_filter,
         Rres_final < Rres_filter)

p <- all_filtered %>% 
  ggplot() +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1,
            fill = "grey70", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 1, ymax = 2.5,
            fill = "grey90", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(aes(x = NW_jitter, y = BCR_scenario,
                 colour = Baseline_vector_host_ratio_factor), size = 2) +
  labs(colour = pad_title("Vector host ratio", 25)) +
  ylab(my_label("BCR_scenario")) + xlab(my_label("NW")) +
  scale_x_continuous(breaks = c(0, 100, 200)) +
  my_theme_SALT_TZ()
p

plot_details <- paste0("trend_vs_wildlife_no_facet_by_Baseline_vector_host_ratio_factor_", 
                       "BCR_gt_", BCR_filter,
                       "_Rres_lt_", Rres_filter, " _prev_lt_", prevalence_filter)
filename <- paste0("output/SALT_TZ/Trend_plots/", plot_details, ".pdf")
my_ggsave(plot = p, filename = filename, 
          width = trend_width, height = trend_height)

# ################################################################################
prevalence_filter <- 0.5
Rres_filter <- 10
BCR_filter <- 2.5
treat_prop_filter <- 0.6

all_filtered <- filtered_by_insecticide %>%
  filter(treat_prop < treat_prop_filter, 
         prevalence < prevalence_filter,
         BCR_scenario > BCR_filter,
         Rres_final < Rres_filter)

p <- all_filtered %>% 
  ggplot() +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1,
            fill = "grey70", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 1, ymax = 2.5,
            fill = "grey90", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(aes(x = NW_jitter, y = BCR_scenario,
                 colour = Baseline_vector_host_ratio_factor), size = 2) +
  labs(colour = pad_title("Vector host ratio", 25)) +
  ylab(my_label("BCR_scenario")) + xlab(my_label("NW")) +
  scale_x_continuous(breaks = c(0, 100, 200)) +
  my_theme_SALT_TZ()
p

plot_details <- paste0("trend_vs_wildlife_no_facet_by_Baseline_vector_host_ratio_factor_", 
                       "BCR_gt_", BCR_filter,
                       "_Rres_lt_", Rres_filter, " _prev_lt_", prevalence_filter)
filename <- paste0("output/SALT_TZ/Trend_plots/", plot_details, ".pdf")
my_ggsave(plot = p, filename = filename, 
          width = trend_width, height = trend_height)

############################################
p <- all_filtered %>% 
  ggplot() +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1,
            fill = "grey70", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 1, ymax = 2.5,
            fill = "grey90", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(aes(x = NW_jitter, y = BCR_scenario,
                 colour = Baseline_vector_host_ratio_factor), size = 2) +
  labs(colour = pad_title("Vector host ratio", 25)) +
  ylab(my_label("BCR_scenario")) + xlab(my_label("NW")) +
  scale_x_continuous(breaks = c(0, 100, 200)) +
  facet_wrap(~ Insecticide_strategy) +
  my_theme_SALT_TZ()
p

plot_details <- paste0("trend_vs_wildlife_facet_strategy_by_Baseline_vector_host_ratio_factor_", 
                       "BCR_gt_", BCR_filter,
                       "_Rres_lt_", Rres_filter, " _prev_lt_", prevalence_filter)
filename <- paste0("output/SALT_TZ/Trend_plots/", plot_details, ".pdf")
my_ggsave(plot = p, filename = filename, 
          width = trend_width, height = trend_height)


############################################
best_by_scenario3 <- all_filtered %>%
  group_by(NW, K_variable, maintain_vector_pop) %>%
  slice_max(order_by = BCR_scenario, n = 1, with_ties = FALSE) %>%   # keep one row
  ungroup()

nrow(best_by_scenario3)

p <- best_by_scenario3 %>% ggplot() +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1,
            fill = "grey70", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(aes(x = NW, y = BCR_scenario, 
                 colour = Insecticide_strategy, shape = Baseline_vector_host_ratio_factor)) +
  scale_colour_manual(values = c("#D55E00", "#0072B2")) +
  scale_shape_manual(values = c(1, 4, 15)) +
  labs(shape = "Vector host ratio", colour = "Insecticide strategy") +
  ylab(my_label("BCR_scenario")) + xlab(my_label("NW")) +
  my_theme_SALT_TZ() 
p

plot_details <- paste0("trend_vs_wildlife_colour_by_Insecticide_strategy_", 
                       "BCR_gt_", BCR_filter,
                       "_Rres_lt_", Rres_filter, " _prev_lt_", prevalence_filter)
filename <- paste0("output/SALT_TZ/Trend_plots/", plot_details, ".pdf")
my_ggsave(plot = p, filename = filename, 
          width = trend_width, height = trend_height)

max(all_filtered$Rres_final)
max(all_filtered$prevalence)

# ################################################################################
prevalence_filter <- 0.1
Rres_filter <- 1
BCR_filter <- 2.5
treat_prop_filter <- 0.6

all_filtered <- filtered_by_insecticide %>%
  filter(treat_prop < treat_prop_filter, 
         prevalence < prevalence_filter,
         BCR_scenario > BCR_filter,
         Rres_final < Rres_filter)

p <- all_filtered %>% 
  ggplot() +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1,
            fill = "grey70", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 1, ymax = 2.5,
            fill = "grey90", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(aes(x = NW_jitter, y = BCR_scenario,
                 colour = Baseline_vector_host_ratio_factor), size = 2) +
  facet_wrap(~ Insecticide_strategy) +
  labs(colour = pad_title("Vector host ratio", 25)) +
  ylab(my_label("BCR_scenario")) + xlab(my_label("NW")) +
  scale_x_continuous(breaks = c(0, 100, 200)) +
  my_theme_SALT_TZ()
p

plot_details <- paste0("trend_vs_wildlife_facet_strategy_by_Baseline_vector_host_ratio_factor_", 
                       "Rres_lt_", Rres_filter, " _prev_lt_", prevalence_filter)
filename <- paste0("output/SALT_TZ/Trend_plots/", plot_details, ".pdf")
my_ggsave(plot = p, filename = filename, 
          width = trend_width, height = trend_height)


p <- all_filtered %>% 
  ggplot() +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1,
            fill = "grey70", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 1, ymax = 2.5,
            fill = "grey90", alpha = 0.3, inherit.aes = FALSE) +
  # Add shading for regions of BCR < 2.5 and < 1
  geom_point(aes(NW_jitter, BCR_scenario, 
                 colour = prop_cattle_with_insecticide_factor),
             alpha = 0, show.legend = TRUE) +
  geom_point(aes(x = NW_jitter, y = BCR_scenario,
                 colour = prop_cattle_with_insecticide_factor), size = 2) +
  facet_wrap(~ Insecticide_strategy) +
  labs(colour = pad_title(" Insecticide coverage", 21)) +
  ylab(my_label("BCR_scenario")) + xlab(my_label("NW")) +
  scale_x_continuous(breaks = c(0, 100, 200)) +
  #scale_colour_manual(values = cols) +
  my_theme_SALT_TZ() 
p

plot_details <- paste0("trend_vs_wildlife_facet_strategy_by _insecticide_", 
                       "Rres_lt_", Rres_filter, " _prev_lt_", prevalence_filter)
filename <- paste0("output/SALT_TZ/Trend_plots/", plot_details, ".pdf")
my_ggsave(plot = p, filename = filename, 
          width = trend_width, height = trend_height)



p3 <- all_filtered %>% 
  ggplot() +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1,
            fill = "grey70", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 1, ymax = 2.5,
            fill = "grey90", alpha = 0.3, inherit.aes = FALSE) +
  
  # 1) all points, faded (NO legend from this layer)
  geom_point(aes(x = NW_jitter, y = BCR_scenario),
             colour = "white",
             size = 2, alpha = 1.0, show.legend = FALSE) +
  
  # 2) highlighted points, strong (legend comes from here)
  geom_point(data = dplyr::filter(all_filtered, proph_ongoing > 0),
             aes(x = NW_jitter, y = BCR_scenario,
                 colour = prop_cattle_with_insecticide_factor),
             size = 2, alpha = 1, show.legend = TRUE) +
  
  facet_wrap(~ Insecticide_strategy) +
  labs(colour = pad_title(" Insecticide coverage", 21)) +
  ylab(my_label("BCR_scenario")) + xlab(my_label("NW")) +
  scale_x_continuous(breaks = c(0, 100, 200)) +
  my_theme_SALT_TZ()

p3

plot_details <- paste0("trend_vs_wildlife_facet_strategy_by _insecticide_", 
                       "Rres_lt_", Rres_filter, " _prev_lt_", prevalence_filter, "_proph_ongoing_gt_0")
filename <- paste0("output/SALT_TZ/Trend_plots/", plot_details, ".pdf")
my_ggsave(plot = p3, filename = filename, 
          width = trend_width, height = trend_height)


# ################################################################################
