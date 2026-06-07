library(ggplot2)
library(ggpattern)
library(ggtext)

for_plotting <- saved_simulations %>% 
  mutate(treat_percentage = 100 * treat_prop,
         No_trt_cat_curtailed = case_when(No_trt_cat > 100 ~ 105, TRUE ~ No_trt_cat),
         Number_treated = cut(No_trt_cat_curtailed, 
                              breaks = c(0, 10, seq(20, 105 + 20, by = 20)), include.lowest = TRUE),
         drug_treatment = treat_prop + treatments_per_year,
         Insecticide = as.factor(prop_cattle_with_insecticide),
         Wildlife = as.factor(NW),
         Strategy = as.factor(treatment_code),
         facet_label = paste0("Wildlife: ", NW, "\nInsecticide: ", Insecticide),
         prevalence = prevalence_new,
         Insecticide_strategy = case_when(maintain_vector_pop == FALSE ~ "Cooperative",
                                          maintain_vector_pop == TRUE ~ "Individual"))

# Define the full set of levels once, from the full plotting data
all_levels <- levels(for_plotting$Number_treated)

# Create a named palette once
colour_vals <- setNames(
  scales::hue_pal()(length(all_levels)),
  all_levels
)

df <- data.frame(
  level = names(colour_vals),
  x = seq_along(colour_vals)
)

ggplot(df, aes(x = x, y = 1, fill = level)) +
  geom_tile() +
  scale_fill_manual(values = colour_vals)

subset_for_plotting <- for_plotting %>% 
  filter(
    Baseline_vector_host_ratio == 20
  )


########################################################
# Plot safe treatment boundaries
########################################################

#########################################
#Responsive mode 
#########################################
this_insecticide_strategy <- "Cooperative"
this_insecticide_strategy <- "Individual"

plot_this1 <- subset_for_plotting %>%
  mutate(facet_label = case_when(laXbel == "proph_ongoing" ~ "Longlasting ongoing",
                                 laXbel == "responsive_curative" ~ "Responsive curative",
                                 laXbel == "responsive_longlasting" ~ "Responsive longlasting")) %>%
  mutate(facet_label = paste0(facet_label, "\nWildlife ", NW))

#########################################
plot_this_filtered <- plot_this1 %>% 
  filter(treatment_code != 3, 
         prop_cattle_with_insecticide <= 0.8, Insecticide_strategy == this_insecticide_strategy, NW == 100)


treat_prop_thresh <- 0.95
plot_this3 <- plot_this_filtered %>% filter(treat_prop < treat_prop_thresh)


plot_boundary <- plot_this3 %>% filter(Rres_final < 1) %>% 
  group_by(prop_cattle_with_insecticide, facet_label) %>% 
  slice_min(order_by = prevalence_new, with_ties = FALSE) %>% 
  select(prevalence_new, prop_cattle_with_insecticide, facet_label, treat_prop) %>%
  arrange(prop_cattle_with_insecticide)
plot_maximum_prevalence <- plot_this3 %>% 
  group_by(prop_cattle_with_insecticide, facet_label) %>% slice_max(order_by = prevalence_new, with_ties = FALSE) %>%
  select(prevalence_new, prop_cattle_with_insecticide, facet_label) %>%
  arrange(prop_cattle_with_insecticide)
plot_minimum_prevalence2 <- plot_this3 %>% 
  group_by(prop_cattle_with_insecticide, facet_label) %>% slice_min(order_by = prevalence_new, with_ties = FALSE) %>% 
  select(prevalence_new, prop_cattle_with_insecticide, facet_label) %>%
  arrange(prop_cattle_with_insecticide)

plot_merged <- data.frame(boundary_prevalence = plot_boundary$prevalence_new, 
                          upper_prevalence = plot_maximum_prevalence$prevalence_new,
                          lower_prevalence = plot_minimum_prevalence2$prevalence_new,
                          prop_insecticide = plot_boundary$prop_cattle_with_insecticide,
                          boundary_treat_prop = plot_boundary$treat_prop,
                          facet_label = plot_boundary$facet_label) %>%
  mutate(prop_insecticide_percent = 100 * prop_insecticide)

plot_merged_reduced <- plot_merged %>% filter(boundary_prevalence > 1e-6)

head(plot_merged, 20)



 zone_label1 <- "Recommended"
 zone_label2 <- "May promote resistance"
 #formatted_title <- "This is <span style='color:red;'>dog</span>"
 formatted_title <- "Interventions that <span style='color:green;'>limit resistance</span> versus <br>interventions
 that can <span style='color:cyan;'>promote resistanceX</span>."
 formatted_title <- "<span style='color:green4;'>Recommended</span> combinations of interventions to <br> reduce prevalence
 and <span style='color:green4;'>limit the spread of resistance </span>."

  p <- plot_merged %>% ggplot() +
    geom_ribbon(aes(x = prop_insecticide_percent, ymin = boundary_prevalence, ymax = upper_prevalence, alpha = zone_label1), 
                fill = "green4") +
    geom_ribbon(aes(x = prop_insecticide_percent, ymin = lower_prevalence, ymax = boundary_prevalence, alpha = zone_label2), 
                fill = "grey80") +
    geom_point(aes(x = prop_insecticide_percent, y = upper_prevalence, shape = "Insecticide only"),
               colour = "green4", fill = "white", stroke = 1) +
    geom_point(data = plot_merged_reduced, aes(x = prop_insecticide_percent, y = boundary_prevalence, fill = boundary_treat_prop),
               shape = 24, colour = "black") +
   # scale_fill_viridis_d(
   #   option = "plasma",
   #   begin = .4
   # ) +
   MetBrewer::scale_fill_met_c(
     name = "Hiroshige" 
   ) +
    scale_alpha_manual(values = setNames(
      c(1, 1),
      c(zone_label1, zone_label2)
      )) +
    scale_shape_manual(values = setNames(
      c(21),
      c("Insecticide only")
    )) +
    labs(
      x = "Cattle with insecticide coverage (%)",
      y = "Prevalence\nachieved\nwith\nintegrated\ncontrol",
      fill = "Max recommended\ncase treatment\nproportion",
      alpha = "Intervention outcome",
      shape = NULL
    ) +
    guides(
      fill = guide_legend(order = 3),
      alpha = guide_legend(order = 1),
      shape = guide_legend(order = 2)
    ) +
    ggtitle(formatted_title) +
  facet_wrap(~ facet_label) +
    my_theme() + 
   theme(
      legend.spacing.y = unit(0.3, "cm"),
      legend.key.height = unit(0.7, "cm"),
      legend.margin = margin(0, 0, 0, 0),
      plot.title = element_markdown(),
        axis.title.y = element_text(
          angle = 0,
          vjust = 0.5,
          hjust = 1
    ))

 ggsave("for_federica.pdf", p, height = 5, width = 10)
#############################################

 plot_this_filtered <- plot_this1 %>% 
   filter(treatment_code == 3, 
          NW == 100, prop_cattle_with_insecticide <= 0.8, Insecticide_strategy == this_insecticide_strategy)
 
 
 treatments_per_year_thresh <- 6
 plot_this3 <- plot_this_filtered %>% filter(treatments_per_year < treatments_per_year_thresh)
 
 
 plot_boundary <- plot_this3 %>% filter(Rres_final < 0.8/0.8) %>% 
   group_by(prop_cattle_with_insecticide, facet_label) %>% 
   slice_min(order_by = prevalence_new, with_ties = FALSE) %>% 
   select(prevalence_new, prop_cattle_with_insecticide, facet_label, treatments_per_year) %>%
   arrange(prop_cattle_with_insecticide)
 plot_maximum_prevalence <- plot_this3 %>% 
   group_by(prop_cattle_with_insecticide, facet_label) %>% slice_max(order_by = prevalence_new, with_ties = FALSE) %>%
   select(prevalence_new, prop_cattle_with_insecticide, facet_label) %>%
   arrange(prop_cattle_with_insecticide)
 plot_minimum_prevalence2 <- plot_this3 %>% 
   group_by(prop_cattle_with_insecticide, facet_label) %>% slice_min(order_by = prevalence_new, with_ties = FALSE) %>% 
   select(prevalence_new, prop_cattle_with_insecticide, facet_label) %>%
   arrange(prop_cattle_with_insecticide)
 
 plot_merged <- data.frame(boundary_prevalence = plot_boundary$prevalence_new, 
                           upper_prevalence = plot_maximum_prevalence$prevalence_new,
                           lower_prevalence = plot_minimum_prevalence2$prevalence_new,
                           prop_insecticide = plot_boundary$prop_cattle_with_insecticide,
                           boundary_treatments_per_year = plot_boundary$treatments_per_year,
                           facet_label = plot_boundary$facet_label) %>%
   mutate(prop_insecticide_percent = 100 * prop_insecticide)
 
 plot_merged_reduced <- plot_merged %>% filter(boundary_prevalence > 1e-6)
 
 head(plot_merged, 20)
 
 
 
 zone_label1 <- "Recommended"
 zone_label2 <- "May promote resistance"
 #formatted_title <- "This is <span style='color:red;'>dog</span>"
 formatted_title <- "Interventions that <span style='color:green;'>limit resistance</span> versus <br>interventions
 that can <span style='color:cyan;'>promote resistanceX</span>."
 formatted_title <- "<span style='color:green;'>Recommended </span> combinations of interventions to <br> reduce prevalence
 and <span style='color:green;'>limit the spread of resistance </span>."
 
 plot_merged %>% ggplot() +
   geom_ribbon(aes(x = prop_insecticide_percent, ymin = boundary_prevalence, ymax = upper_prevalence, alpha = zone_label1), 
               fill = "lightgreen") +
   geom_ribbon(aes(x = prop_insecticide_percent, ymin = lower_prevalence, ymax = boundary_prevalence, alpha = zone_label2), 
               fill = "grey80") +
   geom_point(aes(x = prop_insecticide_percent, y = upper_prevalence, shape = "Insecticide only"),
              colour = "darkgreen", fill = "white", stroke = 1) +
   geom_point(data = plot_merged_reduced, aes(x = prop_insecticide_percent, y = boundary_prevalence, fill = as.factor(boundary_treatments_per_year)),
              shape = 24, colour = "black") +
   scale_alpha_manual(values = setNames(
     c(1, 1),
     c(zone_label1, zone_label2)
   )) +
   scale_shape_manual(values = setNames(
     c(21),
     c("Insecticide only")
   )) +
   labs(
     x = "Cattle with insecticide coverage (%)",
     y = "Prevalence achieved with integrated control",
     fill = "Max safe \ntreatments \nper year",
     alpha = "Intervention outcome",
     shape = NULL
   ) +
   guides(
     fill = guide_legend(order = 3),
     alpha = guide_legend(order = 1),
     shape = guide_legend(order = 2)
   ) +
   ggtitle(formatted_title) +
   facet_wrap(~ facet_label) +
   my_theme() + theme(
     legend.spacing.y = unit(0.3, "cm"),
     legend.key.height = unit(0.7, "cm"),
     legend.margin = margin(0, 0, 0, 0),
     plot.title = element_markdown()
   )
 

