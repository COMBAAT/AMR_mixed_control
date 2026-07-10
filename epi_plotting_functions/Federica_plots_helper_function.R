library(ggplot2)
library(ggpattern)
library(ggtext)

########################################################
# Helper functions
########################################################

make_plot_merged <- function(
    plot_this3,
    variable_name,
    group_vars = c("facet_label"),
    Rres_threshold = 1
) {
  
  group_cols <- c("prop_cattle_with_insecticide", group_vars)
  
  plot_boundary <- plot_this3 %>%
    filter(Rres_final < Rres_threshold) %>% 
    group_by(across(all_of(group_cols))) %>% 
    slice_min(order_by = prevalence_new, with_ties = FALSE) %>%
    ungroup()
  
  plot_maximum_prevalence <- plot_this3 %>% 
    group_by(across(all_of(group_cols))) %>%
    slice_max(order_by = prevalence_new, with_ties = FALSE) %>%
    ungroup()
  
  plot_minimum_prevalence <- plot_this3 %>% 
    group_by(across(all_of(group_cols))) %>%
    slice_min(order_by = prevalence_new, with_ties = FALSE) %>%
    ungroup()
  
  plot_merged <- plot_boundary %>%
    select(
      all_of(group_cols),
      boundary_prevalence = prevalence_new,
      boundary_value = all_of(variable_name)
    ) %>%
    left_join(
      plot_maximum_prevalence %>%
        select(all_of(group_cols), upper_prevalence = prevalence_new),
      by = group_cols
    ) %>%
    left_join(
      plot_minimum_prevalence %>%
        select(all_of(group_cols), lower_prevalence = prevalence_new),
      by = group_cols
    ) %>%
    rename(prop_insecticide = prop_cattle_with_insecticide) %>%
    mutate(prop_insecticide_percent = 100 * prop_insecticide)
  
  return(plot_merged)
}

make_plot_merged_old <- function(plot_this3, variable_name, Rres_threshold = 1) {
  
  plot_boundary <- plot_this3 %>%
    filter(Rres_final < Rres_threshold) %>% 
    group_by(prop_cattle_with_insecticide, facet_label) %>% 
    slice_min(order_by = prevalence_new, with_ties = FALSE) %>%
    arrange(prop_cattle_with_insecticide)
  
  plot_maximum_prevalence <- plot_this3 %>% 
    group_by(prop_cattle_with_insecticide, facet_label) %>%
    slice_max(order_by = prevalence_new, with_ties = FALSE) %>%
    arrange(prop_cattle_with_insecticide)
  
  plot_minimum_prevalence <- plot_this3 %>% 
    group_by(prop_cattle_with_insecticide, facet_label) %>%
    slice_min(order_by = prevalence_new, with_ties = FALSE) %>%
    arrange(prop_cattle_with_insecticide)
  
  plot_merged <- data.frame(
    #NW = plot_boundary$NW,
    boundary_prevalence = plot_boundary$prevalence_new,
    upper_prevalence = plot_maximum_prevalence$prevalence_new,
    lower_prevalence = plot_minimum_prevalence$prevalence_new,
    prop_insecticide = plot_boundary$prop_cattle_with_insecticide,
    boundary_value = plot_boundary[[variable_name]],
    facet_label = plot_boundary$facet_label
  ) %>%
    mutate(prop_insecticide_percent = 100 * prop_insecticide)
  
  return(plot_merged)
}

make_safe_plot_original <- function(plot_merged, plot_this3, fill_label, 
                                    fill_as_factor = FALSE,
                                    facet_type = "wrap") {
  
  #plot_this3_Rres_gte_1 <- plot_this3 %>% filter(Rres_final >= 1)
  
  plot_merged_reduced <- plot_merged %>%
    filter(boundary_prevalence > 1e-6)
  
  if (fill_as_factor) {
    plot_merged_reduced <- plot_merged_reduced %>%
      mutate(boundary_value = as.factor(boundary_value))
  }
  
  zone_label1 <- "Recommended"
  zone_label2 <- "May promote resistance"
  
  formatted_title <- "<span style='color:green4;'>Recommended</span> combinations of interventions to <br> reduce prevalence
 and <span style='color:green4;'>limit the spread of resistance </span>."
  
  p <- plot_merged %>%
    ggplot() +
    geom_ribbon(
      aes(
        x = prop_insecticide_percent,
        ymin = boundary_prevalence,
        ymax = upper_prevalence,
        alpha = zone_label1
      ),
      fill = "green4"
    ) +
    geom_ribbon(
      aes(
        x = prop_insecticide_percent,
        ymin = lower_prevalence,
        ymax = boundary_prevalence,
        alpha = zone_label2
      ),
      fill = "grey80"
    ) +
    geom_point(
      aes(
        x = prop_insecticide_percent,
        y = upper_prevalence,
        shape = "Insecticide only"
      ),
      colour = "green4",
      fill = "white",
      stroke = 1
    ) +
    geom_point(
      data = plot_merged_reduced,
      aes(
        x = prop_insecticide_percent,
        y = boundary_prevalence,
        fill = boundary_value
      ),
      shape = 24,
      colour = "black"
    ) +
    scale_alpha_manual(
      values = setNames(c(1, 1), c(zone_label1, zone_label2))
    ) +
    scale_shape_manual(
      values = setNames(c(21), c("Insecticide only"))
    ) +
    labs(
      x = "Cattle with insecticide coverage (%)",
      y = "Prevalence\nachieved\nwith\nintegrated\ncontrol",
      fill = fill_label,
      alpha = "Intervention outcome",
      shape = NULL
    ) +
    guides(
      fill = guide_legend(order = 3),
      alpha = guide_legend(order = 1),
      shape = guide_legend(order = 2)
    ) +
    ggtitle(formatted_title) +
    #facet_wrap(~ facet_label) +
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
      )
    ) +
    xlim(0, 100) + ylim(0, 0.8)
  
  if (facet_type == "wrap") {
    p <- p + facet_wrap(~ facet_label)
  }
  
  if (facet_type == "grid") {
    p <- p + facet_grid(
      rows = vars(Fitness),
      cols = vars(Wildlife),
      labeller = labeller(
        Fitness = function(x) paste("Fitness\n", x),
        Wildlife = function(x) paste("Wildlife\n", x)
      )
    ) +
    theme(
        strip.placement = "outside",
        strip.text.y = element_text(angle = 0, face = "plain"),
        strip.text.x = element_text(face = "bold")
      )
  }
  
  return(p)
}



