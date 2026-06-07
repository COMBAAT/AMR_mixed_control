plot_boundary_responsive_play <- function(
    plot_this_filtered,
    treat_prop_thresh,
    all_levels,
    colour_vals
) {
  
  plot_this3 <- plot_this_filtered %>% filter(treat_prop < treat_prop_thresh)
  plot_no_drugs <- plot_this3 %>% filter(drug_treatment == 0)
  plot_as_white_crosses <- plot_this3 %>% filter(Rres_final > 1)
  #plot_minimum_prevalence <- plot_this3 %>% filter(Rres_final < 1) %>% 
  #  group_by(prop_cattle_with_insecticide, facet_label) %>% slice_min(order_by = prevalence_new)
  
  max_insecticide_without_complete_control <- plot_no_drugs %>%
    group_by(prop_cattle_with_insecticide, facet_label)
    
  plot_minimum_prevalence <- plot_this3 %>% filter(Rres_final < 1.0) %>% 
    group_by(prop_cattle_with_insecticide, facet_label) %>% slice_min(order_by = prevalence_new) 
  
  
  plot_minimum_prevalence_reduced <- plot_minimum_prevalence %>%
    filter(R0sen_final > 1)
  
  Treatment_title = "Max safe\ncase treatment\nproportion"
  
  ggplot() +
    geom_point(
      data = plot_this3,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        #colour = as.factor(Number_treated)
        colour = Number_treated
      )
    ) +
    geom_point(
      data = plot_as_white_crosses,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        shape = "Rres > 1"
      ),
      colour = "white",
      size = 1
    ) +
    geom_point(
      data = plot_no_drugs,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        shape = "No drugs"
      ),
      colour = "white",
      size = 1
    ) +
    geom_line(
      data = plot_minimum_prevalence_reduced,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        linetype = "Rres = 1"
      ),
      colour = "#619CFF",
      linewidth = 1.5
    ) +
    geom_point(
      data = plot_minimum_prevalence_reduced,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        fill = as.factor(treat_prop)#,
        #size = Number_treated
      ),
      shape = 24,
      colour = "black"
    ) +
    facet_wrap(~facet_label, nrow = 2) +
    scale_shape_manual(
      name = "Highlight",
      values = c(
        "Rres > 1" = 4,
        "No drugs" = 16
      )
    ) +
    scale_linetype_manual(
      name = "Boundary",
      values = c("Rres = 1" = "solid")
    ) +
    scale_colour_manual(
      values = colour_vals,
      #limits = all_levels,
      breaks = all_levels,
      #drop = FALSE,
      name = "Number treated"
    ) +
    labs(
      x = my_label("prop_cattle_with_insecticide"),
      y = my_label("prevalence_new"),
      fill = "Max safe\ncase treatment\nproportion"
    ) +
    guides(
      shape = guide_legend(
        order = 1,
        override.aes = list(
          size = 4,
          stroke = 1.5,
          colour = "white"
        )
      ),
      linetype = guide_legend(
        order = 2,
        override.aes = list(
          colour = "#619CFF",
          linewidth = 1.5
        )
      )
    ) +
    my_theme() +
    theme(
      panel.background = element_rect(fill = "grey95"),
      legend.key = element_rect(fill = "grey90"),
      legend.spacing.y = unit(0.1, "mm")
    ) 
  
}


############################################

plot_boundary_responsive_tidy <- function(
    plot_this_filtered,
    treat_prop_thresh,
    num_rows = 3
) {
  
  plot_this3 <- plot_this_filtered %>% filter(treat_prop < treat_prop_thresh)
  plot_no_drugs <- plot_this3 %>% filter(drug_treatment == 0)
  plot_as_white_crosses <- plot_this3 %>% filter(Rres_final > 1)
  plot_minimum_prevalence <- plot_this3 %>% filter(Rres_final < 1) %>% 
    group_by(prop_cattle_with_insecticide, facet_label) %>% slice_min(order_by = prevalence_new)
  
  plot_minimum_prevalence_reduced <- plot_minimum_prevalence %>%
    #filter(prevalence > 1e-6)
    filter(R0sen_final > 1)
  
  Treatment_title = "Max safe\ncase treatment\nproportion"
  
  ggplot() +
    geom_point(
      data = plot_this3,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        colour = RiskA
      )
    ) +
    geom_point(
      data = plot_as_white_crosses,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        shape = "Rres > 1"
      ),
      colour = "white",
      size = 1
    ) +
    geom_point(
      data = plot_no_drugs,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        shape = "No drugs"
      ),
      colour = "white",
      size = 1
    ) +
    geom_line(
      data = plot_minimum_prevalence_reduced,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        linetype = "Rres = 1"
      ),
      colour = "#619CFF",
      linewidth = 1.5
    ) +
    geom_point(
      data = plot_minimum_prevalence_reduced,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        fill = as.factor(treat_prop)
      ),
      shape = 24,
      colour = "black"
    ) +
    facet_wrap(~facet_label, nrow = num_rows) +
    scale_shape_manual(
      name = "Highlight",
      values = c(
        "Rres > 1" = 4,
        "No drugs" = 16
      )
    ) +
    scale_linetype_manual(
      name = "Boundary",
      values = c("Rres = 1" = "solid")
    ) +
    scale_colour_gradientn(
      colours = terrain.colors(15),
      limits = c(0, 10),
      name = "Selection\nopportunity"
    ) +
    labs(
      x = my_label("prop_cattle_with_insecticide"),
      y = my_label("prevalence_new"),
      fill = "Max safe\ncase treatment\nproportion"
    ) +
    guides(
      shape = guide_legend(
        order = 1,
        override.aes = list(
          size = 4,
          stroke = 1.5,
          colour = "white"
        )
      ),
      linetype = guide_legend(
        order = 2,
        override.aes = list(
          colour = "#619CFF",
          linewidth = 1.5
        )
      )
    ) +
    my_theme() +
    theme(
      panel.background = element_rect(fill = "grey95"),
      legend.key = element_rect(fill = "grey90")
    ) 
  
}


plot_boundary_ongoing_tidy <- function(
    plot_this_filtered,
    treat_freq_thresh,
    num_rows = 3
) {
  
  plot_this3 <- plot_this_filtered %>% filter(treatments_per_year < treat_freq_thresh)
  
  plot_no_drugs <- plot_this3 %>% filter(drug_treatment == 0)
  plot_as_white_crosses <- plot_this3 %>% filter(Rres_final > 1)
  plot_minimum_prevalence <- plot_this3 %>% filter(Rres_final < 1) %>% 
    group_by(prop_cattle_with_insecticide, facet_label) %>% slice_min(order_by = prevalence_new)
  plot_minimum_prevalence_reduced <- plot_minimum_prevalence %>%
    #filter(prevalence > 1e-6)
   filter(R0sen_final > 1)
  
  ggplot() +
    geom_point(
      data = plot_this3,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        colour = RiskA
      )
    ) +
    geom_point(
      data = plot_as_white_crosses,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        shape = "Rres > 1"
      ),
      colour = "white",
      size = 1
    ) +
    geom_point(
      data = plot_no_drugs,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        shape = "No drugs"
      ),
      colour = "white",
      size = 1
    ) +
    geom_line(
      data = plot_minimum_prevalence_reduced,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        linetype = "Rres = 1"
      ),
      colour = "#619CFF",
      linewidth = 1.5
    ) +
    geom_point(
      data = plot_minimum_prevalence_reduced,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        fill = as.factor(treat_prop)
      ),
      shape = 24,
      colour = "black",
      size = 2
    ) +
    facet_wrap(~facet_label, nrow = num_rows) +
    scale_shape_manual(
      name = "Highlight",
      values = c(
        "Rres > 1" = 4,
        "No drugs" = 16
      )
    ) +
    scale_linetype_manual(
      name = "Boundary",
      values = c("Rres = 1" = "solid")
    ) +
    scale_colour_gradientn(
      colours = terrain.colors(15),
      limits = c(0, 10),
      name = "Selection\nopportunity"
    ) +
    labs(
      x = my_label("prop_cattle_with_insecticide"),
      y = my_label("prevalence_new"),
      fill = "Max safe\ncase treatments\nper year"
    ) +
    guides(
      shape = guide_legend(
        order = 1,
        override.aes = list(
          size = 4,
          stroke = 1.5,
          colour = "white"
        )
      ),
      linetype = guide_legend(
        order = 2,
        override.aes = list(
          colour = "#619CFF",
          linewidth = 1.5
        )
      )
    ) +
    my_theme() +
    theme(
      panel.background = element_rect(fill = "grey95"),
      legend.key = element_rect(fill = "grey90")
    ) 
  
}





plot_boundary_tidy <- function(
    plot_this3,
    plot_as_white_crosses,
    plot_no_drugs,
    plot_minimum_prevalence,
    treat_prop_thresh
) {
  
  plot_this3 <- plot_this3 %>% filter(treat_prop < treat_prop_thresh)
  plot_as_white_crosses <- plot_as_white_crosses %>% filter(treat_prop < treat_prop_thresh)
  plot_no_drugs <- plot_no_drugs %>% filter(treat_prop < treat_prop_thresh)
  plot_minimum_prevalence <- plot_minimum_prevalence %>% filter(treat_prop < treat_prop_thresh)
  
  if (3 %in% unique(plot_this3$treatment_code)) {
    Treatment_title = "Max safe\ntreatments\nper year"
  } else {
    Treatment_title = "Max safe\ncase treatment\nproportion"
  }
  
  plot_minimum_prevalence_reduced <- plot_minimum_prevalence %>%
    #filter(prevalence > 1e-6) %>% 
    filter(R0sen_final > 1) %>%
    mutate(fill_variable = case_when(treatment_code == 3 ~ treatments_per_year, 
                                     treatment_code %in% c(1, 2) ~ treat_prop))
  
  ggplot() +
    geom_point(
      data = plot_this3,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        colour = RiskA
      )
    ) +
    geom_point(
      data = plot_as_white_crosses,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        shape = "Rres > 1"
      ),
      colour = "white",
      size = 1
    ) +
    geom_point(
      data = plot_no_drugs,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        shape = "No drugs"
      ),
      colour = "white",
      size = 1
    ) +
    geom_line(
      data = plot_minimum_prevalence_reduced,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        linetype = "Rres = 1"
      ),
      colour = "#619CFF",
      linewidth = 1.5
    ) +
    geom_point(
      data = plot_minimum_prevalence_reduced,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        fill = as.factor(fill_variable)
      ),
      shape = 24,
      colour = "black",
      size = 2
    ) +
    facet_wrap(~facet_label, nrow = 3) +
    scale_shape_manual(
      name = "Highlight",
      values = c(
        "Rres > 1" = 4,
        "No drugs" = 16
      )
    ) +
    scale_linetype_manual(
      name = "Boundary",
      values = c("Rres = 1" = "solid")
    ) +
    scale_colour_gradientn(
      colours = terrain.colors(15),
      limits = c(0, 10),
      name = "Selection\nopportunity"
    ) +
    labs(
      x = my_label("prop_cattle_with_insecticide"),
      y = my_label("prevalence_new"),
      fill = Treatment_title
    ) +
    guides(
      shape = guide_legend(
        order = 1,
        override.aes = list(
          size = 4,
          stroke = 1.5,
          colour = "white"
        )
      ),
      linetype = guide_legend(
        order = 2,
        override.aes = list(
          colour = "#619CFF",
          linewidth = 1.5
        )
      )
    ) +
    my_theme() +
    theme(
      panel.background = element_rect(fill = "grey95"),
      legend.key = element_rect(fill = "grey90")
    ) 
    
}





plot_boundary_by_fitness <- function(test_all) {
  
  plot_minimum_prevalence <- test_all %>% filter(Rres_new < 1) %>%
    group_by(prop_cattle_with_insecticide, facet_label, fitness) %>% 
    slice_min(order_by = prevalence)
  
  plot_minimum_prevalence_reduced <- plot_minimum_prevalence %>%
    filter(R0sen_final > 1)
  
  plot_no_drugs <- test_all %>% filter(drug_treatment == 0)
  
  p <- ggplot() +
    facet_wrap(~ facet_label, nrow = 3) +
    geom_point(data = plot_minimum_prevalence_reduced, aes(y = prevalence, x = prop_cattle_with_insecticide, colour = fitness)) +
    geom_line(data = plot_minimum_prevalence_reduced, aes(y = prevalence, x = prop_cattle_with_insecticide, colour = fitness))  +
    #geom_point(data = plot_no_drugs, aes(y = y, x = prop_cattle_with_insecticide), colour = "grey50", size = 2.5) +
    geom_point(data = plot_no_drugs, aes(y = prevalence, x = prop_cattle_with_insecticide, 
                                         shape = "No drugs", fill = "No drugs"), size = 2, stroke = 1.5, colour = "grey70") +
    scale_shape_manual(name = "Highlight", values = c("No drugs" = 21)) +
    scale_fill_manual(name = "Highlight", values = c("No drugs" = "white")) +
    ylab(my_label("prevalence_new")) + xlab(my_label("prop_cattle_with_insecticide")) +
    my_theme() +
    labs(colour= "Rres = 1\nboundary\nby relative fitness") +
    theme(panel.background = element_rect(fill = "grey95"),
          legend.key = element_rect(fill = "grey90"))
  p
}


plot_boundary_by_wildlife <- function(plot_this2) {
  
  plot_minimum_prevalence <- plot_this2 %>% filter(Rres_final < 1) %>%
    group_by(prop_cattle_with_insecticide, facet_label, Wildlife) %>%
    slice_min(order_by = prevalence)
  
  plot_minimum_prevalence_reduced <- plot_minimum_prevalence %>%
    filter(R0sen_final > 1)
  
  plot_no_drugs <- plot_this2 %>% filter(drug_treatment == 0)
  
  p <- ggplot() +
    facet_wrap(~ facet_label, nrow = 3) +
    geom_point(data = plot_minimum_prevalence_reduced, aes(y = prevalence, x = prop_cattle_with_insecticide, colour = Wildlife)) +
    geom_line(data = plot_minimum_prevalence_reduced, aes(y = prevalence, x = prop_cattle_with_insecticide, colour = Wildlife))  +
    geom_line(data = plot_no_drugs, aes(y = prevalence, x = prop_cattle_with_insecticide, colour = Wildlife, linetype = "No drugs")) +
    geom_point(data = plot_no_drugs, aes(y = prevalence, x = prop_cattle_with_insecticide, 
                                         colour = Wildlife, shape = "No drugs", fill = "No drugs", stroke = 1.5)) +
    
    #geom_point(data = plot_no_drugs, aes(y = y, x = prop_cattle_with_insecticide), colour = "purple", size = 1) +
    ylab(my_label("prevalence_new")) + xlab(my_label("prop_cattle_with_insecticide")) +
    scale_shape_manual(
      name = "Highlight",
      values = c("No drugs" = 21)
    ) +
    scale_fill_manual(
      name = "Highlight",
      values = c("No drugs" = "white")
    ) +
    scale_linetype_manual(
      name = "Highlight",
      values = c("No drugs" = "dashed")
    ) +
    my_theme() +
    labs(colour= "Rres = 1\nboundary\nby wildlife") +
    theme(panel.background = element_rect(fill = "grey95"), 
          legend.key = element_rect(fill = "grey90")) +
    guides(
      shape = guide_legend(
        override.aes = list(
          size = 2,        # bigger cross
          stroke = 1.5,    # thicker lines
          colour = "white" # keep it white
        )
      )
    )
  p
}


plot_boundary_responsive_tidy_Incidence <- function(
    plot_this_filtered,
    treat_prop_thresh,
    num_rows = 3
) {
  
  plot_this3 <- plot_this_filtered %>% filter(treat_prop < treat_prop_thresh)
  plot_no_drugs <- plot_this3 %>% filter(drug_treatment == 0)
  plot_as_white_crosses <- plot_this3 %>% filter(Rres_final > 1)
  plot_minimum_Incidence <- plot_this3 %>% filter(Rres_final < 1) %>% 
    group_by(prop_cattle_with_insecticide, facet_label) %>% slice_min(order_by = Incidence)
  
  plot_minimum_Incidence_reduced <- plot_minimum_Incidence %>%
    #filter(prevalence > 1e-6)
    filter(R0sen_final > 1)
  
  Treatment_title = "Max safe\ncase treatment\nproportion"
  
  ggplot() +
    geom_point(
      data = plot_this3,
      aes(
        x = prop_cattle_with_insecticide,
        y = Incidence,
        colour = RiskA
      )
    ) +
    geom_point(
      data = plot_as_white_crosses,
      aes(
        x = prop_cattle_with_insecticide,
        y = Incidence,
        shape = "Rres > 1"
      ),
      colour = "white",
      size = 1
    ) +
    geom_point(
      data = plot_no_drugs,
      aes(
        x = prop_cattle_with_insecticide,
        y = Incidence,
        shape = "No drugs"
      ),
      colour = "white",
      size = 1
    ) +
    geom_line(
      data = plot_minimum_Incidence_reduced,
      aes(
        x = prop_cattle_with_insecticide,
        y = Incidence,
        linetype = "Rres = 1"
      ),
      colour = "#619CFF",
      linewidth = 1.5
    ) +
    geom_point(
      data = plot_minimum_Incidence_reduced,
      aes(
        x = prop_cattle_with_insecticide,
        y = Incidence,
        fill = as.factor(treat_prop)
      ),
      shape = 24,
      colour = "black"
    ) +
    facet_wrap(~facet_label, nrow = num_rows) +
    scale_shape_manual(
      name = "Highlight",
      values = c(
        "Rres > 1" = 4,
        "No drugs" = 16
      )
    ) +
    scale_linetype_manual(
      name = "Boundary",
      values = c("Rres = 1" = "solid")
    ) +
    scale_colour_gradientn(
      colours = terrain.colors(15),
      limits = c(0, 10),
      name = "Selection\nopportunity"
    ) +
    labs(
      x = my_label("prop_cattle_with_insecticide"),
      y = my_label("prevalence_new"),
      fill = "Max safe\ncase treatment\nproportion"
    ) +
    guides(
      shape = guide_legend(
        order = 1,
        override.aes = list(
          size = 4,
          stroke = 1.5,
          colour = "white"
        )
      ),
      linetype = guide_legend(
        order = 2,
        override.aes = list(
          colour = "#619CFF",
          linewidth = 1.5
        )
      )
    ) +
    my_theme() +
    theme(
      panel.background = element_rect(fill = "grey95"),
      legend.key = element_rect(fill = "grey90")
    ) 
  
}



plot_boundary_responsive_clean <- function(
    plot_this_filtered,
    treat_prop_thresh,
    num_rows = 3
) {
  
  plot_this3 <- plot_this_filtered %>% filter(treat_prop < treat_prop_thresh)
  plot_no_drugs <- plot_this3 %>% filter(drug_treatment == 0)
  plot_as_white_crosses <- plot_this3 %>% filter(Rres_final > 1)
  plot_minimum_prevalence <- plot_this3 %>% filter(Rres_final < 1) %>% 
    group_by(prop_cattle_with_insecticide, facet_label) %>% slice_min(order_by = prevalence_new)
  plot_maximum_prevalence <- plot_this3 %>% 
    group_by(prop_cattle_with_insecticide, facet_label) %>% slice_max(order_by = prevalence_new)
  plot_minimum_prevalence2 <- plot_this3 %>% 
    group_by(prop_cattle_with_insecticide, facet_label) %>% slice_min(order_by = prevalence_new)
  
  plot_minimum_prevalence_reduced <- plot_minimum_prevalence %>%
    #filter(prevalence > 1e-6)
    filter(R0sen_final > 1)
  
  Treatment_title = "Max safe\ncase treatment\nproportion"
  
  df_new <- data.frame(prev_max = plot)
  
  ggplot() +
    geom_point(data = plot_minimum_prevalence2,
               aes(
                 x = prop_cattle_with_insecticide,
                 y = prevalence
               ),
  colour = "red", size = 3
  ) +
    # geom_point(
    #   data = plot_this3,
    #   aes(
    #     x = prop_cattle_with_insecticide,
    #     y = prevalence,
    #     colour = RiskA
    #   )
    # ) +
    # geom_point(
    #   data = plot_as_white_crosses,
    #   aes(
    #     x = prop_cattle_with_insecticide,
    #     y = prevalence,
    #     shape = "Rres > 1"
    #   ),
    #   colour = "white",
    #   size = 1
    # ) +
    geom_point(
      data = plot_maximum_prevalence,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        shape = "No drugs"
      ),
      colour = "black",
      size = 1
    ) +
    geom_line(
      data = plot_minimum_prevalence_reduced,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        linetype = "Rres = 1"
      ),
      colour = "#619CFF",
      linewidth = 1.5
    ) +
    geom_point(
      data = plot_minimum_prevalence_reduced,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        fill = as.factor(treat_prop)
      ),
      shape = 24,
      colour = "black"
    ) +
    facet_wrap(~facet_label, nrow = num_rows) +
    scale_shape_manual(
      name = "Highlight",
      values = c(
        "Rres > 1" = 4,
        "No drugs" = 16
      )
    ) +
    scale_linetype_manual(
      name = "Boundary",
      values = c("Rres = 1" = "solid")
    ) +
    scale_colour_gradientn(
      colours = terrain.colors(15),
      limits = c(0, 10),
      name = "Selection\nopportunity"
    ) +
    labs(
      x = my_label("prop_cattle_with_insecticide"),
      y = my_label("prevalence_new"),
      fill = "Max safe\ncase treatment\nproportion"
    ) +
    guides(
      shape = guide_legend(
        order = 1,
        override.aes = list(
          size = 4,
          stroke = 1.5,
          colour = "white"
        )
      ),
      linetype = guide_legend(
        order = 2,
        override.aes = list(
          colour = "#619CFF",
          linewidth = 1.5
        )
      )
    ) +
    my_theme() +
    theme(
      panel.background = element_rect(fill = "grey95"),
      legend.key = element_rect(fill = "grey90")
    ) 
  
}
