
get_desired_vec <- function() {
  desired_vec <- round( c(seq(0, 0.98, by = 0.02), 0.99), 3 )
  desired_vec
}

###############################################################################
plot_boundary_tidy_Rres <- function(
    plot_this_filtered, variable, variable_thresh,
    num_rows = 3
) {
  
  desired_vec <- get_desired_vec()
  
  plot_this <- plot_this_filtered %>% 
    #filter(.data[[variable]] < variable_thresh) %>%
    filter(round(prop_cattle_with_insecticide, 3) %in% desired_vec)
  
  plot_no_drugs <- plot_this %>% filter(drug_treatment == 0)
  
  plot_minimum_prevalence <- plot_this %>% filter(Rres_final < 1) %>% 
    group_by(prop_cattle_with_insecticide, facet_label) %>% 
    #filter(max(prevalence_new) > 0) %>%
    slice_min(order_by = prevalence_new)
  
  plot_minimum_prevalence_reduced <- plot_minimum_prevalence %>%
    filter(prevalence > 1e-6)
    #filter(R0sen_final > 1)
  
  plot_this3 <- plot_this %>% 
    filter(.data[[variable]] < variable_thresh)
  
  
  ggplot() +
    geom_point(
      data = plot_this3,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        colour = Rres_final
      ), size = 2
    ) +
    
    geom_line(
      data = plot_minimum_prevalence_reduced,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        linetype = "Rres = 1"
      ),
      #colour = "#619CFF",
      colour = "black",
      linewidth = 1.5
    ) +
    geom_point(
      data = plot_no_drugs,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        shape = "No drug \ntreatment"
      ),
      fill = "white", 
      colour = "#7F7F7F",
      #colour = "orange",
      size = 2.0, stroke = 1
    ) +
    facet_wrap(~facet_label, nrow = num_rows) +
    scale_shape_manual(
      name = "",
      values = c(
        "No drug \ntreatment" = 21
      )
    ) +
    scale_linetype_manual(
      name = "Resistance spread\nthreshold",
      values = c("Rres = 1" = "solid")
    ) +
    scale_colour_gradientn(
    colours = c(
      "#D9F0D3",  # pale green
      "#00A600",  # strong green
      "#FDE725",  # yellow
      "#FDAE61",  # orange
      "#D95F8D",  # rose
      "#7B3294"   # purple
    ),
  values = scales::rescale(c(0, 1, 1.01, 2, 5)),
  limits = c(0, 5),
  name = "Capacity for\nresistance spread\nRres"
    ) +
    labs(
      x = my_label("prop_cattle_with_insecticide"),
      y = my_label("prevalence_new"),
    ) +
    guides(
      linetype = guide_legend(
        order = 3,
        override.aes = list(
          colour = "black", ##619CFF",
          linewidth = 2.0
        )
      ),
      
      colour = guide_colourbar(order = 4, reverse = TRUE),
      shape = guide_legend(
        order = 1,
        override.aes = list(
          shape = 21,
          fill = "white",
          colour = "#7F7F7F",
          size = 4,
          stroke = 1.5
        )
      )
    ) +
    my_theme() +
    theme(
      panel.background = element_rect(fill = "#FAFAFA", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid       = element_blank(),
      legend.title = element_text(size = 16),
      legend.text  = element_text(size = 14)
    )
  
}


############################################

plot_boundary_tidy_RiskA <- function(
    plot_this_filtered, variable, variable_thresh,
    num_rows = 3
) {
  
  desired_vec <- get_desired_vec()
  
  plot_this <- plot_this_filtered %>% 
    #filter(.data[[variable]] < variable_thresh) %>%
    filter(round(prop_cattle_with_insecticide, 3) %in% desired_vec)
  
  
  plot_no_drugs <- plot_this %>% filter(drug_treatment == 0)
  plot_minimum_prevalence <- plot_this %>% filter(Rres_final < 1) %>% 
    group_by(prop_cattle_with_insecticide, facet_label) %>% slice_min(order_by = prevalence_new)
  
  plot_minimum_prevalence_reduced <- plot_minimum_prevalence %>%
    filter(prevalence > 1e-6)
    #filter(R0sen_final > 1)
  
  plot_this3 <- plot_this %>% 
    filter(.data[[variable]] < variable_thresh)
  
  ggplot() +
    geom_point(
      data = plot_this3,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        colour = RiskA
      ), size = 2
    ) +
    
    geom_line(
      data = plot_minimum_prevalence_reduced,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        linetype = "Rres = 1"
      ),
      colour = "black",#619CFF",
      linewidth = 1.5
    ) +
    geom_point(
      data = plot_no_drugs,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        shape = "No drug \ntreatment"
      ),
      colour = "#7F7F7F",
      fill = "white",
      size = 1.5, stroke = 1
    ) +
    facet_wrap(~facet_label, nrow = num_rows) +
    scale_shape_manual(
      name = "",
      values = c(
        "No drug \ntreatment" = 21
      )
    ) +
    scale_linetype_manual(
      name = "Resistance spread\nthreshold",
      values = c("Rres = 1" = "solid")
    ) +
    scale_colour_gradientn(
      colours = c(
          "#ABD9E9",
          "#74ADD1",
          "#FEE08B",
          "#FDAE61",
          "#7B3294"
        ),
        values = scales::rescale(c(0, 1, 2, 4, 6, 8)),
        limits = c(0, 8),
        name = "Selection opportunity"
    ) +
    labs(
      x = my_label("prop_cattle_with_insecticide"),
      y = my_label("prevalence_new"),
      #fill = "Max safe\ncase treatment\nproportion"
    ) +
    guides(
      linetype = guide_legend(
        order = 3,
        override.aes = list(
          colour = "black", ##619CFF",
          linewidth = 2.0
        )
      ),
      
      colour = guide_colourbar(order = 4, reverse = TRUE),
      
      shape = guide_legend(
        order = 1,
        override.aes = list(
          shape = 21,
          fill = "white",
          colour = "#7F7F7F",
          size = 4,
          stroke = 1.5
        )
      )
    ) +
    my_theme() +
    theme(
      panel.background = element_rect(fill = "#FAFAFA", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid       = element_blank(),
      legend.title = element_text(size = 16),
      legend.text  = element_text(size = 14)
    )
  
}


plot_boundary_tidy_Number_treated <- function(
    plot_this_filtered, variable, variable_thresh,
    num_rows = 3
) {
  
  desired_vec <- get_desired_vec()
  
  plot_this <- plot_this_filtered %>% 
    #filter(.data[[variable]] < variable_thresh) %>%
    filter(round(prop_cattle_with_insecticide, 3) %in% desired_vec)
  
  plot_no_drugs <- plot_this %>% filter(drug_treatment == 0)
  plot_minimum_prevalence <- plot_this %>% filter(Rres_final < 1) %>% 
    group_by(prop_cattle_with_insecticide, facet_label) %>% slice_min(order_by = prevalence_new)
  
  plot_minimum_prevalence_reduced <- plot_minimum_prevalence %>%
    filter(prevalence > 1e-6)
    #filter(R0sen_final > 1)
  
  plot_this3 <- plot_this %>% 
    filter(.data[[variable]] < variable_thresh)
  
  ggplot() +
    geom_point(
      data = plot_this3,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        colour = No_trt_cat
      ), size = 2
    ) +
    
    geom_line(
      data = plot_minimum_prevalence_reduced,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        linetype = "Rres = 1"
      ),
      colour = "black", ##619CFF",
      linewidth = 1.5
    ) +
    geom_point(
      data = plot_no_drugs,
      aes(
        x = prop_cattle_with_insecticide,
        y = prevalence,
        shape = "No drug \ntreatment"
      ),
      colour = "#7F7F7F",
      fill = "white",
      size = 1.75, stroke = 1
    ) +
    facet_wrap(~facet_label, nrow = num_rows) +
    scale_shape_manual(
      name = "",
      values = c(
        "No drug \ntreatment" = 21
      )
    ) +
    scale_linetype_manual(
      name = "Resistance spread\nthreshold",
      values = c("Rres = 1" = "solid")
    ) +
    scale_colour_gradientn(
      colours = c(
        "#DDEBFA",
        "#C77CFF",
        "#D95F8D",
        "#E76F51",
        "#C96A3D",
        "#F6A04D",
        "#FFF3B0"
      ),
      limits = c(0, 250),
      breaks = c(0, 50, 100, 150, 200, 250),
      labels = c("0", "50", "100", "150", "200", ">250"),
      oob = scales::squish,
      na.value = "#FFF3B0",
      name = "Number treated"
    ) +
    labs(
      x = my_label("prop_cattle_with_insecticide"),
      y = my_label("prevalence_new"),
    ) +
    guides(
      linetype = guide_legend(
        order = 3,
        override.aes = list(
          colour = "black", #"#619CFF",
          linewidth = 2.0
        )
      ),
      
      colour = guide_colourbar(order = 4, reverse = TRUE),
      
      shape = guide_legend(
        order = 1,
        override.aes = list(
          shape = 21,
          fill = "white",
          colour = "#7F7F7F",
          size = 4,
          stroke = 1.5
        )
      )
    ) +
    my_theme() +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid       = element_blank(),
      legend.title = element_text(size = 16),
      legend.text  = element_text(size = 14)
    )
  
}





