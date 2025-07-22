

# Plot R resistant/R sensitive versus wildlife
plot_type11_selective_advantage_by_insecticide <- function(df, this_NW, R0_threshold, this_vector_measure, this_vector_measure_value, lw = my_linewidth(), ps = my_pointsize()) {
  plot_this <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", this_vector_measure), as.factor) %>%
    filter(NW == this_NW, get(this_vector_measure) == this_vector_measure_value, 
           prop_cattle_with_insecticide %in% c(0, 0.1, 0.2, 0.3, 0.4, 0.5))
  plot_this2 <- plot_this %>% filter(R0sen_final < R0_threshold)
  
  p <- plot_this %>%
    ggplot(aes(treat_prop, ratio, colour = prop_cattle_with_insecticide, linetype = prop_cattle_with_insecticide, shape = NW)) +
    geom_segment(x = 0.0, y = 1.0, xend = 1.0, yend = 1.0, colour = "red", linewidth = 0.5) +
    geom_point(size = ps) +
    geom_line(linewidth = lw) +
    xlim(c(0,1)) + ylim(c(0, 5)) +
    xlab(my_label("treat_prop")) +
    ylab("Selective advantage to \n resistant strain") +
    labs(colour = my_label("prop_cattle_with_insecticide"), linetype = my_label("prop_cattle_with_insecticide"), shape = my_label("NW")) +
    my_theme() +
    # added grey out data subset
    #geom_point(data = plot_this2, aes(treat_prop, ratio), size = ps, colour = "grey", alpha = 1.0) +
    geom_line(data = plot_this2, aes(treat_prop, ratio, group = prop_cattle_with_insecticide), linetype = "solid", linewidth = 1, colour = "white", alpha = 1.0) 
  
  p
}

plot_type11_selective_advantage_by_NW <- function(df, this_insecticide, R0_threshold, this_vector_measure, this_vector_measure_value, lw = my_linewidth(), ps = my_pointsize()) {
  plot_this <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", this_vector_measure), as.factor) %>%
    filter(prop_cattle_with_insecticide == this_insecticide, get(this_vector_measure) == this_vector_measure_value)
  plot_this2 <- plot_this %>% filter(R0sen_final < R0_threshold)
  
  p <- plot_this %>%
    ggplot(aes(treat_prop, ratio, colour = NW, linetype = NW, shape = prop_cattle_with_insecticide)) +
    geom_segment(x = 0.0, y = 1.0, xend = 1.0, yend = 1.0, colour = "red", linewidth = 0.5) +
    geom_point(size = ps) +
    geom_line(linewidth = lw) +
    xlim(c(0,1)) + ylim(c(0,20)) +
    xlab(my_label("treat_prop")) +
    ylab("Selective advantage to \n resistant strain") +
    labs(colour = my_label("NW"), linetype = my_label("NW"), shape = my_label("prop_cattle_with_insecticide")) +
    my_theme() +
    # added grey out data subset
    #geom_point(data = plot_this2, aes(treat_prop, ratio), size = ps, colour = "grey") +
    geom_line(data = plot_this2, aes(treat_prop, ratio, group = NW), linetype = "solid", linewidth = 1, colour = "white") 
  
  p
}

plot_type12_yvar_by_NW_and_insectide <- function(df, y_var, ymax, this_NW, this_vector_measure, this_vector_measure_value) {
  insecticide_vector <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5) #unique(df$prop_cattle_with_insecticide)
  df$y <- df[, y_var]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)
  
  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "prop_cattle_with_insecticide"), as.factor) %>%
    filter(get(this_vector_measure) == this_vector_measure_value, prop_cattle_with_insecticide %in% insecticide_vector, NW == this_NW) %>%
    ggplot(aes(treat_prop, y, colour = prop_cattle_with_insecticide)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    coord_cartesian(ylim = c(0, ymax)) +
    labs(colour = my_label("prop_cattle_with_insecticide")) +
    my_theme()
  p
}

# functions for plotting
create_selective_advantage_combination_plots <- function(subset_for_plotting, this_NW, this_insecticide, R0_threshold, label, plot_title, plot_choice,
                                                         this_vector_measure, this_vector_measure_value) {
  if (plot_choice == "by_NW") {
    p1a <- plot_type11_selective_advantage_by_NW(subset_for_plotting, this_insecticide, R0_threshold, this_vector_measure, this_vector_measure_value, lw = 0.7, ps = 2)
    p1b <- plot_type11_selective_advantage_by_NW(subset_for_plotting, this_insecticide, R0_threshold, this_vector_measure, this_vector_measure_value)
  }
  if (plot_choice == "by_insecticide") {
    p1a <- plot_type11_selective_advantage_by_insecticide(subset_for_plotting, this_NW, R0_threshold, this_vector_measure, this_vector_measure_value, lw = 0.7, ps = 2)
    p1b <- plot_type11_selective_advantage_by_insecticide(subset_for_plotting, this_NW, R0_threshold, this_vector_measure, this_vector_measure_value)
  }
  
  p1b <- p1b + ggtitle(plot_title) +
    geom_rect(aes(xmin = 0.0, xmax = 0.5, ymin = 0.0, ymax = 2.0),
              fill = "transparent", color = "black", linewidth = 0.5, linetype = "dashed"
    )
  p1b
  
  
  p1c <- p1a +
    coord_cartesian(ylim = c(0.0, 1.5), xlim = c(0, 0.5)) +
    geom_segment(
      x = 0.0, y = 0.65, xend = 0.0, yend = 0.95, colour = "grey20", linewidth = 0.75,
      arrow = arrow(length = unit(0.03, "npc"), ends = "both")
    ) +
    theme(legend.position="none")
  p1c
  # add text annotation to arrow
  p1c_v <- p1c + annotate("text", x = 0.22, y = 0.8, label = "fitness cost", size = 5, colour = "grey20")
  p1c_v
  p1_vertical <- p1b / p1c_v + plot_layout(nrow = 2, guides = "collect", axis_titles = "collect") +
    plot_annotation("A", caption = " ")
  p1_vertical
  
  p1c_inset <- p1c + annotate("text", x = 0.02, y = 0.82, label = "fitness cost", size = 4, colour = "grey20", hjust = 0.0)
  
  p1d <- p1c_inset +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      #axis.text.x = element_blank(),
      #axis.text.y = element_blank(),
      #axis.ticks.x = element_blank(),
      #axis.ticks.y = element_blank()
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 7)
    )
  p1_with_inset <- p1b + inset_element(p1d, 0.02, 0.32, 0.74, 0.97) + plot_layout(guides = "collect")
  p1_with_inset
  
  list(p1_vertical, p1_with_inset)
}
