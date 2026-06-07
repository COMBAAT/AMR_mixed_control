
plot_treatment_comparison_panel <- function(y_var, plot_this, mainvecpop) {
  plot_this$y <- plot_this[, y_var]
  plot_this <- plot_this %>% filter(maintain_vector_pop == mainvecpop) 
  
  plot_this_high_treat_prop <- plot_this %>% filter(treat_prop > 0.5)
  
  facet_cols <- length(unique(plot_this$Insecticide))
  
  p <- plot_this %>%
    ggplot() +
    geom_point(aes(x = prevalence, y = y, 
                   colour = laXbel), size = 2) +
    geom_line(aes(x = prevalence, y = y, 
                  colour = laXbel)) +
    labs(colour = my_label("laXbel")) +
    facet_wrap(~facet_label, ncol = facet_cols) +
    ylab(my_label(y_var)) +
    xlab(my_label("prevalence")) +
    theme_grey(base_size = 16) +
    theme(legend.position = "bottom",
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    my_theme() +
    theme(panel.background = element_rect(fill = "grey95")) +
    scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8))
  
  
  if (y_var == "Rres_final") {
    p <- p + geom_hline(aes(yintercept = 1), linetype = "dashed", colour = "black") +
      coord_cartesian(ylim = c(0, 10))
    
  } else {
    p <- p #+ coord_cartesian(ylim = c(0, 10))
  }
  
  p2 <- p + geom_point(data = plot_this_high_treat_prop, 
                       aes(x = prevalence, y = y), 
                       colour = "white", size = 2, shape = 4)
  p2
}



