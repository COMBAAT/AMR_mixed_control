
get_subset_for_plotting <- function(scenarios_df, test, option, scenario = 1, fit_adj_new = 0.6) {
  # select quick treatment (1), responsive treatment with prophylactic drug (2), ongoing prophylactic treatment (3)
  subset <- create_data_subsets(test, option)
  
  # subset further by scenario if addiotnal parameters varied, default is first row
  selected_row <- 1
  subset_for_plotting <- select_scenario(scenarios_df, subset, selected_row)
  
  # Adjust fitness post simulation
  subset_for_plotting <- adjust_fitness(subset_for_plotting, fit_adj_new = fit_adj_new)
  subset_for_plotting
}



# functions for plotting
create_p1_plots <- function(subset_for_plotting, this_K, label, plot_title) {
  p1a <- plot_type11_selective_advantage_by_NW(subset_for_plotting, this_K, lw = 0.7, ps = 2)
  p1b <- plot_type11_selective_advantage_by_NW(subset_for_plotting, this_K)
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

