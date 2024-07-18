library(codetools)

output_baseline_params_as_dotplot <- function(){
  
  params_for_output <- set_baseline_parameters()
  plot_this <- convert_named_vector_to_long_df(params_for_output)
  
  ymax = 2200
  p <- plot_parameters(plot_this, ymax)
  p <- add_labels_to_baseline_parameters_dotplot(p, plot_this, ymax)
  p <- p + ggtitle("baseline parameters")
  print(p)
  
  ggsave(p, filename = paste0("output/baseline_params_plot.pdf"))
  write.csv(plot_this, paste0("output/baseline_params.csv"))
}


output_scenarios_as_dotplot <- function(scenario_df) {
  
  scenario_numeric <- scenario_df %>% select(where(is.numeric))
  scenario_numeric_long <- pivot_longer(scenario_numeric, everything(), names_to = "name", values_to = "value")
  plot_this <- distinct(scenario_numeric_long)
  
  df <- plot_this %>% filter(name %in%  c("K"))
  p1 <- plot_parameters(df, ymax = 10100)
  p1 <- add_labels_to_scenarios_dotplot(p1, df)
  
  df <- plot_this %>% filter(name %in%  c("birth_adj"))
  p2 <- plot_parameters(df, ymax = 2.1)
  p2 <- add_labels_to_scenarios_dotplot(p2, df)
  
  df <- plot_this %>% filter(name %in%  c("NC", "NW"))
  p3 <- plot_parameters(df, ymax = 300)
  p3 <- add_labels_to_scenarios_dotplot(p3, df)
  
  df <- plot_this %>% filter(name %in% c("max_time"))
  p4 <- plot_parameters(df, ymax = 10000)
  p4 <- add_labels_to_scenarios_dotplot(p4, df)
  
  df <- plot_this %>% filter(name %in% c("option"))
  p5 <- plot_parameters(df, ymax = 4.2)
  p5 <- add_labels_to_scenarios_dotplot(p5, df)
  
  df <- plot_this %>% filter(!(name %in% c("NC", "NW", "K", "birth_adj", "max_time", "option")))
  p6 <- plot_parameters(df, ymax = 1.0)
  
  p <- p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol = 1, heights = c(1, 1, 1, 1, 1, 3))
  print(p)
  ggsave(p, filename = paste0("output/scenario_plot.pdf"))
  write.csv(plot_this, paste0("output/scenario_params.csv"))
}

plot_parameters <- function(df, ymax) {
  my_size = 5
  my_colour = "hotpink"
  my_shape = 16
  
  p <- ggplot(df) +
    geom_point(aes(x = name, y = value), size = my_size, colour = my_colour, shape = my_shape) +
    ylim(0, ymax) +
    xlab("") + ylab("") +
    coord_flip()
  p
}

add_labels_to_baseline_parameters_dotplot <- function(p, df, ymax){
  my_vjust <- 0.25
  my_hjust <- 0.5
  p <- p + geom_text(aes(label= signif(value, 4), y = value + ymax/15, x = name), 
                     vjust = my_vjust, hjust = my_hjust)
  p
}

add_labels_to_scenarios_dotplot <- function(p, df){
  my_vjust <- 0.5
  my_hjust <- 0.5
  my_angle = 45
  p <- p + geom_text(aes(label= signif(value, 4), y = value, x = name), 
                     vjust = my_vjust, hjust = my_hjust, angle = my_angle)
  p
}

findGlobals(fun = plot_parameters, merge = FALSE)$variables
findGlobals(fun = add_labels_to_baseline_parameters_dotplot, merge = FALSE)$variables
findGlobals(fun = add_labels_to_scenarios_dotplot, merge = FALSE)$variables
findGlobals(fun = output_baseline_params_as_dotplot, merge = FALSE)$variables
findGlobals(fun = output_scenarios_as_dotplot, merge = FALSE)$variables
