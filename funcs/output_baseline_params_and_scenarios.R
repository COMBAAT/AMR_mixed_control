library(codetools)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)

plot_baseline_parameters <- function(params){
  
  plot_this <- convert_named_vector_to_long_df(params)
  ymax = max(plot_this$value) * 1.2
  p <- plot_parameters(plot_this, ymax)
  p <- add_labels_to_baseline_parameters_dotplot(p, plot_this, ymax)
  p <- p + ggtitle("baseline parameters")
  p
}

get_simplified_scenarios <- function(scenarios_df) {
  scenarios_numeric <- scenarios_df %>% select(where(is.numeric))
  scenarios_numeric_long <- pivot_longer(scenarios_numeric, everything(), names_to = "name", values_to = "value")
  simplified_scenarios <- distinct(scenarios_numeric_long)
  simplified_scenarios
}


plot_scenarios <- function(scenarios_df) {
  
  simplified_scenarios <- get_simplified_scenarios(scenarios_df)
  
  plot_this <- simplified_scenarios %>% filter(name %in%  c("K"))
  p1 <- plot_parameters(plot_this, ymax = 10100)
  p1 <- add_labels_to_scenarios_dotplot(p1, plot_this)
  
  plot_this <- simplified_scenarios %>% filter(name %in%  c("birth_adj"))
  p2 <- plot_parameters(plot_this, ymax = 2.1)
  p2 <- add_labels_to_scenarios_dotplot(p2, plot_this)
  
  plot_this <- simplified_scenarios %>% filter(name %in%  c("NC", "NW"))
  p3 <- plot_parameters(plot_this, ymax = 300)
  p3 <- add_labels_to_scenarios_dotplot(p3, plot_this)
  
  plot_this <- simplified_scenarios %>% filter(name %in% c("max_time"))
  p4 <- plot_parameters(plot_this, ymax = 10000)
  p4 <- add_labels_to_scenarios_dotplot(p4, plot_this)
  
  plot_this <- simplified_scenarios %>% filter(!(name %in% c("NC", "NW", "K", "birth_adj", "max_time")))
  p6 <- plot_parameters(plot_this, ymax = 1.0)
  
  p <- p1 + p2 + p3 + p4 + p6 + plot_layout(ncol = 1, heights = c(1, 1, 1, 1, 3))
  p
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
findGlobals(fun = plot_baseline_parameters, merge = FALSE)$variables
findGlobals(fun = plot_scenarios, merge = FALSE)$variables

