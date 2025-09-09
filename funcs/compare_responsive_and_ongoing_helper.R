
plot_type20 <- function(df, x_var, y_var) {
  df$x <- df[, x_var]
  df$y <- df[, y_var]
  
  df2 <- df %>% mutate(treatment_type = 
                         case_when(treatment_type == "proph_ongoing" ~ "Ongoing longlasting", 
                                   treatment_type == "curative" ~ "Responsive curative",
                                   treatment_type == "longlasting" ~ "Responsive longlasting",
                                   T ~ treatment_type))
  p <- df2 %>% filter(prop_cattle_with_insecticide %in% c(0)) %>%
    ggplot() +
    geom_point(aes(y = y, x = x,
                   #group = interaction(treatment_type, Baseline_vector_host_ratio),
                   shape = treatment_type,
                   colour = treatment_type), size = 1.5 * my_pointsize()) +
    geom_line(aes(y = y, x = x, 
                  #group = interaction(treatment_type, Baseline_vector_host_ratio), 
                  colour = treatment_type), 
              linewidth = my_linewidth()) +
    xlab(my_label(x_var)) +
    ylab(my_label(y_var)) +
    xlim(c(0, 1)) +
    labs(colour = my_label("treatment_type", split_across_lines = "other"), 
         shape = my_label("treatment_type", split_across_lines = "other")) +
    scale_shape_manual(values = c(4, 19, 1)) +
    facet_wrap(~ NW) +
    my_theme()
  p
}


plot_type21 <- function(df, x_var, y_var) {
  df$x <- df[, x_var]
  df$y <- df[, y_var]
  
  p <- df %>% filter(prop_cattle_with_insecticide %in% c(0, 0.1, 0.2)) %>%
    ggplot() + 
    geom_point(aes(y = y, x = x, 
                   group = interaction(treatment_type, Baseline_vector_host_ratio), 
                   shape = treatment_type, 
                   colour = treatment_type), size = 1.5 * my_pointsize()) +
    geom_line(aes(y = y, x = x, 
                  group = interaction(treatment_type, Baseline_vector_host_ratio), 
                  colour = treatment_type), linewidth = my_linewidth()) +
    xlab(my_label(x_var)) +
    ylab(my_label(y_var)) +
    xlim(c(0, 1)) +
    labs(colour = my_label("treatment_type"), shape = my_label("treatment_type")) +
    scale_shape_manual(values = c(4, 19, 1)) +
    facet_wrap(~ NW + prop_cattle_with_insecticide) +
    my_theme()
  p
}


plot_type22_y_versus_treat_prop_facet_treatment_type <- function(df, y_var, this_NW_set, this_vector_measure, ttype) {
  
  x_var <- get_treat_var(df, ttype) 
  df$x <- df[, x_var]
  df$y <- df[, y_var]
  this_xlab <- my_label(x_var, split_across_lines = "default")
  this_ylab <- my_label(y_var, split_across_lines = "other")
  df$shape_variable <- df[, this_vector_measure]
  #y_max <- my_lim(y_var)
  
  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "shape_variable"), as.factor) %>%
    filter(
      prop_cattle_with_insecticide == 0,
      NW %in% this_NW_set
    ) %>%
    ggplot(aes(x, y, colour = shape_variable)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    #ylim(c(0, y_max)) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(colour = my_label(this_vector_measure)) +
    my_theme() + 
    coord_cartesian(ylim = c(0, ymax_function(y_var)), xlim = c(0, xmax_function(x_var))) 
  p
}

plot_type23_y_versus_treat_prop_facet_treatment_type <- function(df, y_var, this_vector_measure, 
                                                                 this_vector_measure_value,
                                                                 this_NW_set, ttype) {
  
  x_var <- get_treat_var(df, ttype) 
  ymax <- ymax_function(y_var)
  xmax <- xmax_function(x_var)
  df$x <- df[, x_var]
  df$y <- df[, y_var]
  this_xlab <- my_label(x_var, split_across_lines = "default")
  this_ylab <- my_label(y_var, split_across_lines = "other")
  
  
  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW"), as.factor) %>%
    filter(
      prop_cattle_with_insecticide == 0,
      get(this_vector_measure) == this_vector_measure_value
    ) %>%
    ggplot(aes(x, y, colour = NW)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    #ylim(c(0, y_max)) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(colour = my_label("NW")) +
    my_theme() + 
    coord_cartesian(ylim = c(0, ymax), xlim = c(0, xmax))  +
    scale_x_continuous(breaks = seq(0, xmax, length.out = 5))
  p
}


