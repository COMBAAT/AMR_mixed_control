
plot_type20 <- function(df, x_var, y_var, this_prop_insecticide) {
  df$x <- df[, x_var]
  df$y <- df[, y_var]
  
  df2 <- df 
  
  p <- df2 %>% filter(prop_cattle_with_insecticide %in% c(this_prop_insecticide)) %>%
    ggplot() +
    geom_point(aes(y = y, x = x,
                   shape = laXbel,
                   colour = laXbel), size = 1.5 * my_pointsize()) +
    geom_line(aes(y = y, x = x, 
                  colour = laXbel), 
              linewidth = my_linewidth()) +
    xlab(my_label(x_var)) +
    ylab(my_label(y_var)) +
    xlim(c(0, 1)) +
    labs(colour = my_label("laXbel", split_across_lines = "other"), 
         shape = my_label("laXbel", split_across_lines = "other")) +
    scale_shape_manual(values = c(4, 19, 1)) +
    scale_x_continuous(breaks = seq(0, xmax_function(x_var), by = xmax_function(x_var)/4)) +
    facet_wrap(~ NW) +
    my_theme()
  p
}


plot_type21 <- function(df, x_var, y_var) {
  df$x <- df[, x_var]
  df$y <- df[, y_var]
  
  my_labeller <- labeller(
    NW = function(x) paste0(my_label("NW"), ": ", x),
    prop_cattle_with_insecticide = function(x) paste0("Coverage:", x)
  )
  
  p <- df %>% filter(prop_cattle_with_insecticide %in% c(0, 0.05, 0.1, 0.2)) %>%
    ggplot() + 
    geom_point(aes(y = y, x = x, 
                   group = interaction(laXbel, Baseline_vector_host_ratio), 
                   shape = laXbel, 
                   colour = laXbel), size = 1.0 * my_pointsize()) +
    geom_line(aes(y = y, x = x, 
                  group = interaction(laXbel, Baseline_vector_host_ratio), 
                  colour = laXbel), linewidth = my_linewidth()) +
    xlab(my_label(x_var)) +
    ylab(my_label(y_var)) +
    xlim(c(0, 1)) +
    labs(colour = my_label("laXbel"), shape = my_label("laXbel")) +
    scale_shape_manual(values = c(4, 19, 1)) +
    facet_wrap(~ NW + prop_cattle_with_insecticide) +
    scale_x_continuous(breaks = seq(0, xmax_function(x_var), by = xmax_function(x_var)/4)) +
    scale_y_continuous(breaks = seq(0, ymax_function(y_var), by = ymax_function(y_var)/5)) +
    my_theme_invasion()
  p
  
  if (y_var == "Rres_final") {
    p <- p + geom_abline(intercept = 1, slope = 0, colour = "black", linetype = "dashed") 
  }
  p
}


plot_type22 <- function(df, y_var, this_NW_set, this_vector_measure, ttype) {
  
  x_var <- get_treat_var(df, ttype) 
  df$x <- df[, x_var]
  df$y <- df[, y_var]
  this_xlab <- my_label(x_var, split_across_lines = "default")
  this_ylab <- my_label(y_var, split_across_lines = "other")
  df$shape_variable <- df[, this_vector_measure]
  
  p <- df %>%
    ggplot(aes(x, y, colour = shape_variable)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(colour = my_label(this_vector_measure)) +
    my_theme() + 
    coord_cartesian(ylim = c(0, ymax_function(y_var))) +
    scale_x_continuous(breaks = seq(0, xmax_function(x_var), by = xmax_function(x_var)/4))
  p
}

plot_type23 <- function(df, y_var, this_vector_measure, 
                        this_vector_measure_value, this_NW_set, ttype) {
  
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
    scale_x_continuous(breaks = seq(0, xmax, length.out = 5)) #+
    
    # theme(
    #   panel.grid.major = element_line(
    #     colour = "grey90",
    #     linewidth = 0.35
    #   ),
    #   panel.grid.minor = element_blank(),
    #   panel.border = element_rect(
    #     colour = "grey25",
    #     fill = NA,
    #     linewidth = 0.6
    #   ),
    #   plot.background = element_rect(
    #     fill = "white",
    #     colour = NA
    #   )
    # )
  p
}


