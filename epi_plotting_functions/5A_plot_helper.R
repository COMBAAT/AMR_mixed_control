plot_y_versus_prop_cattle_with_insecticide_facet_NW <- function(df, y_var, this_NW_set, 
                                                                      this_vector_measure, this_vector_measure_value) {
  df$y <- df[, y_var]
  df$shape_variable <- df[, this_vector_measure]
  this_xlab <- my_label("prop_cattle_with_insecticide")
  this_ylab <- my_label(y_var)
  
  desired_vector <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9) # desired values, used to include 0.91
  actual_vector <- unique(df$treat_prop)
  nearest_vector <- find_nearest_vector(desired_vector, actual_vector)
  
  p <- df %>%
    mutate_at(c("treat_prop", "NW", this_vector_measure, "shape_variable"), as.factor) %>%
    filter(
      treat_prop %in% nearest_vector,
      NW %in% this_NW_set,
      get(this_vector_measure) == this_vector_measure_value
    ) %>%
    ggplot(aes(prop_cattle_with_insecticide, y, colour = treat_prop)) +
    geom_point(size = 1) +
    geom_line(linewidth = 1) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    ylim(0, ymax_function(y_var)) +
    xlim(0, 0.8) +
    labs(colour = my_label("treat_prop", split_across_lines = "other")) +
    facet_wrap(~NW, ncol = 3) +
    my_theme() + theme(legend.position = "bottom")
  if (y_var == "Rres_final"){
    p = p + geom_abline(aes(intercept = 1, slope = 0), linetype = "dashed")
  }
  p
}

plot_y_versus_prop_cattle_with_insecticide_facet_NW_ttype3 <- function(df, y_var, this_NW_set, 
                                                                             this_vector_measure, 
                                                                             this_vector_measure_value) {
  df$y <- df[, y_var]
  df$shape_variable <- df[, this_vector_measure]
  this_xlab <- my_label("prop_cattle_with_insecticide")
  this_ylab <- my_label(y_var)
  
  p <- df %>% mutate(proph_frequency = set_days_per_year() * proph_ongoing) %>%
    mutate_at(c("proph_frequency", "NW", this_vector_measure, "shape_variable"), as.factor) %>%
    filter(
      #prop_cattle_with_insecticide <= 0.5,
      NW %in% this_NW_set,
      get(this_vector_measure) == this_vector_measure_value,
      proph_frequency %in% c(0, 1, 2, 3, 6, 8)
    ) %>%
    ggplot(aes(prop_cattle_with_insecticide, y, colour = proph_frequency)) +
    geom_point(size = 1) +
    geom_line(linewidth = 1) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    ylim(0, ymax_function(y_var)) +
    labs(colour = "Doses per year") +
    facet_wrap(~NW, ncol = 3) +
    my_theme() + theme(legend.position = "bottom")
  if (y_var == "Rres_final"){
    p = p + geom_abline(aes(intercept = 1, slope = 0), linetype = "dashed")
  }
  p
}