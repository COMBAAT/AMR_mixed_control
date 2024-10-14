library(codetools)

plot_type7_y_versus_prevalence_facet_NW <- function(df, y_var, this_K) {
  df$y <- df[, y_var]
  this_xlab <- my_label("prevalence")
  this_ylab <- my_label(y_var)
  
  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "K"), as.factor) %>%
    filter(
      K == this_K,
      NW %in% c(0, 100, 250)
    ) %>%
    ggplot(aes(prevalence, y, shape = K, colour = prop_cattle_with_insecticide)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    facet_wrap(~NW) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(shape = my_label("K"), colour = my_label("prop_cattle_with_insecticide")) +
    my_theme()
  p
}

plot_type8_y_versus_treat_prop_facet_insecticide <- function(df, y_var, this_K) {
  df$y <- df[, y_var]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)

  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "K"), as.factor) %>%
    filter(
      K == this_K,
      NW %in% c(0, 100, 250)
    ) %>%
    ggplot(aes(treat_prop, y, shape = NW, colour = prop_cattle_with_insecticide)) +
    #geom_point(size = my_pointsize()) +
    geom_point(aes(size = K)) +
    geom_line(linewidth = my_linewidth()) +
    facet_wrap(~prop_cattle_with_insecticide) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(shape = my_label("NW"), colour = my_label("prop_cattle_with_insecticide")) +
    my_theme()
  p
}

plot_type9_y_versus_treat_prop_facet_treatment_protocol <- function(df, y_var, this_K) {
  df$y <- df[, y_var]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)
  
  df <- df %>% mutate(x_var = case_when(treatment_type == "ongoing" ~ PF_final/All_cows_final,
                                        treatment_type != "ongoing" ~ treat_prop))
  
  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "K"), as.factor) %>%
    filter(
      K == this_K,
      NW %in% c(0)
    ) %>% 
      ggplot(aes(x_var, y, colour = treatment_type)) + #, shape = prop_cattle_with_insecticide)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    facet_wrap(~prop_cattle_with_insecticide) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(shape = my_label("NW"), colour = my_label("prop_cattle_with_insecticide")) +
    my_theme()
  p
}


find_nearest_vector <- function(desired_vector, actual_vector) {
  selected_vector <- c()
  for (element in desired_vector) {
    diff <- abs(actual_vector - element)
    selected_element <- actual_vector[which.min(diff)]
    selected_vector <- c(selected_vector, selected_element)
  }
  selected_vector
}

examine_df <- function(df) {
  print(unique(df$treatment_type))
  print(paste0("length treat_prop = ", length(df$treat_prop)))
  print(paste0("length proph_ongoing = ", length(df$proph_ongoing)))
  print(unique(df$Vector_total_final))
}

plot_type10_R0sen_versus_Rsen <- function(df) {
  
  df <- df %>%
    mutate(reaches_equilibrium = case_when(time_final < 10000 ~ TRUE, time_final == 10000 ~ FALSE)) %>%
    filter(R0sen < 500)
  
  p <- df %>%
    ggplot() +
    geom_point(aes(
      y = Rsen_final, x = R0sen, colour = as.factor(reaches_equilibrium),
      shape = as.factor(treatment_type)
    )) +
    geom_abline(aes(slope = 1, intercept = 0), colour = "black") +
    labs(shape = "treatment_type", colour = "reaches equil") +
    my_theme()
  p
}

# Plot R resistant/R sensitive versus wildlife
plot_type11_selective_advantage_by_NW <- function(df, this_K, lw = my_linewidth(), ps = my_pointsize()) {
  p <- subset_for_plotting %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "K"), as.factor) %>%
    filter(prop_cattle_with_insecticide == 0, K == this_K) %>%
    #ggplot(aes(treat_prop, ratio, colour = NW, shape = K)) +
    ggplot(aes(treat_prop, ratio, colour = NW)) +
    geom_segment(x = 0.0, y = 1.0, xend = 1.0, yend = 1.0, colour = "red", linewidth = 0.5) +
    geom_point(size = ps) +
    geom_line(linewidth = lw) +
    xlim(c(0,1)) +
    xlab(my_label("treat_prop")) +
    ylab("Selective advantage to \n resistant strain") +
    #labs(colour = my_label("NW"), shape = my_label("K")) +
    labs(colour = my_label("NW")) +
    my_theme()
  
  p
}

plot_type11b_selective_advantage_by_NW_and_insectide <- function(df, this_K, ymax, this_NW) {
  insecticide_vector <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5) #unique(df$prop_cattle_with_insecticide)
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label("ratio")
  
  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "prop_cattle_with_insecticide"), as.factor) %>%
    #filter(K == this_K, prop_cattle_with_insecticide %in% insecticide_vector, NW %in% c(0, 10, 50, 100, 250)) %>%
    filter(K == this_K, prop_cattle_with_insecticide %in% insecticide_vector, NW == this_NW) %>%
    #ggplot(aes(treat_prop, ratio, shape = NW, colour = prop_cattle_with_insecticide)) +
    ggplot(aes(treat_prop, ratio, colour = prop_cattle_with_insecticide)) +
    geom_segment(x = 0.0, y = 1.0, xend = 1.0, yend = 1.0, colour = "red", linewidth = 0.5) +
    #facet_wrap(~NW, nrow = 1) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    coord_cartesian(ylim = c(0, ymax)) +
    
    #labs(shape = my_label("NW"), colour = my_label("prop_cattle_with_insecticide")) +
    labs(colour = my_label("prop_cattle_with_insecticide")) +
    my_theme()
  p
}


plot_type12_yvar_by_NW_and_insectide <- function(df, y_var, this_K, ymax, this_NW) {
  insecticide_vector <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5) #unique(df$prop_cattle_with_insecticide)
  df$y <- df[, y_var]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)

  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "prop_cattle_with_insecticide"), as.factor) %>%
    #filter(K == this_K, prop_cattle_with_insecticide %in% insecticide_vector, NW %in% c(0, 10, 50, 100, 250)) %>%
    filter(K == this_K, prop_cattle_with_insecticide %in% insecticide_vector, NW == this_NW) %>%
    #ggplot(aes(treat_prop, y, shape = NW, colour = prop_cattle_with_insecticide)) +
    ggplot(aes(treat_prop, y, colour = prop_cattle_with_insecticide)) +
    #facet_wrap(~NW, nrow = 1) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    coord_cartesian(ylim = c(0, ymax)) +
    #labs(shape = my_label("NW"), colour = my_label("prop_cattle_with_insecticide")) +
    labs(colour = my_label("prop_cattle_with_insecticide")) +
    my_theme()
  p
}


findGlobals(fun = find_nearest_vector, merge = FALSE)$variables