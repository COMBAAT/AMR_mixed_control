
# =========================================================
# Function Names: my_linewidth, my_pointsize, my_label, my_pdfwidth, my_pdfheight, my_theme,
#                 plot_type1_y_versus_treat_prop_facet_NW,
#                 plot_type2_y_versus_treat_prop_facet_prop_cattle_with_insecticide,
#                 plot_type3_y_versus_treat_prop_facet_prop_cattle_with_insecticide_with_highlight,
#                 plot_type4_y_versus_treat_prop_facet_NW,
#                 plot_type5_y_versus_prop_cattle_with_insecticide_facet_NW,
#                 plot_type6_y_versus_treat_prop_facet_NW_K
# Description: This script provides utility functions for standardizing plot formatting in R visualizations.
#              It includes functions to adjust line widths, point sizes, generate appropriate labels for various variables,
#              and set other plot properties such as PDF dimensions, ensuring consistency across multiple plots.
#
# Parameters:
#   variable - A string indicating the variable name for which a label is needed.
#
# Returns:
#   Depending on the function, returns numeric values for sizes or strings for labels.
#
# Example of use:
#   ggplot(data, aes(x, y)) +
#       geom_line(size = my_linewidth()) +
#       geom_point(size = my_pointsize()) +
#       labs(title = my_label("R0sen"))
#
# Dependencies: Requires 'dplyr', 'ggplot2', and 'gghighlight' packages for data manipulation and enhanced visualization.
#
# Author: Shaun Keegan & Louise Matthews
# Date Created: August 2024
# Last Modified: August 2024
# =========================================================
library(dplyr)
library(ggplot2)
library(gghighlight)

# Specify plot formatting ------------------------------------------------------
my_linewidth <- function() {
  1
}

my_pointsize <- function() {
  2
}


my_label <- function(variable, split_across_lines = "default") {
  if (variable == "treat_prop") this_label <- "Case treatment proportion"
  if (variable == "coverage") this_label <- "Prophylactic coverage"
  if (variable == "R0sen") this_label <- "R0 sensitive"
  if (variable == "prevalence") this_label <- "Prevalence"
  if (variable == "Incidence") this_label <- "Incidence"
  if (variable == "No_trt_cat") this_label <- "Number treated"
  if (variable == "Prob_onward_tran") this_label <- "Prob onward transmission"
  if (variable == "RiskE") this_label <- "Risk of emergence and spread"
  if (variable == "RiskA") this_label <- "Risk of emergence"
  if (variable == "prop_cattle_with_insecticide") this_label <- "Insecticide \n coverage"
  if (variable == "prop_cattle_with_insecticide" & split_across_lines == "other") this_label <- "Insecticide coverage \n "
  if (variable == "NW") this_label <- "Wildlife"
  if (variable == "K") this_label <- "Carrying capacity"
  if (variable == "K_host_ratio") this_label <- "Vector host ratio"
  if (variable == "ratio") this_label <- "Selective advantage \n to resistant strain"
  if (variable == "Rres_final") this_label <- "Rres at equilibrium"
  if (variable == "Rsen_final") this_label <- "Rsen at equilibrium"
  if (variable == "R0sen_gt_1") this_label <- "R0sen with control"
  if (variable == "Baseline_vector_population") this_label <- "Vector number"
  if (variable == "Baseline_vector_host_ratio") this_label <- "Vector host ratio"
  this_label
}

my_pdfwidth <- function() {
  8 #7
}

my_pdfheight <- function() {
  8*2/3 #7*2/3
}

#my_pointsize <- function() {
#  3
#}

my_theme <- function() {
  theme_grey(base_size = 16) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 19),
      panel.grid.major = element_blank()
      #panel.grid.minor = element_blank()
    )
}

# Specify plot functions -------------------------------------------------------


#-------------------------------------------------------------------------------
# Function Name: plot_type1_y_versus_treat_prop_facet_NW
#
# Parameters:
#   df - Dataframe containing data.
#   y_var - String, the y-variable to be plotted.
#
# Outputs:
#   Returns a ggplot object.
#
# Dependencies:
#   dplyr, ggplot2, gghighlight
#
#-------------------------------------------------------------------------------

get_x_var <- function(df, ttype) {
  if (ttype == 3) {
    x_var = "coverage"
  } else {
    x_var = "treat_prop"
  }
  x_var
}

plot_type1_y_versus_treat_prop_facet_NW <- function(df, y_var, this_NW_set, this_vector_measure, ttype) {
  
  x_var <- get_x_var(df, ttype) 
  df$x <- df[, x_var]
  df$y <- df[, y_var]
  this_xlab <- my_label(x_var)
  this_ylab <- my_label(y_var)
  df$shape_variable <- df[, this_vector_measure]

  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "shape_variable"), as.factor) %>%
    filter(
      prop_cattle_with_insecticide == 0,
      NW %in% this_NW_set
    ) %>%
    ggplot(aes(x, y, colour = shape_variable)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(colour = my_label(this_vector_measure)) +
    facet_wrap(~NW) +
    my_theme()
  p
}

#-------------------------------------------------------------------------------
# Function Name: plot_type2_y_versus_treat_prop_facet_prop_cattle_with_insecticide
#
# Parameters:
#   df - Dataframe containing data.
#   this_vector_measure_value - Specific value of vector measure to filter by.
#   y_var - String, the y-variable to be plotted.
#
# Outputs:
#   Returns a ggplot object with highlighted areas based on condition.
#
# Dependencies:
#   dplyr, ggplot2, gghighlight
#
#-------------------------------------------------------------------------------

plot_type2_y_versus_treat_prop_facet_prop_cattle_with_insecticide <- function(df, 
          y_var, this_NW_set, this_vector_measure, this_vector_measure_value, ttype) {
  
  x_var <- get_x_var(df, ttype) 
  df$x <- df[, x_var]
  df$y <- df[, y_var]
  this_xlab <- my_label(x_var)
  this_ylab <- my_label(y_var)

  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", this_vector_measure), as.factor) %>%
    filter(
      #prop_cattle_with_insecticide %in% c(0, 0.05, 0.1, 0.15, 0.2),
      #NW %in% this_NW_set,
      get(this_vector_measure) == this_vector_measure_value
    ) %>%
    ggplot(aes(x, y, shape = NW, colour = prop_cattle_with_insecticide)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    facet_wrap(~prop_cattle_with_insecticide) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(shape = my_label("NW"), colour = my_label("prop_cattle_with_insecticide")) +
    my_theme()
  p
}

#-------------------------------------------------------------------------------
# Function Name: plot_type3_y_versus_treat_prop_facet_prop_cattle_with_insecticide_with_highlight
#
# Parameters:
#   df - Dataframe containing data.
#   this_vector_measure_value - Specific value of vector measure to filter by.
#   y_var - String, the y-variable to be plotted.
#   threshold_var - String, the variable used for threshold condition.
#   threshold - Numeric, the value of the threshold for highlighting.
#
# Outputs:
#   Returns a ggplot object with areas highlighted based on the threshold condition.
#
# Dependencies:
#   dplyr, ggplot2, gghighlight
#
#-------------------------------------------------------------------------------

plot_type3_y_versus_treat_prop_facet_prop_cattle_with_insecticide_with_higlight <- function(
    df, y_var, threshold_var, threshold, this_NW_set, this_vector_measure, this_vector_measure_value) {
  df$y <- df[, y_var]
  df$threshold_var <- df[, threshold_var]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)

  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", this_vector_measure), as.factor) %>%
    filter(
      #prop_cattle_with_insecticide %in% c(0, 0.05, 0.1, 0.15, 0.2),
      NW %in% this_NW_set,
      get(this_vector_measure) == this_vector_measure_value
    ) %>%
    ggplot(aes(treat_prop, y,
      group = interaction(NW, prop_cattle_with_insecticide),
      shape = NW, colour = prop_cattle_with_insecticide
    )) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    gghighlight(
      threshold_var < threshold,
      unhighlighted_params = list(colour = "darkgrey"), calculate_per_facet = TRUE
    ) +
    facet_wrap(~prop_cattle_with_insecticide) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(shape = my_label("NW"), colour = my_label("prop_cattle_with_insecticide")) +
    my_theme()
  p
}

#-------------------------------------------------------------------------------
# Function Name: plot_type4_y_versus_treat_prop_facet_NW
#
# Parameters:
#   df - Dataframe containing data.
#   y_var - String, the y-variable to be plotted.
#   this_vector_measure_value - Specific value of vector measure to filter by.
#
# Outputs:
#   Returns a ggplot object.
#
# Dependencies:
#   dplyr, ggplot2
#
#-------------------------------------------------------------------------------
ymax_function <- function(y_var) {
  if (y_var == "R0sen") {
    ymax <- 20.0
  } else if (y_var == "RiskE") {
    ymax <- 8.0
  } else if (y_var == "RiskA") {
    ymax <- 8.0
  } else if (y_var == "Incidence") {
    ymax <- 800
  } else if (y_var == "prevalence") {
    ymax <- 0.8
  } else if (y_var == "No_trt_cat") {
    ymax <- 800
  } else {
    ymax <- 1.0
  }
  return(ymax)
}

plot_type4_y_versus_treat_prop_facet_NW <- function(df, y_var, this_NW_set, 
                                                    this_vector_measure, this_vector_measure_value) {
  df$y <- df[, y_var]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)
  df$shape_variable <- df[, this_vector_measure]

  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", this_vector_measure), as.factor) %>%
    filter(
      get(this_vector_measure) == this_vector_measure_value,
      NW %in% this_NW_set
    ) %>%
    ggplot(aes(treat_prop, y, shape = get(this_vector_measure), colour = prop_cattle_with_insecticide)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    facet_wrap(~NW) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(shape = my_label(this_vector_measure), colour = my_label("prop_cattle_with_insecticide")) +
    my_theme() + 
    coord_cartesian(ylim = c(0, ymax_function(y_var))) 
  p
}

#-------------------------------------------------------------------------------
# Function Name: plot_type5_y_versus_prop_cattle_with_insecticide_facet_NW
#
# Parameters:
#   df - Dataframe containing data.
#   y_var - String, the y-variable to be plotted.
#   this_K - Specific value of K to filter by.
#
# Outputs:
#   Returns a ggplot object.
#
# Dependencies:
#   dplyr, ggplot2
#
#-------------------------------------------------------------------------------

plot_type5_y_versus_prop_cattle_with_insecticide_facet_NW <- function(df, y_var, this_NW_set, 
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
      prop_cattle_with_insecticide <= 0.5,
      #treat_prop %in% c(0, 0.2, 0.4, 0.6, 0.8, 0.9),
      treat_prop %in% nearest_vector,
      NW %in% this_NW_set,
      get(this_vector_measure) == this_vector_measure_value
    ) %>%
    ggplot(aes(prop_cattle_with_insecticide, y, shape = shape_variable, colour = treat_prop)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(colour = my_label("treat_prop"), shape = my_label(this_vector_measure)) +
    facet_wrap(~NW) +
    my_theme()
  p
}

#-------------------------------------------------------------------------------
# Function Name: plot_type10_R0sen_versus_Rsen
#
# Parameters:
#   df - Dataframe containing data.
#
# Outputs:
#   Returns a ggplot object.
#
# Dependencies:
#   dplyr, ggplot2
#
#-------------------------------------------------------------------------------

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


plot_invasion_landscape <- function(prev_threshold, subset_for_plotting) {
  prev_threshold_label <- paste0("prev > ", prev_threshold)
  colours <- c("turquoise", "olivedrab3", "tomato", "mediumorchid1", "lightgrey")
  names(colours) <- c("Sen outcompetes Res", "No Sen & Res can't invade", "Res outcompetes Sen", "No Sen & Res can invade", prev_threshold_label)

  # subset_for_plotting %>%
  #   filter(near(treat_prop, 0.95) | near(treat_prop, 0.99) | treat_prop <= 0.9) %>%
  #   mutate(Region = case_when(prevalence > prev_threshold ~ prev_threshold_label, TRUE ~ Region)) %>%
  #   mutate(facet_variable = get(this_vector_measure)) %>% glimpse()

  plot <- subset_for_plotting %>%
    filter(near(treat_prop, 0.95) | near(treat_prop, 0.99) | treat_prop <= 0.9) %>%
    mutate(Region = case_when(prevalence > prev_threshold ~ prev_threshold_label, TRUE ~ Region)) %>%
    mutate(cc_or_vh_ratio = get(this_vector_measure)) %>%
    ggplot() +
    geom_point(aes(x = prop_cattle_with_insecticide, y = treat_prop, colour = Region, shape = R0sen_gt_1), size = 2, show.legend = TRUE) +
    #geom_point(aes(x = prop_cattle_with_insecticide, y = treat_prop, colour = ratio > 1), size = 0.1, show.legend = TRUE) +
    #geom_point(aes(x = prop_cattle_with_insecticide, y = treat_prop, colour = Region), show.legend = TRUE) +
    #geom_point(aes(x = closest_to_1_value, y = treat_prop), shape = 1, size = 2, colour = "black") +
    # geom_point(aes(x = closest_to_1_value, y = treat_prop), shape = 1, size = 2, colour = "white") +
    scale_color_manual(values = colours) +
    xlab(my_label("prop_cattle_with_insecticide", "other")) +
    ylab(my_label("treat_prop")) +
    facet_wrap(~ NW + cc_or_vh_ratio, labeller = label_both) +
    ggtitle(paste(
      "Treatment type = ", unique(subset_for_plotting$treatment_type), "; ",
      "Use carrying capacity = ", unique(subset_for_plotting$use_carrying_capacity), "; ",
      "Maintain vector pop = ", unique(subset_for_plotting$maintain_vector_pop)
    )) +
    ylim(c(0,1)) +
    scale_shape_manual(values = c(4, 16)) +
    labs(shape = my_label("R0sen_gt_1")) +
    theme_bw()

  output_label <- "plot_type15_invasion_panel"
  output_filename <- paste0(folder_name, output_label, "_ttype", ttype, "_spec_", spec, "_prev_threshold_", prev_threshold, ".pdf")
  ggsave(
    filename = output_filename,
    width = 1.2 * my_pdfwidth(), height = 1.2 * my_pdfheight()
  )
  plot
}


plot_other_landscape <- function(subset_for_plotting, colour_var) {
  subset_for_plotting <- subset_for_plotting %>% mutate(colour_var = .data[[colour_var]])
  
  #subset_for_plotting <- subset_for_plotting %>% group_by(NW, get(this_vector_measure)) %>%
  #  summarise(max = max(prevalence)) %>%
  #              ungroup()
  
  plot <- subset_for_plotting %>%
    filter(near(treat_prop, 0.95) | near(treat_prop, 0.99) | treat_prop <= 0.9) %>%
    mutate(cc_or_vh_ratio = get(this_vector_measure)) %>%
    ggplot() +
    geom_point(aes(x = prop_cattle_with_insecticide, y = treat_prop, 
                   colour = colour_var, shape = R0sen_gt_1), size = 2, show.legend = TRUE) +
    #scale_colour_gradientn(colours = terrain.colors(15)) 
    #geom_point(aes(x = prop_cattle_with_insecticide, y = treat_prop, colour = RiskA), show.legend = TRUE) +
    #geom_point(aes(x = closest_to_1_value, y = treat_prop), shape = 1, size = 2, colour = "black", show.legend = TRUE) +
    xlab(my_label("prop_cattle_with_insecticide", "other")) +
    ylab(my_label("treat_prop")) +
    facet_wrap(~ NW + cc_or_vh_ratio, labeller = label_both) +
    ggtitle(paste(
      "Treatment type = ", unique(subset_for_plotting$treatment_type), "; ",
      "Use carrying capacity = ", unique(subset_for_plotting$use_carrying_capacity), "; ",
      "Maintain vector pop = ", unique(subset_for_plotting$maintain_vector_pop)
    )) + 
    scale_colour_gradientn(colours = terrain.colors(15)) +
    ylim(c(0,1)) +
    #scale_colour_gradientn(colours = c("blue", "red")) +
    scale_shape_manual(values = c(4, 16)) +
    labs(colour = my_label(colour_var), shape = my_label("R0sen_gt_1")) +
    theme_bw()
  
  output_label <- "plot_type16_other_panel"
  output_filename <- paste0(folder_name, output_label,"_var_", colour_var, "_ttype", ttype, "_spec_", spec, ".pdf")
  ggsave(
    filename = output_filename,
    width = 1.2 * my_pdfwidth(), height = 1.2 * my_pdfheight()
  )
  plot
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

# End of script ---------------------------------------------------------------
