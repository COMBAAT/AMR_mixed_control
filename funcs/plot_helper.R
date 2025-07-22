
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
  if (variable == "treat_prop" & split_across_lines == "other") this_label <- "Case treatment \n proportion"
  if (variable == "coverage") this_label <- "Prophylactic coverage"
  if (variable == "coverage" & split_across_lines == "other") this_label <- "Prophylactic \n coverage"
  if (variable == "treatment_type") this_label <- "Treatment type"
  if (variable == "treatments_per_year") this_label <- "Annual treatments per animal"
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


ymax_function <- function(y_var) {
  if (y_var == "R0sen") {
    ymax <- 25.0
  } else if (y_var == "RiskE") {
    ymax <- 12.5
  } else if (y_var == "RiskA") {
    ymax <- 12.5
  } else if (y_var == "Incidence") {
    ymax <- 1250
  } else if (y_var == "prevalence") {
    ymax <- 1.0
  } else if (y_var == "No_trt_cat") {
    ymax <- 1250
  } else {
    ymax <- 1.0
  }
  return(ymax)
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
plot_type0_ratio <- function(df, this_vector_measure, ttype) {
  x_var <- get_treat_var(df, ttype)
  df$x <- df[, x_var]
  this_xlab <- my_label(x_var)
  
  df$shape_variable <- df[, this_vector_measure]
  lhs <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", this_vector_measure, "shape_variable"), as.factor) %>%
    filter(prop_cattle_with_insecticide == 0.0) %>%
    ggplot(aes(x, ratio, colour = NW, shape = shape_variable)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    xlab(this_xlab) +
    ylab(my_label("ratio")) +
    labs(colour = my_label("NW"), shape = my_label(this_vector_measure)) +
    my_theme()
  
  rhs <- lhs + ylim(c(0, 2)) +
    geom_abline(intercept = 1.0, slope = 0, linetype = "dashed")
  rhs
  
  # use patchwork package to stick plots together
  # use guides = collect to remove duplicate legends
  p <- lhs + rhs + plot_layout(ncol = 2, guides = "collect", axis_titles = "collect")
  p
}

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

get_treat_var <- function(df, ttype) {
  if (ttype == 3) {
    x_var = "coverage"
  } else {
    x_var = "treat_prop"
  }
  x_var
}

plot_type1_y_versus_treat_prop_facet_NW <- function(df, y_var, this_NW_set, this_vector_measure, ttype) {
  
  x_var <- get_treat_var(df, ttype) 
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
    #ylim(c(0, y_max)) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(colour = my_label(this_vector_measure)) +
    facet_wrap(~NW) +
    my_theme() + 
    coord_cartesian(ylim = c(0, ymax_function(y_var))) 
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
  
  x_var <- get_treat_var(df, ttype) 
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
    df, y_var, threshold_var, threshold, this_NW_set, this_vector_measure, this_vector_measure_value, ttype) {
  df$threshold_var <- df[, threshold_var]
  x_var <- get_treat_var(df, ttype) 
  df$x <- df[, x_var]
  df$y <- df[, y_var]
  this_xlab <- my_label(x_var)
  this_ylab <- my_label(y_var)

  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", this_vector_measure), as.factor) %>%
    filter(
      #prop_cattle_with_insecticide %in% c(0, 0.05, 0.1, 0.15, 0.2),
      NW %in% this_NW_set,
      get(this_vector_measure) == this_vector_measure_value
    ) %>%
    ggplot(aes(x, y,
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

plot_type4_y_versus_treat_prop_facet_NW <- function(df, y_var, this_NW_set, 
                                                    this_vector_measure, this_vector_measure_value, ttype) {
  x_var <- get_treat_var(df, ttype) 
  df$x <- df[, x_var]
  df$y <- df[, y_var]
  this_xlab <- my_label(x_var)
  this_ylab <- my_label(y_var)
  df$shape_variable <- df[, this_vector_measure]

  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", this_vector_measure), as.factor) %>%
    filter(
      get(this_vector_measure) == this_vector_measure_value,
      NW %in% this_NW_set
    ) %>%
    ggplot(aes(x, y, shape = get(this_vector_measure), colour = prop_cattle_with_insecticide)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    #ylim(c(0, y_max)) +
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

plot_type5_y_versus_prop_cattle_with_insecticide_facet_NW_ttype3 <- function(df, y_var, this_NW_set, 
                                                                      this_vector_measure, this_vector_measure_value) {
  df$y <- df[, y_var]
  df$shape_variable <- df[, this_vector_measure]
  this_xlab <- my_label("prop_cattle_with_insecticide")
  this_ylab <- my_label(y_var)
  
  p <- df %>% mutate(proph_frequency = set_days_per_year() * proph_ongoing) %>%
    mutate_at(c("proph_frequency", "NW", this_vector_measure, "shape_variable"), as.factor) %>%
    filter(
      prop_cattle_with_insecticide <= 0.5,
      NW %in% this_NW_set,
      get(this_vector_measure) == this_vector_measure_value
    ) %>%
    ggplot(aes(prop_cattle_with_insecticide, y, shape = shape_variable, colour = proph_frequency)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(colour = "Doses per year", shape = my_label(this_vector_measure)) +
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


plot_invasion_landscape <- function(prev_threshold, df, ttype) {
  
  y_var <- get_treat_var(df, ttype)
  df <- as.data.frame(df)
  df$y <- df[, y_var]
  
  prev_threshold_label <- paste0("prev > ", prev_threshold)
  colours <- c("turquoise", "olivedrab3", "tomato", "mediumorchid1", "lightgrey")
  names(colours) <- c("Sen outcompetes Res", "No Sen & Res can't invade", "Res outcompetes Sen", "No Sen & Res can invade", prev_threshold_label)

  plot <- df %>%
    filter(near(treat_prop, 0.95) | near(treat_prop, 0.99) | treat_prop <= 0.9) %>%
    mutate(Region = case_when(prevalence > prev_threshold ~ prev_threshold_label, TRUE ~ Region)) %>%
    mutate(cc_or_vh_ratio = get(this_vector_measure)) %>%
    ggplot() +
    geom_point(aes(x = prop_cattle_with_insecticide, y = y, colour = Region, shape = R0sen_gt_1), size = 2, show.legend = TRUE) +
    scale_color_manual(values = colours) +
    xlab(my_label("prop_cattle_with_insecticide", "other")) +
    ylab(my_label(y_var)) +
    facet_wrap(~ NW + cc_or_vh_ratio, labeller = label_both) +
    ggtitle(paste(
      "Treatment type = ", unique(df$treatment_type), "; ",
      "Use carrying capacity = ", unique(df$use_carrying_capacity), "; ",
      "Maintain vector pop = ", unique(df$maintain_vector_pop)
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


plot_other_landscape <- function(df, colour_var, ttype) {
  df <- df %>% mutate(colour_var = .data[[colour_var]])
  #df$colour_var <- df[, colour_var]
  y_var <- get_treat_var(df, ttype)
  df <- as.data.frame(df)
  df$y <- df[, y_var]
  
  plot <- df %>%
    filter(near(treat_prop, 0.95) | near(treat_prop, 0.99) | treat_prop <= 0.9) %>%
    mutate(cc_or_vh_ratio = get(this_vector_measure)) %>%
    ggplot() +
    geom_point(aes(x = prop_cattle_with_insecticide, y = y, 
                   colour = colour_var, shape = R0sen_gt_1), size = 2, show.legend = TRUE) +
    xlab(my_label("prop_cattle_with_insecticide", "other")) +
    ylab(my_label(y_var)) +
    facet_wrap(~ NW + cc_or_vh_ratio, labeller = label_both) +
    ggtitle(paste(
      "Treatment type = ", unique(df$treatment_type), "; ",
      "Use carrying capacity = ", unique(df$use_carrying_capacity), "; ",
      "Maintain vector pop = ", unique(df$maintain_vector_pop)
    )) + 
    scale_colour_gradientn(colours = terrain.colors(15)) +
    ylim(c(0,1)) +
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





# End of script ---------------------------------------------------------------
