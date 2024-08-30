
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
  3
}


my_label <- function(variable) {
  if (variable == "R0sen") this_label <- "R0 sensitive"
  if (variable == "prevalence") this_label <- "Prevalence"
  if (variable == "Incidence") this_label <- "Incidence"
  if (variable == "No_trt_cat") this_label <- "Number treated cattle"
  if (variable == "Prob_onward_tran") this_label <- "Prob onward transmission"
  if (variable == "RiskE") this_label <- "Risk of emergence and spread"
  if (variable == "RiskA") this_label <- "Risk of emergence"
  if (variable == "treat_prop") this_label <- "Treatment proportion"
  if (variable == "prop_cattle_with_insecticide") this_label <- "Insecticide coverage"
  if (variable == "NW") this_label <- "Wildlife"
  if (variable == "K") this_label <- "Carrying capacity"
  this_label
}

my_pdfwidth <- function() {
  9
}

my_pdfheight <- function() {
  6
}

my_pointsize <- function() {
  3
}

my_theme <- function() {
  theme_grey(base_size = 16) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 19) # ,
      # panel.grid.major = element_blank() #,
      # panel.grid.minor = element_blank()
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


plot_type1_y_versus_treat_prop_facet_NW <- function(df, y_var) {
  df$y <- df[, y_var]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)

  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "K"), as.factor) %>%
    filter(
      prop_cattle_with_insecticide == 0,
      NW %in% c(0, 100, 250)
    ) %>%
    ggplot(aes(treat_prop, y, shape = K, colour = NW)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(shape = my_label("K"), colour = my_label("NW")) +
    facet_wrap(~NW) +
    my_theme()
  p
}

#-------------------------------------------------------------------------------
# Function Name: plot_type2_y_versus_treat_prop_facet_prop_cattle_with_insecticide
#
# Parameters:
#   df - Dataframe containing data.
#   this_K - Specific value of K to filter by.
#   y_var - String, the y-variable to be plotted.
#
# Outputs:
#   Returns a ggplot object with highlighted areas based on condition.
#
# Dependencies:
#   dplyr, ggplot2, gghighlight
#
#-------------------------------------------------------------------------------

plot_type2_y_versus_treat_prop_facet_prop_cattle_with_insecticide <- function(df, this_K, y_var) {
  df$y <- df[, y_var]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)

  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "K"), as.factor) %>%
    filter(
      prop_cattle_with_insecticide %in% c(0, 0.05, 0.1, 0.15, 0.2),
      NW %in% c(0, 100, 250),
      K == this_K
    ) %>%
    ggplot(aes(treat_prop, RiskA, shape = NW, colour = prop_cattle_with_insecticide)) +
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
#   this_K - Specific value of K to filter by.
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
    df, this_K, y_var, threshold_var, threshold) {
  df$y <- df[, y_var]
  df$threshold_var <- df[, threshold_var]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)

  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "K"), as.factor) %>%
    filter(
      prop_cattle_with_insecticide %in% c(0, 0.05, 0.1, 0.15, 0.2),
      NW %in% c(0, 100, 250),
      K == this_K
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
#   this_K - Specific value of K to filter by.
#
# Outputs:
#   Returns a ggplot object.
#
# Dependencies:
#   dplyr, ggplot2
#
#-------------------------------------------------------------------------------

plot_type4_y_versus_treat_prop_facet_NW <- function(df, y_var, this_K) {
  df$y <- df[, y_var]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)

  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "K"), as.factor) %>%
    filter(
      K == this_K,
      NW %in% c(0, 100, 250)
    ) %>%
    ggplot(aes(treat_prop, y, shape = K, colour = prop_cattle_with_insecticide)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    facet_wrap(~NW) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(shape = my_label("K"), colour = my_label("prop_cattle_with_insecticide")) +
    my_theme()
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

plot_type5_y_versus_prop_cattle_with_insecticide_facet_NW <- function(df, y_var, this_K) {
  df$y <- df[, y_var]
  this_xlab <- my_label("prop_cattle_with_insecticide")
  this_ylab <- my_label(y_var)

  p <- df %>%
    mutate_at(c("treat_prop", "NW", "K"), as.factor) %>%
    filter(
      prop_cattle_with_insecticide <= 0.5,
      treat_prop %in% c(0, 0.2, 0.4, 0.6, 0.8, 0.91),
      NW %in% c(0, 100, 250),
      K == this_K
    ) %>%
    ggplot(aes(prop_cattle_with_insecticide, y, shape = K, colour = treat_prop)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(colour = my_label("treat_prop")) +
    facet_wrap(~NW) +
    my_theme()
  p
}

#-------------------------------------------------------------------------------
# Function Name: plot_type6_y_versus_treat_prop_facet_NW_K
#
# Parameters:
#   df - Dataframe containing data.
#   y_var - String, the y-variable to be plotted.
#
# Outputs:
#   Returns a ggplot object.
#
# Dependencies:
#   dplyr, ggplot2
#
#-------------------------------------------------------------------------------

plot_type6_y_versus_treat_prop_facet_NW_K <- function(df, y_var) {
  df$y <- df[, y_var]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)

  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "K"), as.factor) %>%
    filter(
      K %in% c(500, 1000, 6000),
      NW %in% c(0, 100, 250)
    ) %>%
    ggplot(aes(treat_prop, y, shape = K, colour = prop_cattle_with_insecticide)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    facet_wrap(~ K + NW) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(shape = my_label("K"), colour = my_label("prop_cattle_with_insecticide")) +
    my_theme()
  p
}