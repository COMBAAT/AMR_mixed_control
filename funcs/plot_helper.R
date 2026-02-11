
# =========================================================
# Function Names: my_linewidth, my_pointsize, my_label, my_pdfwidth, my_pdfheight, my_theme,
#                 plot_type1_y_versus_treat_prop_facet_NW,
#                 plot_type2_y_versus_treat_prop_facet_prop_cattle_with_insecticide,
#                 plot_type3_y_versus_treat_prop_facet_prop_cattle_with_insecticide_with_highlight,
#                 plot_type4_y_versus_treat_prop_facet_NW,
#                 plot_type5_y_versus_prop_cattle_with_insecticide_facet_NW,
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
#source("funcs/plot_settings.R")

my_ggsave <- function(plot, filename, width, height) {
  ggsave(
    plot = plot,
    filename = filename,
    width = width,
    height = height,
    units = "in",
    device = cairo_pdf,
    limitsize = FALSE
  )
}

# update to remove the gridlines and make background offwhite for contrast
my_theme <- function() {
  theme_bw(base_size = 15) +
    theme(
      panel.background = element_rect(fill = "#FAFAFA"),
      plot.background  = element_rect(fill = "#FAFAFA", colour = NA),
      plot.title = element_text(hjust = 0.5, size = 1.0 * 15),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
      #axis.line        = element_line(colour = "#111111")
    )
}

my_theme_invasion <- function() {
  theme_bw(base_size = 20) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 1.0 * 15),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Specify plot formatting ------------------------------------------------------
my_linewidth <- function() {
  1
}

my_pointsize <- function() {
  2
}


my_label <- function(variable, split_across_lines = "default") {
  this_label <- variable
  if (variable == "fit_adj_new") this_label <- "Relative fitness"
  if (variable == "Risk_per_treatment") this_label <- "Risk per treatment"
  if (variable == "treat_prop") this_label <- "Case treatment proportion"
  if (variable == "treat_prop" & split_across_lines == "other") this_label <- "Case treatment \n proportion"
  if (variable == "coverage") this_label <- "Prophylactic coverage"
  if (variable == "coverage" & split_across_lines == "other") this_label <- "Prophylactic \n coverage"
  if (variable == "treatment_type") this_label <- "Treatment type"
  if (variable == "treatment_type" & split_across_lines == "other") this_label <- "Protocol"
  if (variable == "treatments_per_year") this_label <- "Treatments per year"
  if (variable == "R0sen") this_label <- "R0 sensitive"
  if (variable == "prevalence") this_label <- "Prevalence in cattle"
  if (variable == "prevalence_wildlife") this_label <- "Prevalence in wildlife"
  if (variable == "prevalence_vectors") this_label <- "Prevalence in vectors"
  if (variable == "Incidence") this_label <- "Incidence"
  if (variable == "Incidence_new") this_label <- "Incidence_new"
  if (variable == "No_trt_cat") this_label <- "Number treated"
  if (variable == "Prob_onward_tran") this_label <- "Prob onward transmission"
  if (variable == "RiskE") this_label <- "Risk of emergence and spread"
  if (variable == "RiskA") this_label <- "Selection opportunity"
  if (variable == "prop_cattle_with_insecticide") this_label <- "Insecticide coverage"
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
  if (variable == "curative") this_label <- "responsive curative"
  if (variable == "longlasting") this_label <- "responsive longlasting"
  if (variable == "proph_ongoing") this_label <- "ongoing longlasting"
  if (variable == "responsive_curative") this_label <- "Responsive curative"
  if (variable == "responsive_longlasting") this_label <- "Responsive longlasting"
  if (variable == "proph_ongoing") this_label <- "Ongoing longlasting"
  if (variable == "label") this_label <- "Treatment protocol"
  if (variable == "BCR_scenario") this_label <- "Benefit cost ratio"
  if (variable == "sum_averted_production_losses") this_label <- "Avoided losses \n USD per 100 cattle"
  if (variable == "treat_insecticide_cost") this_label <- "Treatment costs \n USD per 100 cattle"
  this_label
}

my_title <- function(variable, split_across_lines = "default") {
  if (variable == "curative") {this_title <- "Responsive \ncurative"}
  if (variable == "curative" & split_across_lines == "other") {this_title <- "Responsive curative"}
  if (variable == "longlasting") {this_title <- "Responsive \nlonglasting"}
  if (variable == "longlasting" & split_across_lines == "other") {this_title <- "Responsive longlasting"}
  if (variable == "ongoing") {this_title <- "Ongoing \nlonglasting"}
  if (variable == "ongoing" & split_across_lines == "other") {this_title <- "Ongoing longlasting"}
  this_title
}

add_fancy_title <- function(p, line1, line2, line3) {
  if (line1 != "") {
    p <- p +
      labs(
        title = paste0(
          "<span style='font-size:20pt;'>", line1, "</span><br>",
          "<span style='font-size:16pt;'>", line2, "</span><br>",
          "<span style='font-size:16pt;'>", line3, "</span>"
        )
      ) +
      theme(plot.title = element_markdown(hjust = 0.5))
    p
  } else {
    p <- p +
      labs(
        title = paste0(
          "<span style='font-size:16pt;'>", line3, "</span>"
        )
      ) +
      theme(plot.title = element_markdown(hjust = 0.5))
  }
  p
}

ymax_function <- function(y_var) {
  if (y_var == "R0sen") {
    ymax <- 25.0
  } else if (y_var == "RiskE") {
    ymax <- 12.5
  } else if (y_var == "treat_prop") {
    ymax <- 1
  } else if (y_var == "treatments_per_year") {
    ymax <- 12
  } else if (y_var == "RiskA") {
    ymax <- 10
  } else if (y_var == "Incidence") {
    ymax <- 1250
  } else if (y_var == "prevalence") {
    ymax <- 1.0
  } else if (y_var == "prevalence_vectors") {
    ymax <- 0.1
  } else if (y_var == "No_trt_cat") {
    ymax <- 1250
  } else if (y_var == "Rres_final") {
    ymax <- 10
  } else {
    ymax <- 1.0
  }
  return(ymax)
}

xmax_function <- function(x_var) {
  if (x_var == "treatments_per_year") {
    xmax <- 12
  } else {
    xmax <- 1
  }
  return(xmax)
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

my_theme_old <- function() {
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
    x_var = "treatments_per_year"
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
                                                    this_vector_measure, this_vector_measure_value, ttype,
                                                    vertical = FALSE) {
  x_var <- get_treat_var(df, ttype) 
  df$x <- df[, x_var]
  df$y <- df[, y_var]
  this_xlab <- my_label(x_var)
  this_ylab <- my_label(y_var)
  df <- df %>% mutate(Control = case_when(R0sen > 1 ~ "R0 > 1", R0sen <= 1 ~ "R0 <= 1"))
  df$shape_variable <- df[, this_vector_measure]
  
  if (vertical == TRUE) {
    this_ncol <- 1
    this_position = "bottom"
  } else {
    this_ncol <- length(this_NW_set)
    this_position = "right"
  }

  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", this_vector_measure), as.factor) %>%
    filter(
      get(this_vector_measure) == this_vector_measure_value,
      NW %in% this_NW_set
    ) %>%
    #ggplot(aes(x, y, shape = get(this_vector_measure), colour = prop_cattle_with_insecticide)) +
    ggplot(aes(x, y, shape = Control, colour = prop_cattle_with_insecticide)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    #ylim(c(0, y_max)) +
    facet_wrap(~NW, ncol = this_ncol) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    scale_shape_manual(values = c(4, 16)) +
    labs(shape = "Control\nefficacy", colour = my_label("prop_cattle_with_insecticide")) +
    my_theme() + theme(legend.position = this_position)
    coord_cartesian(ylim = c(0, ymax_function(y_var))) 
  p
}

plot_type4_y_versus_treat_prop_facet_NW <- function(df, y_var, this_NW_set, 
                                                    this_vector_measure, this_vector_measure_value, ttype,
                                                    vertical = FALSE) {
  x_var <- get_treat_var(df, ttype) 
  df$x <- df[, x_var]
  df$y <- df[, y_var]
  this_xlab <- my_label(x_var)
  this_ylab <- my_label(y_var)
  df <- df %>% mutate(Control = case_when(R0sen > 1 ~ "R0 > 1", R0sen <= 1 ~ "R0 <= 1"))
  df$shape_variable <- df[, this_vector_measure]
  
  if (vertical == TRUE) {
    this_ncol <- 1
    this_position = "bottom"
  } else {
    this_ncol <- 3
    this_position = "right"
  }
  
  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", this_vector_measure), as.factor) %>%
    filter(
      get(this_vector_measure) == this_vector_measure_value,
      NW %in% this_NW_set
    ) %>%
    #ggplot(aes(x, y, shape = get(this_vector_measure), colour = prop_cattle_with_insecticide)) +
    ggplot(aes(x, y, shape = Control, colour = prop_cattle_with_insecticide)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    #ylim(c(0, y_max)) +
    facet_wrap(~NW, ncol = this_ncol) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    scale_shape_manual(values = c(4, 16)) +
    labs(shape = "Control\nefficacy", colour = my_label("prop_cattle_with_insecticide")) +
    my_theme() + theme(legend.position = this_position) +
    scale_x_continuous(breaks = seq(0, xmax_function(x_var), by = xmax_function(x_var)/4)) +
    coord_cartesian(ylim = c(0, ymax_function(y_var))) 
  p
}

plot_type6_y_versus_treat_prop_facet_NW <- function(df, y_var, this_NW_set, 
                                                    this_vector_measure, this_vector_measure_value, ttype,
                                                    vertical = FALSE) {
  x_var <- get_treat_var(df, ttype) 
  df$x <- df[, x_var]
  df$y <- df[, y_var]
  this_xlab <- my_label(x_var)
  this_ylab <- my_label(y_var)
  df <- df %>% mutate(Control = case_when(R0sen > 1 ~ "R0 > 1", R0sen <= 1 ~ "R0 <= 1"))
  df$shape_variable <- df[, this_vector_measure]
  
  colours <- c("turquoise", "olivedrab3", "tomato", "mediumorchid1")
  names(colours) <- c("Sen outcompetes Res", "No Sen & Res can't invade", "Res outcompetes Sen", "No Sen & Res can invade")
  
  if (vertical == TRUE) {
    this_ncol <- 1
    this_position = "bottom"
  } else {
    this_ncol <- 3
    this_position = "right"
  }
  
  plot_this <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", this_vector_measure), as.factor) %>%
    filter(
      get(this_vector_measure) == this_vector_measure_value,
      NW %in% this_NW_set
    )
   insecticide_vec <- unique(plot_this$prop_cattle_with_insecticide)
   x_vec <- sort(unique(plot_this$x))
   n_x <- length(x_vec)
   
   # set label position
   # if (x_var == "treat_prop") {
   #   this_by = 2
   #   this_nudge = 0.075
   #   this_end = 11
   #   df_test <- data.frame(x = x_vec[n_x - seq(1, this_end, by = this_by) ],
   #                         prop_cattle_with_insecticide = insecticide_vec) 
   # } else {
   #   this_nudge = 0
   #   n_positions <- length(insecticide_vec)
   #   positions <- sort(sample(x_vec, n_positions), decreasing = TRUE)
   #   positions <- c(12, 10, 8, 6, 4, 2)
   #   df_test <- data.frame(x = positions,
   #                         prop_cattle_with_insecticide = insecticide_vec) 
   # }
   # 
   # df_labels <- inner_join(df_test, plot_this) %>% 
   #   mutate(label = prop_cattle_with_insecticide, 
   #          x_location = x - this_nudge,
   #          y_location = Rres_final)
   # # end set labels
  
  p <- plot_this %>%
    ggplot(aes(x, y)) +
    scale_color_manual(values = colours) +
    geom_line(aes(x, y, linetype = prop_cattle_with_insecticide, colour = Region), 
              linewidth = 0.5 * my_linewidth(), colour = "grey20") +
    
    geom_abline(aes(intercept = 1, slope = 0), colour = "red") +
    geom_point(aes(shape = Region, colour = Region), size = 1.0 * my_pointsize()) +
    
    # geom_label(data = df_labels, aes(x = x_location, y = y_location, label = label),
    #                  nudge_x = 0.0,
    #                  na.rm = TRUE, colour = "blue", size = 2) +
    facet_wrap(~NW, ncol = this_ncol) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    scale_shape_manual(values = c(1, 1, 16, 16)) +
    labs(shape = "Region", colour = "Region", linetype = my_label("prop_cattle_with_insecticide")) +
    my_theme() + theme(legend.position = this_position) +
    coord_cartesian(ylim = c(0, ymax_function(y_var)), xlim = c(0, xmax_function(x_var))) +
    scale_x_continuous(breaks = seq(0, xmax_function(x_var), by = xmax_function(x_var)/4))
    
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
                                                                      this_vector_measure, this_vector_measure_value,
                                                                      vertical = FALSE) {
  df$y <- df[, y_var]
  df$shape_variable <- df[, this_vector_measure]
  this_xlab <- my_label("prop_cattle_with_insecticide")
  this_ylab <- my_label(y_var)
  
  if (vertical == TRUE) {
    this_ncol <- 1
    this_position = "bottom"
  } else {
    this_ncol <- 3
    this_position = "right"
    }
  
  desired_vector <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9) # desired values, used to include 0.91
  actual_vector <- unique(df$treat_prop)
  nearest_vector <- find_nearest_vector(desired_vector, actual_vector)

  p <- df %>%
    mutate_at(c("treat_prop", "NW", this_vector_measure, "shape_variable"), as.factor) %>%
    filter(
      #prop_cattle_with_insecticide <= 0.5,
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
    ylim(0, ymax_function(y_var)) +
    labs(colour = my_label("treat_prop", split_across_lines = "other"), shape = my_label(this_vector_measure)) +
    facet_wrap(~NW, ncol = this_ncol) +
    my_theme() + theme(legend.position = this_position)
  p
}

plot_type5_y_versus_prop_cattle_with_insecticide_facet_NW_ttype3 <- function(df, y_var, this_NW_set, 
                                                                      this_vector_measure, this_vector_measure_value,
                                                                      vertical = FALSE) {
  df$y <- df[, y_var]
  df$shape_variable <- df[, this_vector_measure]
  this_xlab <- my_label("prop_cattle_with_insecticide")
  this_ylab <- my_label(y_var)
  
  if (vertical == TRUE) {
    this_ncol <- 1
    this_position = "bottom"} else {
      this_ncol <- 3
      this_position <- "right"}
  
  
  
  p <- df %>% mutate(proph_frequency = set_days_per_year() * proph_ongoing) %>%
    mutate_at(c("proph_frequency", "NW", this_vector_measure, "shape_variable"), as.factor) %>%
    filter(
      #prop_cattle_with_insecticide <= 0.5,
      NW %in% this_NW_set,
      get(this_vector_measure) == this_vector_measure_value,
      proph_frequency %in% c(0, 1, 2, 3, 6, 8)
    ) %>%
    ggplot(aes(prop_cattle_with_insecticide, y, shape = shape_variable, colour = proph_frequency)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    ylim(0, ymax_function(y_var)) +
    labs(colour = "Doses per year", shape = my_label(this_vector_measure)) +
    facet_wrap(~NW, ncol = this_ncol) +
    my_theme() + theme(legend.position = this_position)
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

my_labeller <- labeller(
  NW = function(x) paste0(my_label("NW"), ": ", x),
  cc_or_vh_ratio = function(x) paste0("Vector host ratio:", x)
)



plot_other_landscape <- function(df, colour_var, max_value, ttype, panel_type = "all") {
  df <- df %>% mutate(colour_var = .data[[colour_var]])
  #df$colour_var <- df[, colour_var]
  y_var <- get_treat_var(df, ttype)
  df <- as.data.frame(df)
  df$y <- df[, y_var]
  
  all_levels <- c("R0sen > 1", "R0sen < 1", "R0sen = 0")
  
  df$R0sen_gt_1 <- factor(df$R0sen_gt_1, levels = all_levels)
  
  
    if (panel_type == "single") {
    plot_this <- df %>%
    filter(near(treat_prop, 0.95) | near(treat_prop, 0.99) | treat_prop <= 0.9) %>%
    mutate(cc_or_vh_ratio = get(this_vector_measure)) %>%
    filter(Baseline_vector_host_ratio == 20, NW == 100)
    pt_size = 5.0
    contour_label_size = 0.5
    contour_linewidth <- 1.0
    } else {
      plot_this <- df %>%
      filter(near(treat_prop, 0.95) | near(treat_prop, 0.99) | treat_prop <= 0.9) %>%
        mutate(cc_or_vh_ratio = get(this_vector_measure))
      pt_size = 2.0
      contour_label_size = 0.5
      contour_linewidth <- 1.0
    }
    
    plot <- plot_this %>% ggplot() +
    geom_point(aes(x = prop_cattle_with_insecticide, y = y, 
                   colour = colour_var, shape = R0sen_gt_1), size = 4, show.legend = TRUE) +
      # geom_contour(data = plot_this, aes(
      #   x = prop_cattle_with_insecticide,
      #   y = y, z = RiskA), colour = "grey30", linewidth = contour_linewidth) +
      # geom_label_contour(data = plot_this, aes(
      #   x = prop_cattle_with_insecticide,
      #   y = y, z = RiskA), colour = "grey30", label.size = contour_label_size) +
    xlab(my_label("prop_cattle_with_insecticide", "other")) +
    ylab(my_label(y_var)) +
    facet_wrap(~ NW + cc_or_vh_ratio, labeller = my_labeller) +
    # ggtitle(paste(
    #   "Treatment type = ", unique(df$treatment_type), "; ",
    #   "Use carrying capacity = ", unique(df$use_carrying_capacity), "; ",
    #   "Maintain vector pop = ", unique(df$maintain_vector_pop)
    # )) + 
    ggtitle(paste(
      "Treatment type = ", my_label(unique(df$treatment_type))
    )) + 
    scale_colour_gradientn(colours = terrain.colors(15), limits = c(0, max_value)) +
    #ylim(c(0, ymax)) +
    scale_y_continuous(breaks = seq(0, ymax_function(y_var), by = ymax_function(y_var) / 4)) +
    scale_shape_manual(values = c(16, 1, 4), drop = FALSE) +
    labs(colour = my_label(colour_var), shape = my_label("R0sen_gt_1")) +
    my_theme_invasion()
  
  if (mainvecpop == F) {
    plan = "Collective"
  } else {
    plan = "Local"
  }
  if (panel_type != "single") {
    plot <- plot + ggtitle(paste0(
      "Treatment type: ", my_label(unique(df$laXbel)), "    Insecticide delivery: ", plan
    )) 
  } else {
    plot <- plot + ggtitle(paste0(
      "Treatment type: ", my_label(unique(df$laXbel))
    )) 
  }
  
  plot
}


plot_type7_y_versus_treat_prop_facet_NW <- function(df, y_var, this_NW_set, 
                                                      this_vector_measure, this_vector_measure_value, ttype,
                                                      vertical = FALSE) {
  
  reduced_df <- df %>% filter(prop_cattle_with_insecticide < 0.3)
  current_fitness <- unique(reduced_df$fit_adj_new)
  
  new_df <- data.frame()
  for (fitness in c(1.0, 0.8, 0.6)) {
    adjusted_df <- reduced_df %>% 
      mutate(Rres_final = Rres_final / current_fitness * fitness,
             fit_adj_new = fitness)
    new_df <- rbind(new_df, adjusted_df)
  }
  
  df <- new_df
  df <- add_competition_and_invasion_columns(df)
  
  #df %>% filter(NW == 200, treat_prop > 0.85, prop_cattle_with_insecticide > 0.18, 
  #              prop_cattle_with_insecticide < 0.22, Baseline_vector_host_ratio == 20) %>% glimpse()
  #df %>% filter(NW == 200, treat_prop > 0.85, prop_cattle_with_insecticide > 0.18, 
  #                     prop_cattle_with_insecticide < 0.22, Baseline_vector_host_ratio == 20) %>% select(Region, Rres_final, Rsen_final) %>% print()
  
  x_var <- get_treat_var(df, ttype) 
  df$x <- df[, x_var]
  df$y <- df[, y_var]
  this_xlab <- my_label(x_var)
  this_ylab <- my_label(y_var)
  df <- df %>% mutate(Control = case_when(R0sen > 1 ~ "R0 > 1", R0sen <= 1 ~ "R0 <= 1"))
  df$shape_variable <- df[, this_vector_measure]
  
  colours <- c("turquoise", "olivedrab3", "tomato", "mediumorchid1")
  names(colours) <- c("Sen outcompetes Res", "No Sen & Res can't invade", "Res outcompetes Sen", "No Sen & Res can invade")
  
  if (vertical == TRUE) {
    this_ncol <- 1
    this_position = "bottom"
  } else {
    this_ncol <- 3
    this_position = "right"
  }
  
  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "fit_adj_new", "NW", this_vector_measure), as.factor) %>%
    filter(
      get(this_vector_measure) == this_vector_measure_value,
      NW %in% this_NW_set
    ) %>%
    #ggplot(aes(x, y, shape = get(this_vector_measure), colour = prop_cattle_with_insecticide)) +
    ggplot(aes(x, y, shape = Region, linetype = fit_adj_new, colour = Region)) +
    #ggplot(aes(x, y, shape = NW, linetype = NW, colour = Region)) +
    scale_color_manual(values = colours) +
    
    geom_line(linewidth = 0.5 * my_linewidth(), colour = "grey20") +
    geom_abline(aes(intercept = 1, slope = 0), colour = "red") +
    geom_point(size = 1.0 * my_pointsize()) +
    #ylim(c(0, y_max)) +
    facet_wrap(~ prop_cattle_with_insecticide + NW, ncol = this_ncol) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    scale_shape_manual(values = c(1, 1, 16, 16)) +
    #labs(shape = "Control\nefficacy", colour = my_label("prop_cattle_with_insecticide")) +
    labs(shape = "Region", colour = "Region", linetype = my_label("fit_adj_new")) +
    my_theme() + theme(legend.position = this_position) +
    coord_cartesian(ylim = c(0, 4)) 
  #coord_cartesian(ylim = c(0, 0.25 * ymax_function(y_var))) 
  p
}


rescale_fitness_and_Rres <- function(df, new_fitness) {
  current_fitness <- unique(df$fit_adj_new)
  rescale <- new_fitness / current_fitness
  new_df <- df %>% mutate(fit_adj_new = fit_adj_new * rescale, Rres_final = Rres_final * rescale)
  new_df
}

create_df_with_all_fitnesses <- function(df, fitness_vec) {
  new_df <- data.frame()
  for (fitness in fitness_vec) {
    rescaled_df <- rescale_fitness_and_Rres(df, fitness)
    new_df <- rbind(new_df, rescaled_df)
  }
  new_df
}

plot_type8 <- function(df, ttype) {
  fitness_vec <- seq(0.4, 1.0, by = 0.05)
  new_df <- create_df_with_all_fitnesses(df, fitness_vec)
  
  if (ttype != 3) {
  plot_this <- new_df %>%
    filter(Rres_final < 1) %>%
    group_by(NW, prop_cattle_with_insecticide, fit_adj_new) %>%
    summarise(max_drug_use = max(treat_prop))
  } else {
    plot_this <- new_df %>%
      filter(Rres_final < 1) %>%
      group_by(NW, prop_cattle_with_insecticide, fit_adj_new) %>%
      summarise(max_drug_use = max(treatments_per_year))
  }
  
  p <- plot_this %>%
    # filter(NW == 100) %>%
    #filter(prop_cattle_with_insecticide < 0.25) %>%
    mutate(NW = as.factor(NW)) %>%
    mutate(prop_cattle_with_insecticide = as.factor(prop_cattle_with_insecticide)) %>%
    ggplot(aes(
      y = max_drug_use, x = fit_adj_new,
      colour = prop_cattle_with_insecticide
    )) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = 0.8, linetype = "dashed") +
    facet_wrap(~NW) +
    labs(colour = my_label("prop_cattle_with_insecticide")) +
    xlab(my_label("fit_adj_new")) + ylab("Maximum treatment") +
    my_theme()
  
  p
}



plot_invasion_landscape <- function(prev_threshold, df, ttype, mainvecpop, 
                                    panel_type = "all", with_contours = FALSE, with_grey = TRUE) {
  
  #df <- df %>% mutate(Region = case_when(R0sen < 1e-06 ~ "R0_eq_0", TRUE ~ Region))
  all_levels <- c("R0sen > 1", "R0sen < 1", "R0sen = 0")
  
  df$R0sen_gt_1 <- factor(df$R0sen_gt_1, levels = all_levels)
  
  y_var <- get_treat_var(df, ttype)
  df <- as.data.frame(df)
  df$y <- df[, y_var]

  ymax <- ymax_function(y_var)

  prev_threshold_label <- paste0("prev > ", prev_threshold)
  colours <- c("turquoise", "olivedrab3", "tomato", "mediumorchid1", "grey80")
  names(colours) <- c("Sen outcompetes Res", "No Sen & Res can't invade", "Res outcompetes Sen", "No Sen & Res can invade", "R0_eq_0")

  if (panel_type == "single") {
  plot_this <- df %>%
    # filter(near(treat_prop, 0.95) | near(treat_prop, 0.99) | treat_prop <= 0.9) %>%
    filter(near(treat_prop, 0.99) | treat_prop <= 0.9) %>%
    mutate(cc_or_vh_ratio = get(this_vector_measure)) %>%
    filter(Baseline_vector_host_ratio == this_vector_measure_value, NW == 100)
  pt_size = 4.0
  stroke_size = 1.25
  contour_label_size = 0.5
  contour_linewidth <- 1.0
  } else {
    plot_this <- df %>%
      # filter(near(treat_prop, 0.95) | near(treat_prop, 0.99) | treat_prop <= 0.9) %>%
      filter(near(treat_prop, 0.99) | treat_prop <= 0.9) %>%
      mutate(cc_or_vh_ratio = get(this_vector_measure))
    pt_size = 2.0
    stroke_size = 1.0
    contour_label_size = 0.5
    contour_linewidth <- 1.0
  }

  plot <- ggplot() +
    geom_point(data = plot_this, aes(
      x = prop_cattle_with_insecticide, y = y, colour = Region,
      shape = R0sen_gt_1
    ), size = pt_size, stroke = stroke_size, show.legend = TRUE) +
    # geom_contour(data = plot_this, aes(
    #   x = prop_cattle_with_insecticide, 
    #   y = y, z = prevalence), colour = "grey30", linewidth = contour_linewidth) +
    # geom_label_contour(data = plot_this, aes(
    #   x = prop_cattle_with_insecticide, 
    #   y = y, z = prevalence), colour = "grey30", label.size = contour_label_size) +
    scale_color_manual(values = colours) +
    xlab(my_label("prop_cattle_with_insecticide", "other")) +
    ylab(my_label(y_var)) +
    facet_wrap(~ NW + cc_or_vh_ratio, labeller = my_labeller) +
    ylim(c(0, ymax)) +
    # scale_x_continuous(breaks = seq(0, xmax_function(x_var), by = xmax_function(x_var)/4)) +
    scale_y_continuous(breaks = seq(0, ymax_function(y_var), by = ymax_function(y_var) / 4)) +
    scale_shape_manual(values = c(16, 1, 4), drop = FALSE) +
    labs(shape = my_label("R0sen_gt_1")) +
    my_theme_invasion()
  
  if (with_contours == TRUE) {
  plot <- plot + geom_contour(data = plot_this, aes(
       x = prop_cattle_with_insecticide, 
       y = y, z = prevalence), colour = "grey30", linewidth = contour_linewidth) +
    # geom_text(
    #   stat = "contour",
    #   aes(label = after_stat(level)),
    #   size = 3
    # )
     geom_label_contour(data = plot_this, aes(
        x = prop_cattle_with_insecticide, 
        y = y, z = prevalence), colour = "grey30", label.size = contour_label_size)
  }
  
  with_grey <- TRUE
  if (with_grey == TRUE) {
    plot_this_grey <- plot_this %>% filter(R0sen < 1e-06)
    plot <- plot + geom_point(data = plot_this_grey, aes(
      x = prop_cattle_with_insecticide, y = y
    ), colour = "grey90", shape = 4, size = pt_size, stroke = 1.0)
  }
  
  if (mainvecpop == F) {
    plan = "Collective"
  } else {
    plan = "Local"
  }
  if (panel_type != "single") {
    plot <- plot + ggtitle(paste0(
         "Treatment type: ", my_label(unique(df$laXbel)), "    Insecticide delivery: ", plan
       )) 
  } else {
    plot <- plot + ggtitle(paste0(
      "Treatment type: ", my_label(unique(df$laXbel))
    )) 
  }

  plot
}


get_subset <- function(df_ttype1, df_ttype2, df_ttype3, ttype) {
  if (ttype == 1) {
    df <- df_ttype1_F
  }
  if (ttype == 2) {
    df <- df_ttype2_F
  }
  if (ttype == 3) {
    df <- df_ttype3_F
  }
  df
}


# End of script ---------------------------------------------------------------
