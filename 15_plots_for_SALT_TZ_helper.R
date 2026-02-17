# plots for SALT_TZ helper
source("funcs/plot_settings.R")
###############################################################################
my_pt_size = 2.0
my_line_width = 1.0

###############################################################################
test_palette <- function() {
  df <- data.frame(
    x = rep(1:6, each = 5),
    y = rep(1:5, times = 6),
    g = factor(rep(letters[1:6], each = 5))
  )
  
  p <- ggplot(df, aes(x, y, colour = g)) +
    geom_line(linewidth = 4) +
    geom_point(size = 3) +
    labs(
      title = "Current ggplot2 discrete colour palette",
      subtitle = "Each group = one colour"
    ) +
    theme_minimal()
  print(p)
  p
}

###############################################################################
#props_all = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
#props_all2 = c(0, 0.05, 0.1, 0.15, 0.2, 0.25)

get_base_cols <- function(props_all, my_palette) {
  repeated_palette <- rep(my_palette, 3)
  repeated_palette <- repeated_palette[1:length(props_all)]
  base_cols <- setNames(
    repeated_palette,
    as.character(props_all)
  )
  base_cols
}
# base_cols <- setNames(
#   projector_cols_warm_first,
#   as.character(props_all)
# )
# base_cols2 <- setNames(
#   projector_cols_warm_first,
#   as.character(props_all2)
# )

make_cols_highlight <- function(highlight_props,
                                #props_all = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
                                props_all,
                                base_cols,
                                grey = "grey70") {
  props_chr <- as.character(props_all)
  hl_chr <- as.character(highlight_props)
  
  cols <- setNames(rep(grey, length(props_chr)), props_chr)
  cols[hl_chr] <- base_cols[hl_chr]  # keep identity colour for highlighted
  cols
}
###############################################################################
get_core_plot <- function(df, ttype, y_max, x_var_label, x_breaks, x_labs, plot_titles) {
  p <- ggplot(df) +
    geom_point(aes(x = x, y = y, colour = prop_cattle_with_insecticide_factor), size = my_pt_size) +
    geom_line(aes(x = x, y = y, colour = prop_cattle_with_insecticide_factor), linewidth = my_line_width) +
    coord_cartesian(ylim = c(0, y_max)) +
    scale_x_continuous(breaks = x_breaks, labels = x_labs) +
    labs(colour = "Insecticide \n coverage") +
    ylab(my_label(y_var)) + xlab(x_var_label) +
    scale_colour_manual(values = cols, limits = names(base_cols), drop = FALSE) +
    ggtitle(plot_titles[ttype]) +
    #scale_x_continuous(breaks = seq(0, 1, by = 0.25)) +
    my_theme_SALT_TZ()
  p
}

###############################################################################
get_Rres_plot <- function(df, ttype, y_max, x_var_label, x_breaks, x_labs, plot_titles, rectangle) {
  if (rectangle == TRUE) {
    rectangle_colour = "grey70"
    rectangle_alpha = 0.3
  } else {
    rectangle_colour = "orange"
    rectangle_alpha = 0.0
  }
  p <- ggplot(df) +
    geom_rect(
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1,
      fill = rectangle_colour,
      alpha = rectangle_alpha,
      inherit.aes = FALSE
    ) +
    geom_point(aes(x = x, y = y, colour = prop_cattle_with_insecticide_factor), size = my_pt_size) +
    geom_line(aes(x = x, y = y, colour = prop_cattle_with_insecticide_factor), linewidth = my_line_width) +
    coord_cartesian(ylim = c(0, y_max)) +
    scale_x_continuous(breaks = x_breaks, labels = x_labs) +
    labs(colour = "Insecticide \n coverage") +
    ylab("Resistant strain spread (R)") + xlab(x_var_label) +
    scale_colour_manual(values = cols, limits = names(base_cols), drop = FALSE) +
    ggtitle(plot_titles[ttype]) +
    my_theme_SALT_TZ()
  p
}

###############################################################################
get_BCR_plot <- function(df, ttype, y_max, x_var_label, x_breaks, x_labs, plot_titles, rectangle) {
  if (rectangle == TRUE) {
    alpha_rectangle = 0.3 }
  else {
    alpha_rectangle = 1.0
  }

  p <- ggplot(df) +
    geom_rect(
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1,
      fill = "grey70",
      alpha = alpha_rectangle,
      inherit.aes = FALSE
    ) +
    # geom_rect(
    #   xmin = -Inf, xmax = Inf, ymin = 1, ymax = 2.5,
    #   fill = "grey90",
    #   alpha = alpha_rectangle,
    #   inherit.aes = FALSE
    # ) +
    geom_point(aes(x = x, y = y, colour = prop_cattle_with_insecticide_factor), size = my_pt_size) +
    geom_line(aes(x = x, y = y, colour = prop_cattle_with_insecticide_factor), linewidth = my_line_width) +
    geom_abline(aes(slope = 0, intercept = 1), linetype = "dashed") +
    #geom_abline(aes(slope = 0, intercept = 2.5), linetype = "dashed") +
    coord_cartesian(ylim = c(0, y_max)) +
    scale_x_continuous(breaks = x_breaks, labels = x_labs) +
    labs(colour = "Insecticide \n coverage") +
    ylab(my_label(y_var)) + xlab(x_var_label) +
    scale_colour_manual(values = cols, limits = names(base_cols), drop = FALSE) +
    ggtitle(plot_titles[ttype]) +
    my_theme_SALT_TZ()
  p
}

###############################################################################
plot_panel_by_treatment_type <- function(plot_this, y_var, y_max, rectangle = FALSE) {
  
  plot_titles <- c(" Responsive \n curative ", " Responsive \n longlasting ", " Prophylactic \n longlasting ")
  
  plots <- list()
  for (ttype in 1:3){
    if (ttype != 3) {
      x_var = "treat_prop"
      x_var_label = " Proportion of \n cases treated "
      x_breaks = c(0, 0.5, 1.0)
      x_labs = sprintf("%.1f", x_breaks)
    } else {
      x_var = "treatments_per_year"
      x_var_label = " Herd treatments \n per year "
      x_breaks = as.numeric(c(0, 3, 6, 9))
      x_labs = sprintf("%.1f", x_breaks)
    }
    plot_this$x = plot_this[, x_var]
    
    plot_this_ttype <- plot_this %>% filter(treatment_code == ttype) 
    
    if (y_var == "Rres_final") {
      plots[[ttype]] <- get_Rres_plot(plot_this_ttype, ttype, y_max, x_var_label, x_breaks, x_labs, 
                                      plot_titles, rectangle)
    } else if (y_var == "BCR_scenario") { 
      plots[[ttype]] <- get_BCR_plot(plot_this_ttype, ttype, y_max, x_var_label, x_breaks, x_labs,
                                      plot_titles, rectangle)
    } else {
      plots[[ttype]] <- get_core_plot(plot_this_ttype, ttype, y_max, x_var_label, x_breaks, x_labs,
                                      plot_titles)
    }
  }
  
  p <- (plots[[1]] + plots[[2]] + plots[[3]]) + 
    plot_layout(guides = "collect", axes = "collect") &
    plot_annotation(
      theme = theme(
        plot.background = element_rect(fill = NA, colour = NA)
      )
    )
  p
  #plots[[1]]
}
###############################################################################