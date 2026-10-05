library(cowplot)



# Plot the cost of vs. treatment proportion with insecticide application 
# with the legends on the right side
plot_type4_y_versus_treat_prop_facet_NW_1 <- function(df, y_var, this_K, this_NW_set) {
  df$y <- df[, y_var]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)
  
  p <- df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "K"), as.factor) %>%
    filter(
      K == this_K,
      NW %in% this_NW_set
    ) %>%
    ggplot(aes(treat_prop, y, shape = K, colour = prop_cattle_with_insecticide)) +
    #   geom_hline(yintercept = 0, linetype = "dashed", colour = "black", linewidth = my_linewidth()) + 
    #   geom_hline(yintercept = 1, linetype = "dashed", colour = "black", linewidth = my_linewidth()) + 
    geom_point(size = my_pointsize()) +
    #    scale_y_continuous(limits = c(0,1)) +
    geom_line(linewidth = my_linewidth()) +
    facet_wrap(~NW) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(shape = NULL, colour = my_label("prop_cattle_with_insecticide")) +
    guides(
      shape  = "none"
    ) +
    my_theme()
  p
}


plot_type4_y_versus_treat_prop_facet_NW_3_VHR <- function(df, y_var, this_VHR, this_NW_set, ylim_max = 1000) {
  print("here")
  df$y <- df[[y_var]]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)
  df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "K"), as.factor) %>%
    filter(VHR == this_VHR, NW %in% this_NW_set) %>%
    ggplot(aes(treat_prop, y, shape = K, colour = prop_cattle_with_insecticide)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    facet_wrap(~NW) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    scale_y_continuous(limits = c(0, ylim_max)) +
    labs(shape = NULL, colour = my_label("prop_cattle_with_insecticide")) +
    guides(
      shape  = "none",
      colour = guide_legend(
        #    title.position = "left",
        title.hjust    = 0,
        # direction      = "horizontal",
        ncol           = 1,
        override.aes   = list(shape = 16)
      )) +
    my_theme_1() +
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.box       = "vertical",
      legend.title     = element_text(hjust = 0) 
    )
}



plot_type4_y_versus_treat_prop_facet_NW_3_K <- function(df, y_var, this_K, this_NW_set, ylim_max = 1000) {
  df$y <- df[[y_var]]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)
  df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "K"), as.factor) %>%
    filter(K == this_K, NW %in% this_NW_set) %>%
    ggplot(aes(treat_prop, y, shape = K, colour = prop_cattle_with_insecticide)) +
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    facet_wrap(~NW) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    scale_y_continuous(limits = c(0, ylim_max)) +
    labs(shape = NULL, colour = my_label("prop_cattle_with_insecticide")) +
    guides(
      shape  = "none",
      colour = guide_legend(
    #    title.position = "left",
        title.hjust    = 0,
       # direction      = "horizontal",
        ncol           = 1,
        override.aes   = list(shape = 16)
      )) +
    my_theme_1() +
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.box       = "vertical",
      legend.title     = element_text(hjust = 0) 
    )
}


plot_type4_y_versus_treat_prop_facet_NW_3_update <- function(df, y_var, this_K, this_NW_set, ylim_max = 1000, highlight_prop, dim_alpha = 0.15) {
  df$y <- df[[y_var]]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)
  df2 <- df %>%
    filter(K == this_K, NW %in% this_NW_set) %>%
    mutate(
      prop_num = as.numeric(as.character(prop_cattle_with_insecticide)),
      is_highlight = prop_num %in% highlight_prop,
      prop_cattle_with_insecticide = as.factor(prop_cattle_with_insecticide),
      NW = as.factor(NW),
      K  = as.factor(K))
  ggplot() +
    geom_line(
      data = df2,
      aes(treat_prop, y, group = prop_cattle_with_insecticide),
      colour = "grey70", alpha = dim_alpha, linewidth = my_linewidth()) +
    geom_point(
      data = df2,
      aes(treat_prop, y, group = prop_cattle_with_insecticide),
      colour = "grey70", alpha = dim_alpha, size = my_pointsize()) +
    geom_line(
      data = df2 %>% filter(is_highlight),
      aes(treat_prop, y, colour = prop_cattle_with_insecticide),
      linewidth = my_linewidth() * 1.6) +
    geom_point(
      data = df2 %>% filter(is_highlight),
      aes(treat_prop, y, colour = prop_cattle_with_insecticide),
      size = my_pointsize() * 1.3) +
    facet_wrap(~NW) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    scale_y_continuous(limits = c(0, ylim_max)) +
    labs(colour = my_label("prop_cattle_with_insecticide")) +
    guides(
      colour = guide_legend(
        ncol = 1,
        override.aes = list(shape = 16, alpha = 1, linewidth = my_linewidth() * 1.6))) +
    my_theme_1() +
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.box = "vertical",
      legend.title = element_text(hjust = 0)
    )
}


###############################################
###############################################
#Plotting the Benefit cost ratio with the area below the curve shaded
plot_type4_y_versus_treat_prop_facet_NW_BCR <- function(df, y_var, this_K, this_NW_set, ylim_max) {
  df$y <- df[[y_var]]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)
  
  df %>%
    mutate_at(c("prop_cattle_with_insecticide", "NW", "K"), as.factor) %>%
    filter(
      K == this_K,
      NW %in% this_NW_set
    ) %>%
    ggplot(aes(treat_prop, y, shape = K, colour = prop_cattle_with_insecticide)) +
    
    geom_point(size = my_pointsize()) +
    geom_line(linewidth = my_linewidth()) +
    facet_wrap(~NW) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    scale_y_continuous(limits = c(0, ylim_max)) +
    #     scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 2)) +    
    geom_hline(yintercept = 1, linetype = "dashed", colour = "black", linewidth = my_linewidth()) + 
    labs(shape = NULL, colour = my_label("prop_cattle_with_insecticide")) +
    guides(
      shape  = "none",
      colour = guide_legend(
    #    title.position = "left",
        title.hjust    = 0,
     #   direction      = "horizontal",
     #   nrow           = 1,
        override.aes   = list(shape = 16)
      )
    ) +
    annotate(
      "rect",
      xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = 1,
      alpha = 0.7,
      fill = "grey"
    ) +
    my_theme_1() 
}


###############################################
###############################################
# Plotting the benefit-cost ratio as a heatmap with specified limits
plot_bcr1 <- function(df, y_var, this_K, this_NW_set, bcr_min, bcr_max){
  
  df %>%
    filter(K == this_K, NW %in% this_NW_set) %>%
    mutate(
      y = .data[[y_var]],
      prop_cattle_with_insecticide = as.factor(prop_cattle_with_insecticide),
      treat_prop = as.factor(treat_prop),
      NW = as.factor(NW),
      K = as.factor(K)
    ) %>%
    ggplot(aes(prop_cattle_with_insecticide, treat_prop, fill = y)) +
    geom_tile() +
    geom_tile(
      data = ~ filter(.x, is.na(y)),
      color = "black",
      fill = NA,
      linewidth = 0.6
    ) +
    scale_fill_viridis_c(
      option = "plasma",
      limits = c(bcr_min, bcr_max),
      na.value = "grey80",
      name = "BCR"     
      
    ) +
    facet_wrap(~ NW) +
    xlab(my_label("prop_cattle_with_insecticide")) +
    ylab(my_label("treat_prop")) +
    my_theme_1()
}
