# Specify plot formatting ------------------------------------------------------
my_linewidth <- function() {
  1
}

my_pointsize <- function() {
  3
}


my_label <- function(variable) {
  if (variable == "R0_sen") this_label <- "R0 sensitive"
  if (variable == "prevalence") this_label <- "Prevalence"
  if (variable == "Incidence") this_label <- "Incidence"
  if (variable == "No_trt_cat") this_label <- "Number treated cattle"
  if (variable == "Prob_onward_tran") this_label <- "Prob onward transmission"
  if (variable == "RiskE") this_label <- "Risk of emergence and spread"
  if (variable == "RiskA") this_label <- "Risk of emergence"
  if (variable == "treat_prop") this_label <- "Treatment proportion"
  if (variable == "prop.insecticide") this_label <- "Insecticide coverage"
  if (variable == "W_st") this_label <- "Wildlife"
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

plot_type1_y_versus_treat_prop_facet_W_st <- function(df, y_var) {
  df$y <- df[, y_var]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)

  p <- df %>%
    mutate_at(c("prop.insecticide", "W_st", "K"), as.factor) %>%
    filter(
      prop.insecticide == 0,
      W_st %in% c(0, 100, 250)
    ) %>%
    ggplot(aes(treat_prop, y, shape = K, colour = W_st)) +
    geom_point(size = my_pointsize()) +
    geom_line(size = my_linewidth()) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(shape = my_label("K"), colour = my_label("W_st")) +
    facet_wrap(~W_st) +
    my_theme()
  p
}

plot_type2_y_versus_treat_prop_facet_prop_insecticide <- function(df, this_K, y_var) {
  df$y <- df[, y_var]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)

  p <- df %>%
    mutate_at(c("prop.insecticide", "W_st", "K"), as.factor) %>%
    filter(
      prop.insecticide %in% c(0, 0.05, 0.1, 0.15, 0.2),
      W_st %in% c(0, 100, 250),
      K == this_K
    ) %>%
    ggplot(aes(treat_prop, RiskA, shape = W_st, colour = prop.insecticide)) +
    geom_point(size = my_pointsize()) +
    geom_line(size = my_linewidth()) +
    facet_wrap(~prop.insecticide) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(shape = my_label("W_st"), colour = my_label("prop.insecticide")) +
    my_theme()
  p
}

plot_type3_y_versus_treat_prop_facet_prop_insecticide_with_higlight <- function(
    df, this_K, y_var, threshold_var, threshold) {
  df$y <- df[, y_var]
  df$threshold_var <- df[, threshold_var]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)

  p <- df %>%
    mutate_at(c("prop.insecticide", "W_st", "K"), as.factor) %>%
    filter(
      prop.insecticide %in% c(0, 0.05, 0.1, 0.15, 0.2),
      W_st %in% c(0, 100, 250),
      K == this_K
    ) %>%
    ggplot(aes(treat_prop, y,
      group = interaction(W_st, prop.insecticide),
      shape = W_st, colour = prop.insecticide
    )) +
    geom_point(size = my_pointsize()) +
    geom_line(size = my_linewidth()) +
    gghighlight(
      threshold_var < threshold,
      unhighlighted_params = list(colour = "darkgrey"), calculate_per_facet = TRUE
    ) +
    facet_wrap(~prop.insecticide) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(shape = my_label("W_st"), colour = my_label("prop.insecticide")) +
    my_theme()
  p
}

plot_type4_y_versus_treat_prop_facet_W_st <- function(df, y_var, this_K) {
  df$y <- df[, y_var]
  this_xlab <- my_label("treat_prop")
  this_ylab <- my_label(y_var)

  p <- df %>%
    mutate_at(c("prop.insecticide", "W_st", "K"), as.factor) %>%
    filter(
      K == this_K,
      W_st %in% c(0, 100, 250)
    ) %>%
    ggplot(aes(treat_prop, y, shape = K, colour = prop.insecticide)) +
    geom_point(size = my_pointsize()) +
    geom_line(size = my_linewidth()) +
    facet_wrap(~W_st) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(shape = my_label("K"), colour = my_label("prop.insecticide")) +
    my_theme()
  p
}

plot_type5_y_versus_prop_insecticide_facet_W_st <- function(df, y_var, this_K) {
  df$y <- df[, y_var]
  this_xlab <- my_label("prop.insecticide")
  this_ylab <- my_label(y_var)

  p <- df %>%
    mutate_at(c("treat_prop", "W_st", "K"), as.factor) %>%
    filter(
      prop.insecticide <= 0.5,
      treat_prop %in% c(0, 0.2, 0.4, 0.6, 0.8, 0.91),
      W_st %in% c(0, 100, 250),
      K == this_K
    ) %>%
    ggplot(aes(prop.insecticide, y, shape = K, colour = treat_prop)) +
    geom_point(size = my_pointsize()) +
    geom_line(size = my_linewidth()) +
    xlab(this_xlab) +
    ylab(this_ylab) +
    labs(colour = my_label("treat_prop")) +
    facet_wrap(~W_st) +
    my_theme()
  p
}
