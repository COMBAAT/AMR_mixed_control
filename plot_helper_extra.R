plot_type0_ratio <- function(df, this_vector_measure, ttype) {
  
  x_var <- get_x_var(df, ttype) 
  df$x <- df[, x_var]
  this_xlab <- my_label(x_var)

df$shape_variable <- df[, this_vector_measure]
lhs <- df %>%
  mutate_at(c("prop_cattle_with_insecticide", "NW", this_vector_measure, "shape_variable"), as.factor) %>%
  filter(prop_cattle_with_insecticide == 0.0) %>%
  ggplot(aes(treat_prop, ratio, colour = NW, shape = shape_variable)) +
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
