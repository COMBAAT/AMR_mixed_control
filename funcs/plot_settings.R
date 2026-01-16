
my_ggsave <- function(plot, filename, width, height) {
  ggsave(
    plot = plot,
    filename = filename,
    width = width,
    height = height,
    device = cairo_pdf,
    limitsize = TRUE
  )
}


projector_cols <- c(
  "#4D4D4D",  # dark grey (very stable)
  "#E69F00",  # orange (projects well)
  "#0072B2",  # strong blue (survives washout)
  "#009E73",  # bluish green (distinct from blue)
  "#D55E00",  # vermillion (high contrast)
  "#CC79A7"   # purple (still separable when faded)
)

projector_cols_warm_first <- c(
  "#E69F00",  # orange
  "#D55E00",  # vermillion
  "#0072B2",  # strong blue
  "#009E73",  # bluish green
  
  "#4D4D4D",  # dark grey (last)
  "#CC79A7"  # purple
)

scale_colour_discrete <- function(...) {
  scale_colour_manual(values = rep(projector_cols_warm_first, 100), ...)
}

# scale_colour_discrete <- function(...) {
#   scale_colour_brewer(palette = "Set1", ...)
#   #scale_colour_viridis_d(option = "E", ...)
# }

scale_color_discrete <- scale_colour_discrete







