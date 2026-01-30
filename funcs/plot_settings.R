

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

linetypes6 <- c("solid", "longdash", "dashed", "dotdash", "dotted", "twodash")
shapes6 <- c(16, 17, 15, 3, 7, 8)

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

projector6 <- c(
  "#E69F00", # orange  (very stable)
  "#D55E00", # vermillion (very stable)
  "#0072B2", # blue (stable)
  "#005B4F", # deep teal (stronger than #009E73 on projectors)
  "#4D4D4D",  # dark grey (stable)
  "#8B3A62" # dark purple/magenta (holds up better than #CC79A7)
)

scale_colour_discrete <- function(...) {
  scale_colour_manual(values = rep(projector_cols_warm_first, 100), ...)
}

# scale_colour_discrete <- function(...) {
#   scale_colour_brewer(palette = "Set1", ...)
#   #scale_colour_viridis_d(option = "E", ...)
# }

scale_color_discrete <- scale_colour_discrete


my_theme <- function() {
  theme_bw(base_size = 15) +
    theme(
      text = element_text(colour = "#111111"),
      # Make everything transparent
      plot.background  = element_rect(fill = NA, colour = NA),
      panel.background = element_rect(fill = NA, colour = NA),
      legend.background = element_rect(fill = NA, colour = NA),
      legend.key        = element_rect(fill = NA, colour = NA),
      strip.background  = element_rect(fill = NA, colour = NA),
      #panel.background = element_rect(fill = "#FAFAFA"),
      #plot.background  = element_rect(fill = "#FAFAFA", colour = NA),
      plot.title = element_text(hjust = 0.5, size = 1.0 * 15),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      # Clean look
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      #axis.line        = element_line(colour = "#111111")
    )
}

my_theme_SALT_TZ <- function() {
  theme_bw(base_size = 17) +
    theme(
      # Make everything transparent
      plot.background  = element_rect(fill = NA, colour = NA),
      panel.background = element_rect(fill = NA, colour = NA),
      legend.background = element_rect(fill = NA, colour = NA),
      legend.key        = element_rect(fill = NA, colour = NA),
      strip.background  = element_rect(fill = NA, colour = NA),
      #panel.background = element_rect(fill = "#FAFAFA"),
      #plot.background  = element_rect(fill = "#FAFAFA", colour = NA),
      plot.title = element_text(hjust = 0.5, size = rel(1.2)),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      # Clean look
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      legend.key.size = unit(1.0, "lines"),  # size of each legend entry
      #legend.spacing.y = unit(0.5, "lines")  # vertical spacing between entries
      #axis.line        = element_line(colour = "#111111")
    )
}
