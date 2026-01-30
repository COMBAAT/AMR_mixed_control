library(ggplot2)
library(patchwork)
library(viridis)
library(RColorBrewer)

# reset the defaults
rm(scale_colour_discrete, scale_color_discrete)

# plot background
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
      panel.grid.minor = element_blank(),
      axis.line        = element_line(colour = "black")
    )
}


# Define Okabe–Ito (6 colours)
okabe_ito_cols <- c(
  "#000000",  # black
  "#E69F00",  # orange
  "#56B4E9",  # sky blue
  "#009E73",  # bluish green
  "#0072B2",  # blue
  "#CC79A7"   # purple
)

okabe_ito_brown <- c(
  "#8C510A",  # dark brown
  "#E69F00",  # orange
  "#56B4E9",  # sky blue
  "#009E73",  # bluish green
  "#0072B2",  # blue
  "#CC79A7"   # purple
)

okabe_ito_grey <- c(
  "#4D4D4D",  # dark grey (instead of black)
  "#E69F00",  # orange
  "#56B4E9",  # sky blue
  "#009E73",  # bluish green
  "#0072B2",  # blue
  "#CC79A7"   # purple
)

# Projector-optimised 6-line palette (high luminance separation)
# This is the safest palette I know for bad rooms:
#Why this works:
#No pastel colours
#No light yellows
#Strong differences in brightness
#All survive saturation loss

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

# Example data: 6 trajectories
df <- expand.grid(
  x = seq(0, 10, length.out = 100),
  group = factor(paste0("Line ", 1:6))
)
df$y <- with(df, as.numeric(group) + sin(x))

base_plot <- ggplot(df, aes(x, y, colour = group)) +
  geom_line(linewidth = 4) +
  my_theme() +
  theme(legend.position = "none")

p_default <- base_plot +
  scale_colour_discrete() +
  ggtitle("ggplot default")

p_dark2 <- base_plot +
  scale_colour_brewer(palette = "Dark2") +
  ggtitle("Dark2")

p_viridis <- base_plot +
  scale_colour_viridis_d() +
  ggtitle("Viridis (discrete)")

p_okabe <- base_plot +
  scale_colour_manual(values = okabe_ito_cols) +
  ggtitle("Okabe–Ito")

p_okabe_brown <- base_plot +
  scale_colour_manual(values = okabe_ito_brown) +
  ggtitle("Okabe–Ito brown")

p_okabe_grey <- base_plot +
  scale_colour_manual(values = okabe_ito_grey) +
  ggtitle("Okabe–Ito grey")

p_okabe_projector <- base_plot +
  scale_colour_manual(values = projector_cols) +
  ggtitle("Projector optimised")

p_okabe_projector_warm_first <- base_plot +
  scale_colour_manual(values = projector_cols_warm_first) +
  ggtitle("Projector warm first")

p_okabe_projector6 <- base_plot +
  scale_colour_manual(values = projector6) +
  ggtitle("Projector 6")


p <- (p_default | p_okabe_projector6) / (p_okabe_projector_warm_first | p_okabe) / (p_okabe_brown | p_okabe_grey)
p
ggsave(plot = p, file = "test_palettes.pdf", height = 10, width = 6)
