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

# Example data: 6 trajectories
df <- expand.grid(
  x = seq(0, 10, length.out = 100),
  group = factor(paste0("Line ", 1:6))
)
df$y <- with(df, as.numeric(group) + sin(x))

base_plot <- ggplot(df, aes(x, y, colour = group)) +
  geom_line(linewidth = 1.4) +
  my_theme() +
  theme(legend.position = "right")

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




(p_default | p_dark2) / (p_okabe_projector | p_okabe) / (p_okabe_brown | p_okabe_grey)
