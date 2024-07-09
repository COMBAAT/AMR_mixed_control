# Load required packages -------------------------------------------------------

library(ggplot2)
library(gghighlight)
library(dplyr)
library(patchwork)

# Source files and function
source("funcs/plot_helper.R")
source("funcs/helper_functions.R")

# Load data files --------------------------------------------------------------
latest_file <- get_latest_Rda_file()
load(latest_file)
folder_name <- gsub(".Rda", "/", latest_file)
dir.create(folder_name)

# Generate plots ---------------------------------------------------------------
# Plot R0 versus wildlife faceted by treat_prop
test %>%
  mutate_at(c("prop_insecticide", "treat_prop", "K"), as.factor) %>%
  filter(prop_insecticide == 0, treat_prop %in% c(0, 0.6, 0.95)) %>%
  ggplot(aes(NW, R0sen, colour = K)) +
  geom_point(size = my_pointsize()) +
  geom_line(linewidth = my_linewidth()) +
  facet_wrap(~treat_prop) +
  xlab(my_label("NW")) +
  ylab(my_label("R0sen")) +
  labs(colour = my_label("K")) +
  my_theme()

ggsave(
  filename = paste0(folder_name, "plot_type0_R0sen.pdf"),
  width = my_pdfwidth(), height = my_pdfheight()
)

# ----------------------------------------
# Plot R resistant/R sensitive versus wildlife faceted by treat_prop
lhs <- test %>%
  mutate_at(c("prop_insecticide", "NW", "K"), as.factor) %>%
  filter(prop_insecticide == 0) %>%
  ggplot(aes(treat_prop, Rres_final / Rsen_final, colour = NW, shape = K)) +
  geom_point(size = my_pointsize()) +
  geom_line(linewidth = my_linewidth()) +
  xlab(my_label("treat_prop")) +
  ylab("R resistant / R sensitive") +
  labs(colour = my_label("NW"), shape = my_label("K")) +
  my_theme()

rhs <- lhs + ylim(c(0, 2)) + geom_abline(intercept = 1, slope = 0, linetype = "dashed")
rhs

# use patchwork package to stick plots together
# use guides = collect to remove duplicate legends
lhs + rhs + plot_layout(ncol = 2, guides = "collect")

ggsave(
  filename = paste0(folder_name, "plot_type0_R_res_R_sen_ratio.pdf"),
  width = my_pdfwidth(), height = my_pdfheight()
)

# ----------------------------------------
# Plot y versus_treat_prop faceted by NW
y_vars <- c(
  "R0sen", "prevalence", "Incidence", "No_trt_cat", "Prob_onward_tran",
  "RiskE", "RiskA"
)

for (y_var in y_vars) {
  plot_type1_y_versus_treat_prop_facet_NW(test, y_var)

  plot_name <- paste0("plot_type1_", y_var, ".pdf")
  ggsave(
    filename = paste0(folder_name, plot_name),
    width = my_pdfwidth(), height = my_pdfheight()
  )
}
# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by prop_insecticide

y_var <- "RiskA"
this_K <- 10000
plot_type2_y_versus_treat_prop_facet_prop_insecticide(test, this_K, y_var)
plot_name <- paste0("plot_type2_", y_var, ".pdf")
ggsave(
  filename = paste0(folder_name, plot_name),
  width = my_pdfwidth(), height = my_pdfheight()
)

y_var <- "RiskE"
this_K <- 10000
plot_type2_y_versus_treat_prop_facet_prop_insecticide(test, this_K, y_var)
plot_name <- paste0("plot_type2_", y_var, ".pdf")
ggsave(
  filename = paste0(folder_name, plot_name),
  width = my_pdfwidth(), height = my_pdfheight()
)

# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by prop_insecticide with highlighting
y_var <- "RiskE"
this_K <- 10000
threshold_var <- "prevalence"
threshold <- 0.1
plot_type3_y_versus_treat_prop_facet_prop_insecticide_with_higlight(
  test, this_K, y_var, threshold_var, threshold
)
plot_name <- paste0("plot_type3_", y_var, ".pdf")
ggsave(
  filename = paste0(folder_name, plot_name),
  width = my_pdfwidth(), height = my_pdfheight()
)

# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by NW, coloured by prop_insecticide
y_vars <- c("Incidence", "prevalence", "No_trt_cat", "RiskE")
this_K <- 6000

for (y_var in y_vars) {
  plot_type4_y_versus_treat_prop_facet_NW(test, y_var, this_K)

  plot_name <- paste0("plot_type4_", y_var, ".pdf")
  ggsave(
    filename = paste0(folder_name, plot_name),
    width = my_pdfwidth(), height = my_pdfheight()
  )
}
# ----------------------------------------

# ----------------------------------------
# Plot y versus prop_insecticide faceted by NW, coloured by treat_prop
y_vars <- c("Incidence", "prevalence", "No_trt_cat", "RiskE")
this_K <- 6000

for (y_var in y_vars) {
  plot_type5_y_versus_prop_insecticide_facet_NW(test, y_var, this_K)

  plot_name <- paste0("plot_type5_", y_var, ".pdf")
  ggsave(
    filename = paste0(folder_name, plot_name),
    width = my_pdfwidth(), height = my_pdfheight()
  )
}
# ----------------------------------------
