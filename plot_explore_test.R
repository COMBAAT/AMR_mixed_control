# Load required packages -------------------------------------------------------

library(ggplot2)
library(gghighlight)
library(dplyr)
library(styler)

# Source files and function
source("funcs/plot_helper.R")

# Load data files --------------------------------------------------------------
# Specify whether treatment is fast "F" or prophylactic "P"
treatment_option <- "F"
if (treatment_option == "F") {
  folder_name <- "output/test/"
  load("output/test_F.Rda")
}

# Generate plots ---------------------------------------------------------------
# Plot R0 versus wildlife faceted by treat_prop
test %>%
  mutate_at(c("prop.insecticide", "treat_prop", "K"), as.factor) %>%
  filter(prop.insecticide == 0, treat_prop %in% c(0, 0.6, 0.95)) %>%
  ggplot(aes(W_st, R0_sen, colour = K)) +
  geom_point(size = my_pointsize()) +
  geom_line(size = my_linewidth()) +
  facet_wrap(~treat_prop) +
  xlab(my_label("W_st")) +
  ylab(my_label("R0_sen")) +
  my_theme()

ggsave(
  filename = "output/test/plot_type0_R0_sen.pdf",
  width = my_pdfwidth(), height = my_pdfheight()
)

# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by W_st
y_vars <- c(
  "R0_sen", "prevalence", "Incidence", "No_trt_cat", "Prob_onward_tran",
  "RiskE", "RiskA"
)

for (y_var in y_vars) {
  plot_type1_y_versus_treat_prop_facet_W_st(test, y_var)

  plot_name <- paste0("plot_type1_", y_var, ".pdf")
  ggsave(filename = paste0("output/test/", plot_name),
         width = my_pdfwidth(), height = my_pdfheight()
  )
}
# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by prop.insecticide

y_var <- "RiskA"
this_K <- 10000
plot_type2_y_versus_treat_prop_facet_prop_insecticide(test, this_K, y_var)
plot_name <- paste0("plot_type2_", y_var, ".pdf")
ggsave(filename = paste0("output/test/", plot_name),
  width = my_pdfwidth(), height = my_pdfheight()
)

y_var <- "RiskE"
this_K <- 10000
plot_type2_y_versus_treat_prop_facet_prop_insecticide(test, this_K, y_var)
plot_name <- paste0("plot_type2_", y_var, ".pdf")
ggsave(filename = paste0("output/test/", plot_name),
  width = my_pdfwidth(), height = my_pdfheight()
)

# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by prop.insecticide with highlighting
y_var <- "RiskE"
this_K <- 10000
threshold_var <- "prevalence"
threshold <- 0.1
plot_type3_y_versus_treat_prop_facet_prop_insecticide_with_higlight(
  test, this_K, y_var, threshold_var, threshold
)
plot_name <- paste0("plot_type3_", y_var, ".pdf")
ggsave(filename = paste0("output/test/", plot_name),
  width = my_pdfwidth(), height = my_pdfheight()
)

# ----------------------------------------

# ----------------------------------------
# Plot y versus_treat_prop faceted by W_st, coloured by prop.insecticide
y_vars <- c("Incidence", "prevalence", "No_trt_cat", "RiskE")
this_K <- 6000

for (y_var in y_vars) {
  plot_type4_y_versus_treat_prop_facet_W_st(test, y_var, this_K)

  plot_name <- paste0("plot_type4_", y_var, ".pdf")
  ggsave(filename = paste0("output/test/", plot_name),
    width = my_pdfwidth(), height = my_pdfheight()
  )
}
# ----------------------------------------

# ----------------------------------------
# Plot y versus prop.insecticide faceted by W_st, coloured by treat_prop
y_vars <- c("Incidence", "prevalence", "No_trt_cat", "RiskE")
this_K <- 6000

for (y_var in y_vars) {
  plot_type5_y_versus_prop_insecticide_facet_W_st(test, y_var, this_K)

  plot_name <- paste0("plot_type5_", y_var, ".pdf")
  ggsave(filename = paste0("output/test/", plot_name),
    width = my_pdfwidth(), height = my_pdfheight()
  )
}
# ----------------------------------------
