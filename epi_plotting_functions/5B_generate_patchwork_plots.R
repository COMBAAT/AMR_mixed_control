# Generating plots for the manuscript
rm(list = ls()[!grepl("^(plot|df|this_NW_set|this_vector_measure|saved_simulations)", ls())])
source("funcs/plot_helper.R")
library(patchwork)
library(cowplot)
library(ggtext)

#names(plots3T)
names(plots2F)[grepl("plot_type1", names(plots2F))]


# ##############################################################################

# ##############################################################################

figX.1 <- plots1F$plot_type15_invasion_1_treatment_code1_mainvecpop_FALSE_vector_value_20_fit_adj_0.8_single_panel_with_contours_TRUE 
#figX.1 <- plots1F$plot_type15_invasion_1_ttype1_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8_single_panel 
figX.1 <- add_fancy_title(figX.1, "Collective", "", "Treatment type: Responsive curative")

figX.2 <- plots1T$plot_type15_invasion_1_treatment_code1_mainvecpop_TRUE_vector_value_20_fit_adj_0.8_single_panel_with_contours_TRUE 
#figX.2 <- plots1T$plot_type15_invasion_1_ttype1_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8_single_panel 
figX.2 <- add_fancy_title(figX.2,  "Local", "", "Treatment type: Responsive curative")

figX.3 <- plots2F$plot_type15_invasion_1_treatment_code2_mainvecpop_FALSE_vector_value_20_fit_adj_0.8_single_panel_with_contours_TRUE
#figX.3 <- plots2F$plot_type15_invasion_1_ttype2_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8_single_panel
figX.3 <- add_fancy_title(figX.3,  "", "", "Treatment type: Responsive longlasting")

figX.4 <- plots2T$plot_type15_invasion_1_treatment_code2_mainvecpop_TRUE_vector_value_20_fit_adj_0.8_single_panel_with_contours_TRUE
#figX.4 <- plots2T$plot_type15_invasion_1_ttype2_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8_single_panel
figX.4 <- add_fancy_title(figX.4,  "", "", "Treatment type: Responsive longlasting")

figX.5 <- plots3F$plot_type15_invasion_1_treatment_code3_mainvecpop_FALSE_vector_value_20_fit_adj_0.8_single_panel_with_contours_TRUE
#figX.5 <- plots3F$plot_type15_invasion_1_ttype3_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8_single_panel
figX.5 <- add_fancy_title(figX.5,  "", "", "Treatment type: Ongoing longlasting")

figX.6 <- plots3T$plot_type15_invasion_1_treatment_code3_mainvecpop_TRUE_vector_value_20_fit_adj_0.8_single_panel_with_contours_TRUE
#figX.6 <- plots3T$plot_type15_invasion_1_ttype3_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8_single_panel
figX.6 <- add_fancy_title(figX.6,  "", "", "Treatment type: Ongoing longlasting")


row1 <- (figX.1 + plot_spacer() + figX.2) + plot_layout(widths = c(1, 0.15, 1))
row2 <- (figX.3 + plot_spacer() + figX.4) + plot_layout(widths = c(1, 0.15, 1))
row3 <- (figX.5 + plot_spacer() + figX.6) + plot_layout(widths = c(1, 0.15, 1))

figB <- row1 / row2 / row3 + plot_layout(guides = 'collect')
my_ggsave(plot = figB, filename = "output/ms_figs/5pB_use_as_fig5.pdf", width = 14.5, height = 14) # best size


figX.1 <- plots1F$plot_type16_landscape_treatment_code1_mainvecpop_FALSE_vector_value_20_single_panel
#figX.1 <- plots1F$plot_type16_landscape_ttype1_spec_FALSE_FALSE_vector_value_20_single_panel 
figX.1 <- add_fancy_title(figX.1, "Collective", "", "Treatment type: Responsive curative")

figX.2 <- plots1T$plot_type16_landscape_treatment_code1_mainvecpop_TRUE_vector_value_20_single_panel
#figX.2 <- plots1T$plot_type16_landscape_ttype1_spec_FALSE_TRUE_vector_value_20_single_panel
figX.2 <- add_fancy_title(figX.2,  "Local", "", "Treatment type: Responsive curative")

figX.3 <- plots2F$plot_type16_landscape_treatment_code2_mainvecpop_FALSE_vector_value_20_single_panel
#figX.3 <- plots2F$plot_type16_landscape_ttype2_spec_FALSE_FALSE_vector_value_20_single_panel
figX.3 <- add_fancy_title(figX.3,  "", "", "Treatment type: Responsive longlasting")

figX.4 <- plots2T$plot_type16_landscape_treatment_code2_mainvecpop_TRUE_vector_value_20_single_panel
#figX.4 <- plots2T$plot_type16_landscape_ttype2_spec_FALSE_TRUE_vector_value_20_single_panel
figX.4 <- add_fancy_title(figX.4,  "", "", "Treatment type: Responsive longlasting")

figX.5 <- plots3F$plot_type16_landscape_treatment_code3_mainvecpop_FALSE_vector_value_20_single_panel
#figX.5 <- plots3F$plot_type16_landscape_ttype3_spec_FALSE_FALSE_vector_value_20_single_panel
figX.5 <- add_fancy_title(figX.5,  "", "", "Treatment type: Ongoing longlasting")

figX.6 <- plots3T$plot_type16_landscape_treatment_code3_mainvecpop_TRUE_vector_value_20_single_panel
#figX.6 <- plots3T$plot_type16_landscape_ttype3_spec_FALSE_TRUE_vector_value_20_single_panel
figX.6 <- add_fancy_title(figX.6,  "", "", "Treatment type: Ongoing longlasting")

row1 <- (figX.1 + plot_spacer() + figX.2) + plot_layout(widths = c(1, 0.15, 1))
row2 <- (figX.3 + plot_spacer() + figX.4) + plot_layout(widths = c(1, 0.15, 1))
row3 <- (figX.5 + plot_spacer() + figX.6) + plot_layout(widths = c(1, 0.15, 1))

figB <- row1 / row2 / row3 + plot_layout(guides = 'collect')
my_ggsave(plot = figB, filename = "output/ms_figs/5pB_use_as_fig6.pdf", width = 14, height = 14) # best size



